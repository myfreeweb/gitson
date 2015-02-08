{-# LANGUAGE Safe, FlexibleContexts #-}

-- | Gitson is a simple document store library for Git + JSON.
module Gitson (
  TransactionWriter
, createRepo
, transaction
, saveDocument
, saveNextDocument
, saveDocumentById
, saveDocumentByName
, listCollections
, listDocumentKeys
, listEntries
, readDocument
, readDocumentById
, readDocumentByName
, documentIdFromName
, documentNameFromId
) where

import           System.Directory
import           System.Lock.FLock
import           Control.Applicative ((<$>))
import           Control.Exception (try, IOException)
import           Control.Error.Util (hush)
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.List (find, isSuffixOf)
import           Text.Printf (printf)
import qualified Data.ByteString.Lazy as BL
import           Gitson.Util
import           Gitson.Json

-- | A transaction monad.
type TransactionWriter = WriterT [IO ()]

type IdAndName = (Int, String)
type FileName = String
type Finder = ([(IdAndName, FileName)] -> Maybe (IdAndName, FileName))

splitFindDocument :: (MonadIO i) => FilePath -> Finder -> i (Maybe (IdAndName, FileName))
splitFindDocument collection finder = listDocumentKeys collection >>=
  return . finder . catMaybes . map (\x -> intoFunctor (maybeReadIntString x) x)

documentFullKey :: (MonadIO i) => FilePath -> Finder -> i (Maybe FileName)
documentFullKey collection finder = splitFindDocument collection finder >>= return . (snd <$>)

findById :: Int -> Finder
findById i = find $ (== i) . fst . fst

findByName :: String -> Finder
findByName n = find $ (isSuffixOf n) . snd . fst

-- | Creates a git repository under a given path.
createRepo :: FilePath -> IO ()
createRepo path = do
  createDirectoryIfMissing True path
  insideDirectory path $ shell "git" ["init"]

-- | Executes a blocking transaction on a repository, committing the results to git.
transaction :: (MonadIO i, MonadBaseControl IO i) => FilePath -> TransactionWriter i () -> i ()
transaction repoPath action =
  insideDirectory repoPath $ do
    liftIO $ writeFile lockPath ""
    withLock lockPath Exclusive Block $ do
      writeActions <- execWriterT action
      shell "git" ["stash"] -- it's totally ok to do this without changes
      liftIO $ sequence_ writeActions
      shell "git" ["add", "--all"]
      shell "git" ["commit", "-m", "Gitson transaction"]
      shell "git" ["stash", "pop"]

combineKey :: IdAndName -> FileName
combineKey (n, s) = zeroPad n ++ "-" ++ s
  where zeroPad :: Int -> String
        zeroPad x = printf "%06d" x

writeDocument :: ToJSON a => FilePath -> FileName -> a -> IO ()
writeDocument collection key content = BL.writeFile (documentPath collection key) (encode content)

-- | Adds a write action to a transaction.
saveDocument :: (MonadIO i, ToJSON a) => FilePath -> FileName -> a -> TransactionWriter i ()
saveDocument collection key content = do
  tell [createDirectoryIfMissing True collection,
        writeDocument collection key content]

-- | Adds a write action to a transaction.
-- The key will start with a numeric id, incremented from the last id in the collection.
saveNextDocument :: (MonadIO i, ToJSON a) => FilePath -> FileName -> a -> TransactionWriter i ()
saveNextDocument collection key content = do
  tell [createDirectoryIfMissing True collection,
        listDocumentKeys collection >>=
        return . nextKeyId >>=
        \nextId -> writeDocument collection (combineKey (nextId, key)) content]

-- | Adds a write action to a transaction.
-- Will update the document with the given numeric id.
saveDocumentById :: (MonadIO i, ToJSON a) => FilePath -> Int -> a -> TransactionWriter i ()
saveDocumentById collection i content = do
  tell [documentFullKey collection (findById i) >>=
        \k -> case k of
          Just key -> writeDocument collection key content
          Nothing -> return ()]

-- | Adds a write action to a transaction.
-- Will update the document with the given numeric id.
saveDocumentByName :: (MonadIO i, ToJSON a) => FilePath -> String -> a -> TransactionWriter i ()
saveDocumentByName collection n content = do
  tell [documentFullKey collection (findByName n) >>=
        \k -> case k of
          Just key -> writeDocument collection key content
          Nothing -> return ()]

-- | Lists collections in the current repository.
listCollections :: (MonadIO i) => i [FilePath]
listCollections = liftIO $ do
  contents <- try (getDirectoryContents =<< getCurrentDirectory) :: IO (Either IOException [FilePath])
  filterDirs $ fromMaybe [] $ hush contents

-- | Lists document keys in a collection.
listDocumentKeys :: (MonadIO i) => FilePath -> i [FileName]
listDocumentKeys collection = liftIO $ do
  contents <- try (getDirectoryContents collection) :: IO (Either IOException [String])
  return $ filterFilenamesAsKeys $ fromMaybe [] $ hush contents

-- | Lists entries in a collection.
listEntries :: (MonadIO i, FromJSON a) => FilePath -> i [a]
listEntries collection = liftIO $ do
  ks <- listDocumentKeys collection
  maybes <- mapM (readDocument collection) ks
  return $ fromMaybe [] $ sequence maybes

-- | Reads a document from a collection by key.
readDocument :: (MonadIO i, FromJSON a) => FilePath -> FileName -> i (Maybe a)
readDocument collection key = liftIO $ do
  jsonString <- try (BL.readFile $ documentPath collection key) :: IO (Either IOException BL.ByteString)
  return $ decode =<< hush jsonString

readDocument' :: (MonadIO i, FromJSON a) => FilePath -> Maybe FileName -> i (Maybe a)
readDocument' collection key = liftIO $ case key of
  Just key -> readDocument collection key
  Nothing -> return Nothing

-- | Reads a document from a collection by numeric id (for example, key "00001-hello" has id 1).
readDocumentById :: (MonadIO i, FromJSON a) => FilePath -> Int -> i (Maybe a)
readDocumentById collection i =
  documentFullKey collection (findById i) >>=
  readDocument' collection

-- | Reads a document from a collection by name (for example, key "00001-hello" has name "hello").
readDocumentByName :: (MonadIO i, FromJSON a) => FilePath -> String -> i (Maybe a)
readDocumentByName collection n =
  documentFullKey collection (findByName n) >>=
  readDocument' collection

-- | Returns a document's id by name (for example, "hello" will return 23 when key "00023-hello" exists).
-- Does not read the document!
documentIdFromName :: (MonadIO i) => FilePath -> String -> i (Maybe Int)
documentIdFromName collection n =
  splitFindDocument collection (findByName n) >>=
  return . (fst <$> fst <$>)

-- | Returns a document's name by id (for example, 23 will return "hello" when key "00023-hello" exists).
-- Does not read the document!
documentNameFromId :: (MonadIO i) => FilePath -> Int -> i (Maybe String)
documentNameFromId collection i =
  splitFindDocument collection (findById i) >>=
  return . (drop 1 . snd <$> fst <$>)
