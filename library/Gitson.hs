{-# LANGUAGE NoImplicitPrelude, CPP, FlexibleContexts, UnicodeSyntax #-}

-- | Gitson is a simple document store library for Git + JSON.
module Gitson (
  TransactionWriter
, HasGitsonLock
, getGitsonLock
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
, documentFullKey
, findById
, findByName
) where

import           Prelude.Compat
import           System.Directory
import           System.Lock.FLock
import           System.IO.Unsafe
import           Control.Exception (try, IOException)
import           Control.Error.Util (hush)
import           Control.Monad.Trans.Writer
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar.Lifted
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.List (find, isSuffixOf)
import qualified Data.ByteString.Lazy as BL
import           Data.Aeson (ToJSON, FromJSON, fromJSON, json, Result(..), Value)
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy (ByteString)
import           Data.Conduit.Attoparsec (sinkParserEither, ParseError)
import           Conduit (sourceFile, ($$), runResourceT)
import           Text.Printf (printf)
import           Gitson.Util

encode ∷ ToJSON a ⇒ a → ByteString
encode = encodePretty' $ Config { confIndent = 2, confCompare = compare }

-- | A transaction monad.
type TransactionWriter = WriterT [IO ()]

type IdAndName = (Int, String)
type FileName = String
type Finder = [(IdAndName, FileName)] → Maybe (IdAndName, FileName)

splitFindDocument ∷ (MonadIO i, Functor i) ⇒ FilePath → Finder → i (Maybe (IdAndName, FileName))
splitFindDocument collection finder = 
  finder . mapMaybe (\x → intoFunctor (maybeReadIntString x) x) <$> listDocumentKeys collection

documentFullKey ∷ (MonadIO i, Functor i) ⇒ FilePath → Finder → i (Maybe FileName)
documentFullKey collection finder = (snd <$>) <$> splitFindDocument collection finder

findById ∷ Int → Finder
findById i = find $ (== i) . fst . fst

findByName ∷ String → Finder
findByName n = find $ isSuffixOf n . snd . fst

-- | Creates a git repository under a given path.
createRepo ∷ FilePath → IO ()
createRepo path = do
  createDirectoryIfMissing True path
  insideDirectory path $ shell "git" ["init"]

class HasGitsonLock m where
  getGitsonLock ∷ m (MVar ())

globalGitsonLock ∷ MVar ()
globalGitsonLock = unsafePerformIO $ newMVar ()

instance HasGitsonLock IO where
  getGitsonLock = return globalGitsonLock

-- | Executes a blocking transaction on a repository, committing the results to git.
transaction ∷ (MonadIO i, Functor i, MonadBaseControl IO i, HasGitsonLock i) ⇒ FilePath → TransactionWriter i () → i ()
transaction repoPath action = do
  mlock ← getGitsonLock
  withMVar mlock $ const $ insideDirectory repoPath $ do
    liftIO $ writeFile lockPath ""
    withLock lockPath Exclusive Block $ do
      writeActions ← execWriterT action
      shell "git" ["stash"] -- it's totally ok to do this without changes
      liftIO $ sequence_ writeActions
      shell "git" ["add", "--all"]
      shell "git" ["commit", "-m", "Gitson transaction"]

combineKey ∷ IdAndName → FileName
combineKey (n, s) = printf "%06d-%s" n s

writeDocument ∷ ToJSON a ⇒ FilePath → FileName → a → IO ()
writeDocument collection key content = BL.writeFile (documentPath collection key) (encode content)

-- | Adds a write action to a transaction.
saveDocument ∷ (MonadIO i, Functor i, ToJSON a) ⇒ FilePath → FileName → a → TransactionWriter i ()
saveDocument collection key content =
  tell [createDirectoryIfMissing True collection,
        writeDocument collection key content]

-- | Adds a write action to a transaction.
-- The key will start with a numeric id, incremented from the last id in the collection.
saveNextDocument ∷ (MonadIO i, Functor i, ToJSON a) ⇒ FilePath → FileName → a → TransactionWriter i ()
saveNextDocument collection key content =
  tell [createDirectoryIfMissing True collection,
        listDocumentKeys collection >>=
        return . nextKeyId >>=
        \nextId → writeDocument collection (combineKey (nextId, key)) content]

-- | Adds a write action to a transaction.
-- Will update the document with the given numeric id.
saveDocumentById ∷ (MonadIO i, Functor i, ToJSON a) ⇒ FilePath → Int → a → TransactionWriter i ()
saveDocumentById collection i content =
  tell [documentFullKey collection (findById i) >>=
        \k → case k of
          Just key → writeDocument collection key content
          Nothing → return ()]

-- | Adds a write action to a transaction.
-- Will update the document with the given numeric id.
saveDocumentByName ∷ (MonadIO i, Functor i, ToJSON a) ⇒ FilePath → String → a → TransactionWriter i ()
saveDocumentByName collection n content =
  tell [documentFullKey collection (findByName n) >>=
        \k → case k of
          Just key → writeDocument collection key content
          Nothing → return ()]

-- | Lists collections in the current repository.
listCollections ∷ (MonadIO i, Functor i) ⇒ i [FilePath]
listCollections = liftIO $ do
  contents ← try (getDirectoryContents =<< getCurrentDirectory) ∷ IO (Either IOException [FilePath])
  filterDirs $ fromMaybe [] $ hush contents

-- | Lists document keys in a collection.
listDocumentKeys ∷ (MonadIO i, Functor i) ⇒ FilePath → i [FileName]
listDocumentKeys collection = liftIO $ do
  contents ← try (getDirectoryContents collection) ∷ IO (Either IOException [String])
  return . filterFilenamesAsKeys . fromMaybe [] $ hush contents

-- | Lists entries in a collection.
listEntries ∷ (MonadIO i, Functor i, FromJSON a) ⇒ FilePath → i [a]
listEntries collection = do
  maybes ← mapM (readDocument collection) =<< listDocumentKeys collection
  return . fromMaybe [] $ sequence maybes

-- | Reads a document from a collection by key.
readDocument ∷ (MonadIO i, Functor i, FromJSON a) ⇒ FilePath → FileName → i (Maybe a)
readDocument collection key = do
  j ← liftIO ((try (runResourceT $ sourceFile (documentPath collection key) $$ sinkParserEither json)) ∷ IO (Either IOException (Either ParseError Value)))
  return $ case fromJSON <$> (hush =<< hush j) of
             Just (Success a) → Just a
             _ → Nothing

readDocument' ∷ (MonadIO i, Functor i, FromJSON a) ⇒ FilePath → Maybe FileName → i (Maybe a)
readDocument' collection key = case key of
  Just key' → readDocument collection key'
  Nothing → return Nothing

-- | Reads a document from a collection by numeric id (for example, key "00001-hello" has id 1).
readDocumentById ∷ (MonadIO i, Functor i, FromJSON a) ⇒ FilePath → Int → i (Maybe a)
readDocumentById collection i =
  readDocument' collection =<< documentFullKey collection (findById i)

-- | Reads a document from a collection by name (for example, key "00001-hello" has name "hello").
readDocumentByName ∷ (MonadIO i, Functor i, FromJSON a) ⇒ FilePath → String → i (Maybe a)
readDocumentByName collection n =
  readDocument' collection =<< documentFullKey collection (findByName n)

-- | Returns a document's id by name (for example, "hello" will return 23 when key "00023-hello" exists).
-- Does not read the document!
documentIdFromName ∷ (MonadIO i, Functor i) ⇒ FilePath → String → i (Maybe Int)
documentIdFromName collection n =
  (fst <$> fst <$>) <$> splitFindDocument collection (findByName n)

-- | Returns a document's name by id (for example, 23 will return "hello" when key "00023-hello" exists).
-- Does not read the document!
documentNameFromId ∷ (MonadIO i, Functor i) ⇒ FilePath → Int → i (Maybe String)
documentNameFromId collection i =
  (drop 1 . snd <$> fst <$>) <$> splitFindDocument collection (findById i)
