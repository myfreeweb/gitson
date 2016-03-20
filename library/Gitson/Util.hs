{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, Safe #-}

-- | Various functions used inside Gitson.
module Gitson.Util where

import           Prelude.Compat
import           Control.Monad (void, filterM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Exception.Lifted (bracket)
import           Data.List (isSuffixOf, isPrefixOf)
import           Data.Maybe
import           System.FilePath
import           System.Directory
import           System.Process
import           System.IO

-- | Combines two paths and adds the .json extension.
--
-- >>> documentPath "things" "document"
-- "things/document.json"
--
-- >>> documentPath "things/" "document"
-- "things/document.json"
documentPath :: FilePath -> String -> FilePath
documentPath collection key = collection </> key <.> "json"

-- | Path to the transaction lock file, relative to the repo root.
lockPath :: FilePath
lockPath = ".git" </> "gitson-lock"

-- | Turns a list of filenames into a list of keys, ignoring non-JSON files.
--
-- >>> filterFilenamesAsKeys [".", "..", "k1.json", "unrelated.file"]
-- ["k1"]
filterFilenamesAsKeys :: [FilePath] -> [String]
filterFilenamesAsKeys = map dropExtension . filter (isSuffixOf ".json")

-- | Filters a list of file paths, leaving only paths to existing non-hidden directories.
filterDirs :: [FilePath] -> IO [FilePath]
filterDirs = filterM doesDirectoryExist . filter (not . isPrefixOf ".")

-- | Returns an IO action that switches the current directory to a given path,
-- executes the given IO action and switches the current directory back.
insideDirectory :: (MonadIO i, MonadBaseControl IO i) => FilePath -> i a -> i a
insideDirectory path action = do
  prevPath <- liftIO getCurrentDirectory
  liftBaseOp (bracket (setCurrentDirectory path) (\_ -> setCurrentDirectory prevPath)) (\_ -> action)

-- | Returns the message of the last git commit in the repo where the current directory is located.
lastCommitText :: IO String
lastCommitText = readProcess "git" ["log", "--max-count=1", "--pretty=format:%s"] []

-- | Runs a shell command with stdin, stdout and stderr set to /dev/null.
shell :: (MonadIO i) => String -> [String] -> i ()
shell cmd args = liftIO $ void $ do
  dnull <- openFile "/dev/null" ReadWriteMode
  (_, _, _, pid) <- createProcess (proc cmd args){std_in = UseHandle dnull, std_out = UseHandle dnull, std_err = UseHandle dnull}
  waitForProcess pid

-- | Appends a value to a functor, making the inside a tuple if it's a single value.
--
-- >>> intoFunctor (Just 1) 2
-- Just (1,2)
--
-- >>> intoFunctor Nothing 2
-- Nothing
intoFunctor :: Functor f => f a -> b -> f (a, b)
intoFunctor f x = fmap (flip (,) x) f

-- | Tries to extract the first int out of a string.
--
-- >>> maybeReadIntString "0123-hell0w0rld"
-- Just (123,"-hell0w0rld")
--
-- >>> maybeReadIntString "1"
-- Just (1,"")
--
-- >>> maybeReadIntString "hello"
-- Nothing
maybeReadIntString :: String -> Maybe (Int, String)
maybeReadIntString x = listToMaybe (reads x :: [(Int, String)])

-- | Returns the next numeric id in a sequence of keys.
--
-- >>> nextKeyId []
-- 1
--
-- >>> nextKeyId ["aaaaarrrrrrrrrrr"]
-- 1
--
-- >>> nextKeyId ["1-hell0-w0rld-123456.json", "002-my-second-post.json"]
-- 3
nextKeyId :: [String] -> Int
nextKeyId = (+1) . maxOrZero . mapMaybe maybeReadInt
  where maybeReadInt x = fst <$> maybeReadIntString x
        maxOrZero [] = 0
        maxOrZero xs = maximum xs
