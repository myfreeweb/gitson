{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module GitsonBench (setup, benchmarks, cleanup) where

import           Control.Monad (void)
import           System.Directory
import           Data.Aeson
import           Criterion
import           Gitson

exampleVal ∷ Value
exampleVal = object [ "with" .= [ "some", object [ "stuff" .= String "inside" ], Number 1, Number 2, Number 3, Number 4, Number 5, Bool True ]
                    , "more" .= [ "some", object [ "stuff" .= String "inside" ], object [ "stuff" .= String "inside" ], Bool True ]
                    , "Text" .= String "This is some text. This is some text. This is some text. This is some text. This is some text. This is some text. "
                    , "aaaa" .= [ "some", object [ "stuff" .= String "inside" ], Number 1, Number 2, Number 3, Number 4, Number 5, Bool True ]
                    , "bbbb" .= [ "some", object [ "stuff" .= String "inside" ], object [ "stuff" .= String "inside" ], Bool True ]
                    , "xxxx" .= object [
                        "more" .= [ "some", object [ "stuff" .= String "inside" ], object [ "stuff" .= String "inside" ], Bool True ]
                      , "text" .= String "This is some text. This is some text. This is some text. This is some text. This is some text. This is some text. "
                      , "aaaa" .= [ "some", object [ "stuff" .= String "inside" ], Number 1, Number 2, Number 3, Number 4, Number 5, Bool True ]
                      , "yyyy" .= String "This is some text. This is some text. This is some text. This is some text. This is some text. This is some text. "
                      , "bbbb" .= [ "some", object [ "stuff" .= String "inside" ], object [ "stuff" .= String "inside" ], Bool True ] ]
                    , "text" .= String "This is some text. This is some text. This is some text. This is some text. This is some text. This is some text. "
                    , "texT" .= String "This is some text. This is some text. This is some text. This is some text. This is some text. This is some text. " ]

benchRepoPath ∷ String
benchRepoPath = "tmp/bench-repo"

setup ∷ IO ()
setup = do
  createRepo benchRepoPath
  void $ transaction benchRepoPath $ do
    saveDocument "things" "thing-to-read" exampleVal
    saveNextDocument "things" "thing-to-read" exampleVal

transactOneWrite ∷ IO ()
transactOneWrite = transaction benchRepoPath $ do
  saveDocument "things" "first-thing" exampleVal

transactNextWrites ∷ IO ()
transactNextWrites = transaction benchRepoPath $ do
  saveNextDocument "things" "thing" exampleVal
  saveNextDocument "things" "thing" exampleVal
  saveNextDocument "things" "thing" exampleVal
  saveNextDocument "things" "thing" exampleVal

readThing ∷ IO ()
readThing = void (readDocument "things" "thing-to-read" ∷ IO (Maybe Value))

readByName ∷ IO ()
readByName = void (readDocumentByName "things" "thing-to-read" ∷ IO (Maybe Value))

benchmarks ∷ [Benchmark]
benchmarks = [
    bench "transaction with 1 write" $ nfIO transactOneWrite
  , bench "transaction with 4 writes with incrementing ids" $ nfIO transactNextWrites
  , bench "1 read" $ nfIO $ readThing
  , bench "1 read by name" $ nfIO $ readByName ]

cleanup ∷ IO ()
cleanup = removeDirectoryRecursive benchRepoPath
