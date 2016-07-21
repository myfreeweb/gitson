{-# LANGUAGE TemplateHaskell, UnicodeSyntax #-}

module GitsonSpec (spec) where

import           Test.Hspec
import           System.Directory
import           Data.Aeson.TH
import           Data.List (sort)
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad (void)
import           Gitson
import           Gitson.Util (insideDirectory, lastCommitText)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

data Thing = Thing { val ∷ Int } deriving (Eq, Show, Ord)
$(deriveJSON defaultOptions ''Thing)

spec ∷ Spec
spec = before setup $ after cleanup $ do
  context "transaction" $ do
    describe "saveDocument" $ do
      it "saves entries only after it's done" $ do
        transaction "tmp/repo" $ do
          saveDocument "things" "first-thing" Thing {val = 1}
          saveDocument "things" "second-thing" Thing {val = 2}
          liftIO $ readFile "things/first-thing.json" `shouldThrow` anyIOException
          liftIO $ readFile "things/second-thing.json" `shouldThrow` anyIOException
        insideDirectory "tmp/repo" $ do
          first ← readFile "things/first-thing.json"
          first `shouldBe` "{\n  \"val\": 1.0\n}"
          second ← readFile "things/second-thing.json"
          second `shouldBe` "{\n  \"val\": 2.0\n}"
          commitMsg ← lastCommitText
          commitMsg `shouldBe` "Gitson transaction"

    describe "saveNextDocument" $ do
      it "saves entries with next numeric ids" $ do
        transaction "tmp/repo" $ do
          saveNextDocument "things" "hello" Thing {val = 1}
          saveNextDocument "things" "world" Thing {val = 2}
        insideDirectory "tmp/repo" $ do
          first ← readFile "things/000001-hello.json"
          first `shouldBe` "{\n  \"val\": 1.0\n}"
          second ← readFile "things/000002-world.json"
          second `shouldBe` "{\n  \"val\": 2.0\n}"

    describe "saveDocumentById" $ do
      it "updates an entry by specified id" $ do
        createDirectoryIfMissing True "tmp/repo/things"
        _ ← liftIO $ writeFile "tmp/repo/things/000004-second-thing.json" "{\"val\":1}"
        transaction "tmp/repo" $ do
          saveDocumentById "things" 4 Thing {val = 2}
        insideDirectory "tmp/repo" $ do
          second ← readFile "things/000004-second-thing.json"
          second `shouldBe` "{\n  \"val\": 2.0\n}"

    describe "saveDocumentByName" $ do
      it "updates an entry by specified name" $ do
        createDirectoryIfMissing True "tmp/repo/things"
        _ ← liftIO $ writeFile "tmp/repo/things/000004-second-thing.json" "{\"val\":1}"
        transaction "tmp/repo" $ do
          saveDocumentByName "things" "second-thing" Thing {val = 2}
        insideDirectory "tmp/repo" $ do
          second ← readFile "things/000004-second-thing.json"
          second `shouldBe` "{\n  \"val\": 2.0\n}"

  describe "readDocument" $ do
    it "returns Just the document when reading an document by key" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ ← writeFile "tmp/repo/things/second-thing.json" "{\"val\":2}"
      content ← readDocument "tmp/repo/things" "second-thing" ∷ IO (Maybe Thing)
      content `shouldBe` Just Thing {val = 2}

    it "returns Nothing when reading by a nonexistent key" $ do
      content ← readDocument "tmp/repo/things" "totally-not-a-thing" ∷ IO (Maybe Thing)
      content `shouldBe` Nothing

  describe "readDocumentById" $ do
    it "returns Just the document when reading an document by id" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ ← writeFile "tmp/repo/things/000004-second-thing.json" "{\"val\":1}"
      content ← readDocumentById "tmp/repo/things" 4 ∷ IO (Maybe Thing)
      content `shouldBe` Just Thing {val = 1}

    it "returns Nothing when reading by a nonexistent id" $ do
      content ← readDocumentById "tmp/repo/things" 1 ∷ IO (Maybe Thing)
      content `shouldBe` Nothing

  describe "readDocumentByName" $ do
    it "returns Just the document when reading an document by name" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ ← writeFile "tmp/repo/things/000098-second-thing.json" "{\"val\":1}"
      content ← readDocumentByName "tmp/repo/things" "second-thing" ∷ IO (Maybe Thing)
      content `shouldBe` Just Thing {val = 1}

    it "returns Nothing when reading by a nonexistent name" $ do
      content ← readDocumentByName "tmp/repo/things" "yolo" ∷ IO (Maybe Thing)
      content `shouldBe` Nothing

  describe "documentIdFromName" $ do
    it "returns Just the document id from the document's name" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ ← writeFile "tmp/repo/things/000098-some-thing.json" "THIS SHOULD NOT BE READ"
      i ← documentIdFromName "tmp/repo/things" "some-thing"
      i `shouldBe` Just 98

    it "returns Nothing when run with a nonexistent name" $ do
      i ← documentIdFromName "tmp/repo/things" "yolo"
      i `shouldBe` Nothing

  describe "documentNameFromId" $ do
    it "returns Just the document name from the document's id" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ ← writeFile "tmp/repo/things/000098-some-thing.json" "THIS SHOULD NOT BE READ"
      i ← documentNameFromId "tmp/repo/things" 98
      i `shouldBe` Just "some-thing"

    it "returns Nothing when run with a nonexistent id" $ do
      i ← documentNameFromId "tmp/repo/things" 123
      i `shouldBe` Nothing

  describe "listDocumentKeys" $ do
    it "returns a list of document keys when listing a collection" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ ← writeFile "tmp/repo/things/first-thing.json" "{}"
      _ ← writeFile "tmp/repo/things/second-thing.json" "{}"
      list ← listDocumentKeys "tmp/repo/things"
      sort list `shouldBe` ["first-thing", "second-thing"]

    it "returns an empty when listing a nonexistent collection" $ do
      list ← listDocumentKeys "nonsense"
      list `shouldBe` []

  describe "listEntries" $ do
    it "returns a list of entries when listing a collection" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ ← writeFile "tmp/repo/things/first-thing.json" "{\"val\":1}"
      _ ← writeFile "tmp/repo/things/second-thing.json" "{\"val\":2}"
      list ← listEntries "tmp/repo/things" ∷ IO [Thing]
      sort list `shouldBe` [Thing {val = 1}, Thing {val = 2}]

    it "returns an empty list when listing a nonexistent collection" $ do
      list ← listEntries "nonsense" ∷ IO [Thing]
      list `shouldBe` []

  describe "listCollections" $ do
    it "returns a list of collections" $ do
      createDirectoryIfMissing True "tmp/repo/bits"
      createDirectoryIfMissing True "tmp/repo/pieces"
      insideDirectory "tmp/repo" $ do
        list ← listCollections
        sort list `shouldBe` ["bits", "pieces"]

    it "returns an empty list when listing an empty repo" $ do
      createDirectoryIfMissing True "tmp/repo"
      insideDirectory "tmp/repo" $ do
        list ← listCollections
        sort list `shouldBe` []

setup ∷ IO ()
setup = createRepo "tmp/repo"

cleanup ∷ ActionWith ()
cleanup () = void (try (removeDirectoryRecursive "tmp/repo") ∷ IO (Either IOException ()))
