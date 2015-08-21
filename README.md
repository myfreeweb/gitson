# gitson [![Hackage](https://img.shields.io/hackage/v/gitson.svg?style=flat)](https://hackage.haskell.org/package/gitson) [![Build Status](https://img.shields.io/travis/myfreeweb/gitson.svg?style=flat)](https://travis-ci.org/myfreeweb/gitson) [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A simple document store library for Git + JSON, based on [Aeson].
Uses command line git, at least for now.
No fancy indexes and stuff, but it does what I need right now.
Transactions use [flock], so it's safe even across completely separate programs!

[Aeson]: http://hackage.haskell.org/package/aeson
[flock]: http://hackage.haskell.org/package/flock

## Usage

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Gitson
import Gitson.Util (insideDirectory)
import Data.Aeson.TH
import Control.Monad.IO.Class (liftIO)

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing) -- there are non-Template ways of doing this, see aeson docs

main :: IO ()
main = do
  -- Creating a new "database," basically mkdir + git init
  createRepo "./content"

  -- Writing data to a "database" happens in transactions
  -- A transaction is committed (write files & git commit)
  -- after the block is executed, just like in SQL databases
  -- Also, transactions are thread-safe
  transaction "./content" $ do
    -- order:    (collection) (key        ) (data)
    saveDocument "things"     "first-thing" Thing {val = 1}
    -- Collections are created automatically, like in MongoDB
    liftIO $ putStrLn "Written first-thing"
    -- You have to use liftIO to do IO actions inside of a transaction!
    -- Because a transaction is a monad transformer, WriterT actually

  -- Reading data
  -- (These are normal IO actions, so if you want
  --  to read inside of a transaction, liftIO.
  --  Note: transaction already includes insideDirectory!
  --  Warning: you can't read what you've written in the current transaction!!!
  --  You can only read what's been written before the transaction began.)
  insideDirectory "./content" $ do
    colls <- listCollections
          -- ["things"]
    keys <- listDocumentKeys "things"
         -- ["first-thing"]
    first-thing <- readDocument "things" "first-thing" :: IO (Maybe Thing)
         -- Just Thing {val = 1}
    things <- readEntries "things" :: IO [Thing]
           -- [Thing {val = 1}]

  -- Note: insideDirectory is just a function that changes
  -- the current directory, executes an action and changes it back.
  -- You can use reading actions without it, like this:
  keys <- listDocumentKeys "./content/things"


  -- And now, some bells and whistles:
  -- Numeric id support
  transaction "./content" $ do
    saveNextDocument "things" "hello-world" Thing {val = 1}
    -- will save to things/000001-hello-world.json
  insideDirectory "./content" $ do
    thing <- readDocumentById "things" 1
    same-thing <- readDocumentByName "things" "hello-world"
    -- both will read from things/000001-hello-world.json
    
    i <- documentIdFromName "things" "hello-world"
      -- 1
    n <- documentNameFromId "things" 1
      -- "hello-world"
```

## Development

Use [stack] to build.  
Use ghci to run tests quickly with `:test` (see the `.ghci` file).

``bash
$ stack build

$ stack test && rm tests.tix

$ stack bench

$ stack ghci --ghc-options="-fno-hpc"
``

[stack]: https://github.com/commercialhaskell/stack

## Contributing

Please feel free to submit pull requests!
Bugfixes and simple non-breaking improvements will be accepted without any questions :-)

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/2/0/).

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
