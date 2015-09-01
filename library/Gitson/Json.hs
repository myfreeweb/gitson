{-# LANGUAGE Trustworthy, UnicodeSyntax #-}

-- | Trustworthy wrapper for some libraries
module Gitson.Json (
  ToJSON, FromJSON, encode, fromJSON, json, Result(..), Value, ParseError
, sinkParserEither, sourceFile, ($$), runResourceT
) where

import           Data.Aeson (ToJSON, FromJSON, fromJSON, json, Result(..), Value)
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy (ByteString)
import           Data.Conduit.Attoparsec (sinkParserEither, ParseError)
import           Conduit (sourceFile, ($$), runResourceT)

encode ∷ ToJSON a ⇒ a → ByteString
encode = encodePretty' $ Config { confIndent = 2, confCompare = compare }
