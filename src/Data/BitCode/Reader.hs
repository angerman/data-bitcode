{-# LANGUAGE LambdaCase #-}
module Data.BitCode.Reader
  (readFile)
where

import Prelude hiding (readFile)
import Data.BitCode
import Data.BitCode.Abbreviation

import Data.BitCode.Reader.Monad
import Data.BitCode.Reader.Combinators
import Data.BitCode.Reader.FromBits

readFile :: FilePath -> IO (Either String [BitCode])
readFile f = flip fmap (readBits f) . evalBitCodeReader $ do
  optional parseHeader
  parseLLVMIRHeader
  parseStream 2 mempty
