{-# LANGUAGE LambdaCase #-}
module Prof where

import qualified Data.BitCode.Writer.Monad as Bitcode (writeFile, withHeader)
import qualified Data.BitCode.Writer as Bitcode (emitTopLevel)

import System.Environment (getArgs)
import Data.Binary (decodeFile)

main :: IO ()
main = getArgs >>= \case
  [inf, outf] -> Bitcode.writeFile outf
                 . Bitcode.withHeader True
                 . Bitcode.emitTopLevel =<< decodeFile inf
  _   -> putStrLn "expected InputFile OutputFile"

