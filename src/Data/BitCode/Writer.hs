{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE TupleSections #-}
module Data.BitCode.Writer where

import Data.BitCode
import Data.BitCode.Writer.Monad as M
import Data.BitCode.Writer.ToBits

import Data.Word (Word64)

writeFile :: FilePath -> [BitCode] -> IO ()
writeFile fp = M.writeFile fp . emitTopLevel

emitTopLevel :: [BitCode] -> Bitstream ()
emitTopLevel = mapM_ (emit . (2::Word64,))
