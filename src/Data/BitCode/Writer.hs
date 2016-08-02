{-# LANGUAGE TupleSections #-}
module Data.BitCode.Writer where

import Data.BitCode
import Data.BitCode.Writer.Monad as M
import Data.BitCode.Writer.ToBits

writeFile :: FilePath -> [BitCode] -> IO ()
writeFile fp = M.writeFile fp . emitTopLevel

emitTopLevel :: [BitCode] -> BitCodeWriter ()
emitTopLevel = mapM_ (emit . (2::Int,))
