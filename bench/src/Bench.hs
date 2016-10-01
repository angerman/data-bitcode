module Bench where

import Prelude hiding (writeFile)
import Data.Binary (decodeFile)
import Data.BitCode
import Data.BitCode.Writer             (emitTopLevel)
import Data.BitCode.Writer.Monad       (writeFile)
import Data.BitCode.Writer.Combinators (withHeader)

import           Criterion.Main

--------------------------------------------------------------------------------
-- Reading a serialized module.
readModule :: FilePath -> IO [BitCode]
readModule = decodeFile

writeModule :: FilePath -> [BitCode] -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel

main :: IO ()
main = defaultMain [
  bgroup "writer"
  [ bench "HelloWorld" $ nfIO (writeModule "HelloWorld.bc" =<< readModule "bench/data/HelloWorld.mod")
  , bench "HelloWorld2" $ nfIO (writeModule "HelloWorld2.bc" =<< readModule "bench/data/HelloWorld2.mod")
  ]]
