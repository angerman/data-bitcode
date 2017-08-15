module Bench where

import Prelude hiding (writeFile)
import Data.Binary (decodeFile)
import Data.BitCode
import Data.BitCode.Writer             (emitTopLevel)
import Data.BitCode.Writer.Monad       (writeFile)
import Data.BitCode.Writer.Combinators (withHeader)
import Data.Word (Word8)
import Data.Bits (FiniteBits, setBit, zeroBits)

import           Criterion.Main

--------------------------------------------------------------------------------
-- Reading a serialized module.
readModule :: FilePath -> IO [BitCode]
readModule = decodeFile

writeModule :: FilePath -> [BitCode] -> IO ()
writeModule f = writeFile f . withHeader True . emitTopLevel

--------------------------------------------------------------------------------
-- Turing a stream of Bits into Bytes
-- type Bits = [Bool]

partition :: Int -> Bits -> [Bits]
partition _ [] = []
partition n xs | length xs < n = [xs]
partition n xs | otherwise = h:partition n t
  where (h,t) = (take n xs, drop n xs)

toBytes :: Bits -> [Word8]
toBytes = map toFiniteBits . partition 8
  where toFiniteBits :: (FiniteBits a) => Bits -> a
        toFiniteBits = foldl setBit zeroBits . map fst . filter ((== True) . snd) . zip [0..]



toBytes2 :: Bits -> [Word8]
toBytes2 = go
  where
    go (b0:b1:b2:b3:b4:b5:b6:b7:rest) =
        let byte =
                theBit 0 b0
              $ theBit 1 b1
              $ theBit 2 b2
              $ theBit 3 b3
              $ theBit 4 b4
              $ theBit 5 b5
              $ theBit 6 b6
              $ theBit 7 b7
                zeroBits
        in byte : go rest
      where
        theBit :: Int -> Bool -> Word8 -> Word8
        theBit n True  = flip setBit n
        theBit _ False = id
    go [] = []
    go bs = go (take 8 $ bs ++ [False, False, False, False
                               ,False, False, False, False])

main :: IO ()
main = defaultMain [
--  bgroup "writer"
--  [ bench "HelloWorld" $ nfIO (writeModule "HelloWorld.bc" =<< readModule "bench/data/HelloWorld.mod")
--  , bench "HelloWorld2" $ nfIO (writeModule "HelloWorld2.bc" =<< readModule "bench/data/HelloWorld2.mod")
--  ]
    bgroup "toBytes"  [ bench (show i) $ nf toBytes  [False | _ <- [1..i]] | i <- [8000,16000..96000] ]
  , bgroup "toBytes2" [ bench (show i) $ nf toBytes2 [False | _ <- [1..i]] | i <- [8000,16000..96000] ]
  ]
