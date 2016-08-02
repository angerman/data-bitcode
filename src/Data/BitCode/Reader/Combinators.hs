module Data.BitCode.Reader.Combinators where

import Prelude hiding (read)
import Data.Word (Word8, Word32)
import Data.Bits (FiniteBits, setBit, zeroBits, shift, (.|.))

import Control.Applicative ((<|>))

import Data.BitCode
import Data.BitCode.Reader.Monad

parseBytes :: [Word8] -> BitCodeReader ()
parseBytes ws = do
  bits' <- read len
  if bits == bits' then return ()
    else fail $ "not enough bits. Trying to read " ++ (show ws) ++ " but only got " ++ (show $ length bits') ++ " bits."
  where bits :: Bits
        bits = concatMap fromByte ws
        len  = length bits

toFiniteBits :: (FiniteBits a) => Bits -> a
toFiniteBits = foldl setBit zeroBits . map fst . filter ((== True) . snd) . zip [0..]

parseFixed :: (FiniteBits a) => Int -> BitCodeReader a
parseFixed n = read n >>= return . toFiniteBits

parseVBR :: (FiniteBits a) => Int -> BitCodeReader a
parseVBR n = do
  v <- parseFixed (n-1)
  c <- read 1
  if c == [False]
    then return v
    else do v' <- parseVBR n
            return $ v .|. (shift v' (n-1))

readFixed :: (Show a, FiniteBits a) => Int -> a -> BitCodeReader ()
readFixed n e = do
  v <- parseFixed n
  if e == v then return () else fail $ "failed to read " ++ show e ++ "; got " ++ show v

readBit :: Bool -> BitCodeReader ()
readBit = readFixed 1

parseWord32 :: BitCodeReader Word32
parseWord32 = parseFixed 32

-- * inverse of align
skipToNbits :: Int -> BitCodeReader ()
skipToNbits n = snd <$> ask >>= \x -> read ((-x + n) `mod` 32) >> return ()
skipTo32bits :: BitCodeReader ()
skipTo32bits = skipToNbits 32

parseHeader :: BitCodeReader (Word32, Word32, Word32, Word32)
parseHeader = do
  parseBytes [0xde,0xc0,0x17,0x0b]
  version <- parseWord32
  offset  <- parseWord32
  len     <- parseWord32
  cpuType <- parseWord32
  return (version, offset, len, cpuType)

parseLLVMIRHeader :: BitCodeReader ()
parseLLVMIRHeader = parseBytes [0x42,0x43,0xc0,0xde]

optional :: BitCodeReader a -> BitCodeReader (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
