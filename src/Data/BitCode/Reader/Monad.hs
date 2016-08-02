{-# LANGUAGE BangPatterns #-}
module Data.BitCode.Reader.Monad
  ( BitCodeReader
  , evalBitCodeReader
  , read, ask
  , readBits
  , ppBitCode
  , fromByte
  , FromBits(..)
  , tellGlobalAbbrev, askGlobalAbbrevs
  )
  where

import Prelude hiding (read, readFile)
-- Utility
import Data.Bits (FiniteBits, setBit, zeroBits)
-- reading from file
import qualified Data.ByteString as B (readFile,unpack)
import Data.Word (Word8)
import Data.Bits (testBit)
-- abbrev map
import Data.Maybe (fromMaybe)
-- for Pretty printing
import qualified Text.PrettyPrint as PP
import Data.ByteString (pack)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Base16 (encode)

import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))
-- We basically want a datastructure, where
-- we can ask for N bits from the stream.
-- Can ask how many bits we have consumed.
-- And be able to store Abbreviations.

import Data.BitCode
import Data.BitCode.Abbreviation

class FromBits a where
  parse :: BitCodeReader a

data BitC = BitC { _words :: !Int
                 , _bits :: !Int
                 , _body :: !Bits
                 , _gabbrevs :: !GlobalAbbrevMap
                 } deriving (Show)

instance Monoid BitC where
  mempty = BitC 0 0 mempty mempty
  (BitC w l bs g) `mappend` (BitC w' l' bs' g') = BitC (w+w' + ((l+l') `div` 32)) ((l+l') `mod` 32) (bs `mappend` bs') (g `mappend` g')

data PairS a = PairS { value :: !(Either String a) -- ^ Result
                     , bits :: !BitC               -- ^ bitCodeInfo left
                     } deriving Show

newtype BitCodeReader a = BitCode { runBitCodeReader :: BitC -> PairS a }

evalBitCodeReader :: BitCodeReader a -> Bits -> Either String a
evalBitCodeReader r bits = value $ runBitCodeReader r (BitC 0 0 bits mempty)

-- * Functor
instance Functor BitCodeReader where
  fmap f m = BitCode $ \b -> let PairS a b' = runBitCodeReader m b
                             in PairS (f <$> a)  b'

-- * Applicative
instance Applicative BitCodeReader where
  pure a = BitCode $ \b -> PairS (Right a) b

  m <*> n = BitCode $ \b ->
    let PairS f b' = runBitCodeReader m b
        PairS x b'' = runBitCodeReader n b'
    in PairS (f <*> x) b''

-- * Monad
instance Monad BitCodeReader where
  m >>= n = BitCode $ \b -> case runBitCodeReader m b of
    PairS (Left err) b' -> PairS (Left err) b'
    PairS (Right a) b' -> runBitCodeReader (n a) b'

  fail s = BitCode $ \b -> PairS (Left s) b

-- Monoid makes no sense, or does it?

-- * MonadPlus
instance MonadPlus BitCodeReader where
  mzero = BitCode $ \b -> PairS (Left "") b
  m `mplus` n = BitCode $ \b -> let PairS a b' = runBitCodeReader m b
                                in runBitCodeReader n b'

-- * Alternative
instance Alternative BitCodeReader where
  empty = mzero
  m <|> n = BitCode $ \b -> case runBitCodeReader m b of
                              PairS (Left _) _ -> runBitCodeReader n b
                              res              -> res

-- * Low Level BitCode Functions
read :: Int -> BitCodeReader Bits
read n = BitCode $ \(BitC w b bs g) -> let !h = take n bs
                                           !t = drop n bs
                                       in if length h /= n
                                          then runBitCodeReader (fail "End of stream") mempty
                                          else PairS (pure h) (BitC (w+((b+n) `div` 32)) ((b+n) `mod` 32) t g)

ask :: BitCodeReader (Int, Int)
ask = BitCode $ \b@(BitC ws bs _ _) -> PairS (pure (ws,bs)) b

askGlobalAbbrevs :: BlockId -> BitCodeReader AbbrevMap
askGlobalAbbrevs blockId = BitCode $ \b@(BitC _ _ _ g) -> PairS (pure $ lookupGlobalAbbrev g blockId) b

tellGlobalAbbrev :: BlockId -> BitCode -> BitCodeReader ()
tellGlobalAbbrev blockId abbrev = BitCode $ \b@(BitC _ _ _ g) -> PairS (pure ()) b { _gabbrevs = addGlobalAbbrev g blockId abbrev }
-- * Reading from a file

-- TODO: This is currently highly inefficient.
--       If this was demand driven, on a Lazy ByteString,
--       this would probably be better. For now assume we
--       have enough memory.
fromByte :: Word8 -> Bits
fromByte w = [testBit w i | i <- [0..7]]

toBits :: [Word8] -> Bits
toBits [] = []
toBits (w:ws) = fromByte w ++ toBits ws

readBits :: FilePath -> IO Bits
readBits fp = toBits . B.unpack <$> B.readFile fp

-- * Utility
partition :: Int -> Bits -> [Bits]
partition _ [] = []
partition n xs | length xs < n = [xs]
partition n xs | otherwise = h:partition n t
  where (h,t) = (take n xs, drop n xs)
toBytes :: Bits -> [Word8]
toBytes = map toFiniteBits . partition 8
  where toFiniteBits :: (FiniteBits a) => Bits -> a
        toFiniteBits = foldl setBit zeroBits . map fst . filter ((== True) . snd) . zip [0..]

-- * Pretty Print
ppBitCode :: BitCodeReader PP.Doc
ppBitCode = BitCode $ \b -> PairS (pure $ PP.vcat [ PP.text "* Bitcode"
                            , PP.text "words =" PP.<+> PP.int (_words b)
                            , PP.text "bits =" PP.<+> PP.int (_bits b)
                            , PP.text "body =" PP.<+> ppBitsAndBytes (take (32 * 20) $ _body b)
                            ]) b
  where toBitString :: Bits -> String
        toBitString = map f
          where f True = '1'
                f False = '0'
        toHexString :: [Word8] -> String
        toHexString = unpack . encode . pack
        ppBitsAndBytes :: Bits -> PP.Doc
        ppBitsAndBytes = PP.vcat . map (\bs -> PP.text (toBitString bs) PP.<+> PP.text (toHexString . toBytes $ bs)) . partition 32
