{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.BitCode.Writer.Monad
  ( BitCodeWriter
  , evalBitCodeWriter, execBitCodeWriter
  , tell, ask
  , writeFile, ppBitCodeWriter
  , ToBits(..)
  )
  where

import Data.Monoid (Monoid, mempty, mappend)
import Prelude hiding (writeFile)
-- writing to file
import Data.Word (Word8)
import Data.Bits (FiniteBits, setBit, zeroBits)
import qualified Data.ByteString as BS (writeFile)
-- for Pretty printing
import qualified Text.PrettyPrint as PP
import Data.ByteString (pack, ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Base16 (encode)

import Data.BitCode

-- * BitCode
data BitC = BitC { _words :: Int, _bits :: Int, _body :: Bits } deriving (Show)

instance Monoid BitC where
  mempty = BitC 0 0 []
  (BitC w l bs) `mappend` (BitC w' l' bs') = BitC (w+w' + ((l+l') `div` 32)) ((l+l') `mod` 32) (bs `mappend` bs')

-- * BitCode Writer Monad (with access to the current state)
newtype BitCodeWriter a = BitCode { unBitCode :: BitC -> (a, BitC) }

runBitCodeWriter :: BitCodeWriter a -> (a, BitC)
runBitCodeWriter = flip unBitCode mempty

evalBitCodeWriter :: BitCodeWriter a -> a
evalBitCodeWriter = fst . runBitCodeWriter

execBitCodeWriter :: BitCodeWriter a -> Bits
execBitCodeWriter = _body . snd . runBitCodeWriter

class ToBits a where
  emit :: a -> BitCodeWriter ()

-- * Functor
instance Functor BitCodeWriter where
  fmap f m = BitCode $ \b -> let (a, b') = unBitCode m b
                             in (f a, b')
-- * Applicative
instance Applicative BitCodeWriter where
  pure a = BitCode $ \b -> (a, b)

  m <*> n = BitCode $ \b ->
    let (f, b') = unBitCode m b
        (x, b'') = unBitCode n b'
    in (f x, b' `mappend` b'')

-- * Monad
instance Monad BitCodeWriter where
  m >>= n = BitCode $ \b -> let (a, b') = unBitCode m b in unBitCode (n a) b'

-- * Monoid
instance Monoid (BitCodeWriter ()) where
  mempty = pure ()
  m `mappend` n = BitCode $ \b ->
    let (_, a) = unBitCode m b
        (_, b) = unBitCode n b
    in ((), a `mappend` b)

-- * Low Level BitCode Functions (These need to know about BitCode and BitC)
tell :: Bits -> BitCodeWriter ()
tell bs = BitCode $ \b -> ((), b `mappend` b')
  where n = length bs
        b' = BitC (n `div` 32) (n `mod` 32) bs

-- | Get the number of words and bits in the stream.
ask :: BitCodeWriter (Int, Int)
ask = BitCode $ \b@(BitC ws bs _) -> ((ws,bs),b)

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

-- * Writing out to file.
writeFile :: FilePath -> BitCodeWriter () -> IO ()
writeFile fp = BS.writeFile fp . pack . toBytes . execBitCodeWriter

-- * Pretty Print
ppBitCodeWriter :: BitCodeWriter () -> PP.Doc
ppBitCodeWriter w = PP.vcat [ PP.text "* Bitcode"
                            , PP.text "words =" PP.<+> PP.int (_words b)
                            , PP.text "bits =" PP.<+> PP.int (_bits b)
                            , PP.text "body =" PP.<+> ppBitsAndBytes (_body b)
                            ]
  where b = snd (runBitCodeWriter w)
        toBitString :: Bits -> String
        toBitString = map f
          where f True = '1'
                f False = '0'
        toHexString :: [Word8] -> String
        toHexString = unpack . encode . pack
        ppBitsAndBytes :: Bits -> PP.Doc
        ppBitsAndBytes = PP.vcat . map (\bs -> PP.text (toBitString bs) PP.<+> PP.text (toHexString . toBytes $ bs)) . partition 32
