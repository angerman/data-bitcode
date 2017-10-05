{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, KindSignatures, BinaryLiterals, RecursiveDo, LambdaCase, RankNTypes, FlexibleContexts, BangPatterns #-}
module Data.BitCode.Writer.Monad
  ( nullBuff, addBuff, mkBuff
  , execBitstream, evalBitstream, runBitstream
  -- * Monadic interface
  , withOffset
  -- ** locations
  , loc, locBytes, locWords
  -- ** Bits
  , emitBit, emitBits
  -- ** Words
  , emitWord8, emitWord32R, emitWord32
  -- ** Others
  , emitFixed, emitVBR, emitChar6
  -- ** alignment
  , alignWord8, alignWord32
  -- ** writing
  , writeFile, withHeader
  -- ** For testing
  , Stream(..), Buff(..), Bitstream(..), bitstream
  , streams
  , BType, bSize, showWord8, mkBitstream, emitLLVMIRHeader, emitDarwinHeader, BitstreamState(..), bitstreamBytes, bToWord8, bToOrder, emitVBR_slow, emitVBR_fast, bitstreamBS
  ) where

import Prelude hiding (last, words, writeFile, tail)

import Data.Word
import Data.Bits
import Data.Foldable
import Control.Monad.Trans.State.Strict

import qualified Data.List as L
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Put as Bin (runPut)
import qualified Data.Binary as Bin (put)
import Data.Binary (Put)
import Data.Binary.Put (PutM)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
-- import Data.Monoid ((<>))
import Data.Semigroup (Semigroup, (<>))
import Control.Monad.Fix

import GHC.Stack (HasCallStack)

import Debug.Trace

-- | The position in the stream.
type Position = Int

bSize :: Int
-- ensure the order is correct
bToOrder :: BType -> BType
bPut :: BType -> Put

-- Word64
type BType = Word64
bSize = 64
bToOrder = byteSwap64
bPut = Bin.put . byteSwap64
bToWord8 w = [fromIntegral $ shift w (-i) | i <- [0,8..56]]
-- type BType = Word8
-- bSize = 8
-- bToOrder = id -- Word8
-- bPut = Bin.put
-- bToWord8 = pure

-- | A @Word8@ buffer, tracking the number of bits.
-- I don't think those Unpacks are necessary, -funbox-small-strict-fields is on by default
data Buff = Buff !Int !BType deriving (Eq, Ord)

mask :: (FiniteBits a, Num a) => Int -> a -> a
mask n w = m .&. w
  where m = setBit zeroBits (n+1) - 1

nullBuff :: Buff
nullBuff = Buff 0 0

-- | Adding two buffers. E.g. copying over the bits from one buffer into the
-- other. If we complete a @Word8@, this will be in the first position of the
-- result.
--
-- The bitcode stream will look like
--
-- 0      8 9     16 17    32
-- |      | |      | |      |
-- 10101111 00001100 10101010
--
-- The on disk representation will however look like
--
-- 11110101 00110000 01010101
-- |      | |      | |      |
-- 8      0 16     9 32    17
--
-- Thus, to now have to rearange the bits before writing to disk, we will
-- encode them in disk order (as opposed to stream order) from the start.
--
-- Assume we have two buffers (1, 0b00000001) and (1, 0b00000001), we expect
-- to obtain (2, 0b00000011). Thus we want to shift the second buffer by the
-- length of the first, and or them.
--
-- In the spill case B = (5, 0b00010101) and C = (4, 0b00001111) we expect to get
-- (Just 0b11110101, (1, b000000001))
--
addBuff :: Buff -> Buff -> (Maybe BType, Buff)
addBuff (Buff n w ) (Buff n' w' ) | n+n' < bSize  = (Nothing
                                                    , Buff (n+n') (w .|. (shift w' n)))
                                  | otherwise = (Just (w .|. (shift w' n))
                                                  , Buff ((n+n') `mod` bSize) (shift w' (n-bSize)))


-- | Smart constructor for @Buff@. Ensures that
-- the stored byte is masked properly.
mkBuff :: Int -> BType -> Buff
mkBuff n w = Buff n (mask n w)

-- | A stream is a number of Words, a buffer and a position (length of the stream) marker.
data Stream f a = S
  { _words  :: !(f BType)
  , _buffer :: !Buff
  , _len    :: !Position
  }

deriving instance Eq (f BType) => Eq (Stream f a)
deriving instance Ord (f BType) => Ord (Stream f a)

data Streams f a = Streams
  { _substreams :: !(Seq (Stream f a))
  , _total_len :: !Position
  }

deriving instance Eq (f BType) => Eq (Streams f a)
deriving instance Ord (f BType) => Ord (Streams f a)

instance ( Semigroup (f BType)
         , Foldable f
         , Traversable f
         , Applicative f) => Semigroup (Stream f a) where
  (<>) !lhs  !(S _ _ 0) = lhs
  (<>) !(S _ _ 0) !rhs = rhs
  (<>) !(S w b p) !(S w' b' p') =
    let !r =
          case b of
    -- there are no bits in the buffer. We can simply
    -- concatinate lhs and rhs
            Buff 0 _ -> S (w <> w') b' (p+p')
            -- there are already @n@ bites in the buffer. We will
            -- need to shift all the bits in the RHS left by 8-n.
            Buff n c | null w' -> case addBuff b b' of
                                      (Just w'', b'') -> S (w <> pure w'') b'' (p+p')
                                      (Nothing,  b'') -> S w b'' (p+p')
                     | otherwise -> let !(l, w'') = L.mapAccumL (go' n) c w'
                                    in case addBuff (Buff n l) b' of
                                        (Just w''', b'') -> S (w <> w'' <> pure w''') b'' (p + p')
                                        (Nothing,   b'') -> S (w <> w'') b'' (p + p')
              where go' :: Int    -- ^ shift
                        -> BType  -- ^ buff
                        -> BType  -- ^ input
                        -> ( BType   -- ^ new buff
                           , BType ) -- ^ output
                    go' !n !b !w = (shift w (n-bSize), b .|. shift w n)
    in r

  {-# SPECIALIZE instance Semigroup (Stream Seq a) #-}
  {-# SPECIALIZE instance Semigroup (Stream [] a) #-}

instance ( Semigroup (f BType)
         , Monoid (f BType)
         , Foldable f
         , Traversable f
         , Applicative f) => Monoid (Stream f a) where
  mempty = S mempty nullBuff 0
  mappend = (<>)
  {-# SPECIALIZE instance Monoid (Stream Seq a) #-}
  {-# SPECIALIZE instance Monoid (Stream [] a) #-}

instance Semigroup (Streams f a) where
  (<>) !lhs !(Streams _ 0) = lhs
  (<>) !(Streams _ 0) !rhs = rhs
  (<>) !(Streams ss1 p1) !(Streams ss2 p2) = let !ss' = ss1 <> ss2
                                                 !p'  = p1  +  p2
                                             in Streams ss' p'

  {-# SPECIALIZE instance Semigroup (Streams Seq a) #-}
  {-# SPECIALIZE instance Semigroup (Streams [] a) #-}

instance Monoid (Streams f a) where
  mempty = Streams mempty 0
  mappend = (<>)

  {-# SPECIALIZE instance Monoid (Streams Seq a) #-}
  {-# SPECIALIZE instance Monoid (Streams [] a) #-}

-- mappend is not cheap here.
type ListStream = Stream [] BType
type SeqStream  = Stream Seq BType
type SeqStreams = Streams Seq  BType

toListStream :: Foldable f => Stream f a -> Stream [] a
toListStream (S w b p) = S (toList w) b p

{-# SPECIALIZE toListStream :: Stream Seq a -> Stream [] a #-}

runStreams :: Streams Seq a -> Stream Seq a
runStreams (Streams ss _) = foldl' mappend mempty ss

{-# SPECIALIZE runStreams :: Streams Seq a -> Stream Seq a #-}

-- So we have Streams, which are Sequences of Stream.
-- S0 # # # # # # # # # # # # # # # # # # # +
-- S1 # # # # #
-- S2 # # # # # # # # +
-- S3 # # +
-- S4 # #

fs :: Stream Seq a -> [Stream Seq a] -> Put
fs (S _ _ 0)          [] = pure ()
fs (S _ (Buff _ b) _) [] = bPut b
fs b (x:xs) = let !(S w b'@(Buff n _) _) = b <> x in
                mapM_ bPut w >> fs (S mempty b' n) xs

data BitstreamState = BitstreamState !SeqStreams !Position deriving Show

bssPosition (BitstreamState _ p) = p

newtype Bitstream a = Bitstream { unBitstream :: State BitstreamState a }
  deriving (Functor, Applicative, Monad, MonadFix)

stream :: (SeqStream, Position, a) -> SeqStream
stream (s,_,_) = s
position :: (SeqStream, Position, a) -> Position
position (_,p,_) = p
value :: (SeqStream, Position, a) -> a
value (_,_,v) = v

-- I think we want something like putBitstream, which does the runState
-- and isntead of runStreams, does putStreams
--
putBitstream :: Position -> Bitstream a -> Put
putBitstream p (Bitstream f) = case snd (runState f (BitstreamState mempty 0))
  of BitstreamState ss _ -> putStreams ss
-- ss is Seq (Streams Seq a)
putStreams :: Streams Seq a -> Put
putStreams (Streams ss total_len) = fs mempty (toList ss)

putStream :: Stream Seq a -> PutM (Stream Seq a)
putStream (S as b p) = mapM_ bPut as >> (pure $ S mempty b p)

runBitstream :: Position -> Bitstream a -> (SeqStream, Position, a)
runBitstream p (Bitstream f) = case runState f (BitstreamState mempty 0)
  of (a, BitstreamState ss p) -> (runStreams $ ss, p, a)

execBitstream :: Position -> Bitstream a -> Seq BType
execBitstream p a = _words . stream . runBitstream p $ a >> alignWord8

evalBitstream :: Position -> Bitstream a -> a
evalBitstream p = value . runBitstream p

bitstreamBytes :: Position -> Bitstream a -> [Word8]
bitstreamBytes p a = let (S ws (Buff _ b) l) = stream . runBitstream p $ a >> alignWord8
                         in take (l `div` 8) $ (concatMap bToWord8 . toList $ ws) ++ bToWord8 b

-- NOTE: Be sure not to be strict in the buffer.  Strictness in the
-- buffer will prevent recursive do.
streams :: Foldable f => f BType -> Buff -> Position -> SeqStreams
streams !w b !p
  | p == 0 = mempty
  | otherwise = Streams (pure $ S (Seq.fromList . toList $ w) b p) p

-- {-# SPECIALIZE streams :: [BType] -> Buff -> Position -> SeqStreams #-}
bitstream :: Foldable f => f BType -> Buff -> Int -> Bitstream ()
bitstream !w b !p = Bitstream $ modify' $ \(BitstreamState ss p') -> BitstreamState (ss <> streams w b p) (p + p')
{-# SPECIALIZE bitstream :: [BType] -> Buff -> Int -> Bitstream () #-}

bitstream' :: Foldable f => f BType -> Buff -> Int -> Bitstream ()
bitstream' !w !b !p = Bitstream $ modify' $ \(BitstreamState ss p') -> BitstreamState (ss <> streams w b p) (p + p')
{-# SPECIALIZE bitstream' :: [BType] -> Buff -> Int -> Bitstream () #-}

mkBitstream :: (Integral a, FiniteBits a) => Word64 -> a -> Bitstream ()
mkBitstream !n b | n' < bSize = bitstream [] (mkBuff n' (fromIntegral b)) n'
                 | otherwise = bitstream [shift' i | i <- [0..d-1]] (mkBuff m (shift' d)) n'
  where n' = fromIntegral n
        (d,m) = n' `divMod` bSize
        shift' :: Int -> BType
        shift' n = fromIntegral (shift b (-n*bSize))

mkBitstream' :: (Integral a, FiniteBits a) => Word64 -> a -> Bitstream ()
mkBitstream' !n !b | n' < bSize = bitstream [] (mkBuff n' (fromIntegral b)) n'
                   | otherwise = bitstream [shift' i | i <- [0..d-1]] (mkBuff m (shift' d)) n'
  where n' = fromIntegral n
        (d,m) = n' `divMod` bSize
        shift' :: Int -> BType
        shift' n = fromIntegral (shift b (-n*bSize))

-- Monadic Bitstream API

withOffset :: Int -> Bitstream a -> Bitstream a
withOffset n x = Bitstream $ do
  bss@(BitstreamState ss pos) <- get
  let (r, BitstreamState ss' pos') = runState (unBitstream x) (BitstreamState ss n)
  put $ BitstreamState ss' (pos + pos')
  return r

loc :: HasCallStack => Bitstream Position
loc = Bitstream $ gets bssPosition

locBytes :: HasCallStack => Bitstream Word32
locBytes = Bitstream $ gets $ fromIntegral . (`div` 8) . bssPosition

locWords :: HasCallStack => Bitstream Word32
locWords = Bitstream $ gets $ fromIntegral . (`div` 32) . bssPosition

emitBit :: HasCallStack => Bool -> Bitstream ()
emitBit True  = bitstream [] (Buff 1 1) 1
emitBit False = bitstream [] (Buff 1 0) 1

emitBits :: HasCallStack => Int -> BType -> Bitstream ()
emitBits 0 _ = pure ()
emitBits n b  | n < 8 = do
                 -- traceM $ "emitting " ++ show n ++ " bits; value = " ++ show b
                 let buff = mkBuff n b
                 --traceM $ "buffer " ++ show buff
                 bitstream [] buff n
              | otherwise = error $ "cannot emit " ++ show n ++ " bits from Word8."

emitWord8 :: HasCallStack => Word8 -> Bitstream ()
emitWord8 = mkBitstream 8
emitWord32R :: HasCallStack => Word32 -> Bitstream ()
emitWord32R = mkBitstream 32 . byteSwap32
emitWord32 :: HasCallStack => Word32 -> Bitstream ()
emitWord32 = mkBitstream 32

emitFixed :: HasCallStack => Word64 -> Word64 -> Bitstream ()
emitFixed 0 _ = pure ()
emitFixed !n _ | n > 64 = error $ "invalid number of bits. Cannot emit " ++ show n ++ " bits from Word64."
emitFixed !n !w = mkBitstream' n w

emitVBR :: HasCallStack => Word64 -> Word64 -> Bitstream ()
--emitVBR 0 _ = pure ()
emitVBR !n _ | n < 2 = error "emitting VBR 0 impossible."
emitVBR !n !w = emitVBR_fast n w

emitVBR_fast :: HasCallStack => Word64 -> Word64 -> Bitstream ()
emitVBR_fast n w | l > 64 = emitVBR_slow n w
                 | otherwise = emitFixed (fromIntegral l) (go w n')
  where n' = fromIntegral n
        mask :: Word64
        mask = 2^(n'-1)-1
        cont = 2^(n'-1)
        msb x = finiteBitSize x - countLeadingZeros x
        l = case (msb w) `divMod` (n'-1) of
              (0,0) -> n'
              (d,0) -> n' * d
              (d,m) -> n' * (d+1)
        go :: Word64 -> Int -> Word64
        go w n = let tail = shift w (1-n)
                 in if popCount tail == 0
                    then w .&. mask
                    else w .&. mask .|. cont .|. shift (go tail n) n
emitVBR_slow :: HasCallStack => Word64 -> Word64 -> Bitstream ()
emitVBR_slow !n !w =do
  emitFixed (n-1) w
  let tail = shift w (1-(fromIntegral n))
    in if popCount tail == 0
       then emitBit False
       else emitBit True >> emitVBR n tail

  where logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

emitChar6 :: HasCallStack => Char -> Bitstream ()
emitChar6 '_' = emitBits 6 63
emitChar6 '.' = emitBits 6 62
emitChar6 c | 'a' <= c && c <= 'z' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum 'a')
            | 'A' <= c && c <= 'Z' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum 'A') + 26
            | '0' <= c && c <= '9' = emitBits 6 . fromIntegral $ (fromEnum c - fromEnum '0') + 52
            | otherwise = fail $ "char '" ++ c:"' not in [a-zA-Z0-9._]"

alignWord8 :: HasCallStack => Bitstream ()
alignWord8 = do
  bits <- (`mod` 8) <$> loc
  case bits of
    0 -> pure ()
    x -> emitBits (8 - x) 0

alignWord32 :: HasCallStack => Bitstream ()
alignWord32 = flip mod 32 <$> loc >>= \case
  0 -> pure ()
  x | 32 - x < 8  -> emitBits (32 - x) 0
  x | 32 - x < 16 -> emitWord8 0 >> emitBits (24 - x) 0
  x | 32 - x < 24 -> emitWord8 0 >> emitWord8 0 >> emitBits (16 - x) 0
  x | 32 - x < 32 -> emitWord8 0 >> emitWord8 0 >> emitWord8 0 >> emitBits (8 - x) 0

writeFile
  :: HasCallStack
  => FilePath -> Bitstream a -> IO ()
writeFile f = B.writeFile f . Bin.runPut . putBitstream 0

bitstreamBS :: HasCallStack => Bitstream a -> B.ByteString
bitstreamBS = Bin.runPut . putBitstream 0

-- encode' . execBitstream 0
--  where encode' = Bin.runPut . mapM_ (Bin.put . bToOrder)
-- * BitCode Header
-- | put the BitCodeHeader, on darwin a special wrapper is
-- apparently only required, to make it compatible with
-- the system archiver.
withHeader
  :: HasCallStack
  => Bool    -- ^ wrap in darwin header
  -> Bitstream () -- ^ body bitcode
  -> Bitstream ()
withHeader isDarwin body = mdo
  -- if it's darwin, we add the header with the length of the body
  -- (#words * 4 bytes) as well as the LLVM IR Header (4 bytes)
  if isDarwin
    then emitDarwinHeader n
    else pure ()
  -- start a temporary offset from 0. To get the size
  -- of he body.
  n <- withOffset 0 $ do
    emitLLVMIRHeader
    body
    alignWord32
    locBytes -- get the number of bytes emitted.

  return ()

emitDarwinHeader
  :: Word32 -- ^ number of bytes in body
  -> Bitstream ()
emitDarwinHeader len = do
  emitWord32 0x0b17c0de                -- 0x0b17c0de   4
  emitWord32 0                         -- version: 0  +4
  emitWord32 20                        -- offset: 20  +4 <--.
  emitWord32 len                       -- length      +4    |
  emitWord32 cpuType                   --             +4 => 20 in total.
  where
    -- We are hardcoding x86_64 for now.
    cpuType :: Word32
    cpuType = 0x01000000 -- DARWIN_CPU_ARCH_ABI64
            +          7 -- DARWIN_CPU_TYPE_X86(7),
                         -- DARWIN_CPU_TYPE_ARM(12),
                         -- DARWIN_CPU_TYPE_POWERPC(18)

emitLLVMIRHeader :: Bitstream ()
emitLLVMIRHeader = emitWord32R 0x4243c0de -- 'BC' 0xc0de

-- Show instances. These make parsing debug output much easier.

showWord8 :: BType -> String
showWord8 w = '0':'b':(map f $ [testBit w i | i <- [0..bSize-1]])
  where f True  = '1'
        f False = '0'

instance Show Buff where
  show (Buff n w) = show n ++ " bits: " ++ showWord8 w

instance (Functor f, Foldable f) => Show (Stream f a) where
  show (S ws (Buff n b) p) | null ws = show p ++ " bits: " ++ take n (showWord8' b)
                             | otherwise = show p ++ " bits: " ++ foldl1 (\x y -> x ++ " " ++ y) (fmap showWord8' ws) ++ " " ++ take n (showWord8' b)
    where showWord8' w = map f $ [testBit w i | i <- [0..bSize-1]]
          f True = '1'
          f False = '0'

instance (Functor f, Foldable f) => Show (Streams f a) where
  show (Streams ss len) = "total len: " ++ show len ++ ": " ++ (unlines $ map show $ toList ss)
