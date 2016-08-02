module Data.BitCode.Writer.Combinators where

import Data.BitCode
import Data.BitCode.Writer.Monad
import Data.Word (Word8, Word32)
import Data.Bits (FiniteBits, testBit, popCount, shift)

-- * BitCode Functions
emitBit :: Bit -> BitCodeWriter ()
emitBit = tell . pure
emitBits :: Int -> Bool -> BitCodeWriter ()
emitBits n = tell . replicate n
emitFixed :: (FiniteBits a) => Int -> a -> BitCodeWriter ()
emitFixed n x = tell [testBit x i | i <- [0..(n-1)]]
emitVBR :: (FiniteBits a) => Int -> a -> BitCodeWriter ()
emitVBR n x = do
  emitFixed (n-1) x
  let tail = shift x (-n+1)
    in if popCount tail > 0
       then emitBit True >> emitVBR n tail
       else emitBit False

-- * Utility BitCode Functions
emitWord8 :: Word8 -> BitCodeWriter ()
emitWord8 = emitFixed 8
emitWord32 :: Word32 -> BitCodeWriter ()
emitWord32 = emitFixed 32

-- | Emit words into the bitstream
emitWords8 :: [Word8] -> BitCodeWriter ()
emitWords8 = mapM_ emitWord8
-- | Emit words in reverse order into the bitstream
emitWords8R :: [Word8] -> BitCodeWriter ()
emitWords8R = emitWords8 . reverse

emitChar6 :: Char -> BitCodeWriter ()
emitChar6 '_' = emitFixed 6 (63 :: Word8)
emitChar6 '.' = emitFixed 6 (62 :: Word8)
emitChar6 c | 'a' <= c && c <= 'z' = emitFixed 6 $ (fromEnum c - fromEnum 'a')
            | 'A' <= c && c <= 'Z' = emitFixed 6 $ (fromEnum c - fromEnum 'A') + 26
            | '0' <= c && c <= '9' = emitFixed 6 $ (fromEnum c - fromEnum '0') + 52
            | otherwise = fail $ "char '" ++ [c] ++ "' not in [a-zA-Z0-9._]"

-- * Sizing combinators
-- | Align to 32bits. E.g. fill up the stream with 0s
-- until it's multiple of 32bits.
align :: BitCodeWriter ()
align = do n <- fmap snd ask
           if n == 0
             then return ()
             else emitBits (32 - n) False

-- | Number of words required for this bitcode
size :: (Num a) => BitCodeWriter () -> BitCodeWriter a
size w = case evalBitCodeWriter (w >> ask) of
           (w, 0) -> return . fromIntegral $ w
           _ -> fail $ "size must be mulitple of 4 bytes"

-- * BitCode Header
-- | put the BitCodeHeader, on darwin a special wrapper is
-- apparently only required, to make it compatible with
-- the system archiver.
withHeader
  :: Bool    -- ^ wrap in darwin header
  -> BitCodeWriter () -- ^ body bitcode
  -> BitCodeWriter ()
withHeader isDarwin body = do
  -- if it's darwin, we add the header with the length of the body
  -- (#words * 4 bytes) as well as the LLVM IR Header (4 bytes)
  if isDarwin
    then emitDarwinHeader =<< (fmap (*4) $ size body')
    else pure ()
  body'
  where body' = emitLLVMIRHeader >> body
        emitDarwinHeader
          :: Word32 -- ^ number of bytes in body
          -> BitCodeWriter ()
        emitDarwinHeader len = do
          emitWords8R [0x0b, 0x17, 0xc0, 0xde] -- 0x0b17c0de   4
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
        emitLLVMIRHeader :: BitCodeWriter ()
        emitLLVMIRHeader = emitWords8 [0x42, 0x43, 0xc0, 0xde] -- 'BC' 0xc0de
