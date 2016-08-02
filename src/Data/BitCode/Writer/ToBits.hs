{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.BitCode.Writer.ToBits where

import Data.Word (Word8)

import Data.BitCode
import Data.BitCode.Writer.Monad
import Data.BitCode.Writer.Combinators

import Data.BitCode.IDs.FixedAbbrev

-- * ToBits instances
instance ToBits EncVal where
  emit (Fixed v) = emitFixed 3 (1 :: Word8) >> emitVBR 5 v
  emit (VBR   v) = emitFixed 3 (2 :: Word8) >> emitVBR 5 v
  emit Arr       = emitFixed 3 (3 :: Word8)
  emit Char6     = emitFixed 3 (4 :: Word8)
  emit Blob      = emitFixed 3 (5 :: Word8)

instance ToBits Op where
  emit (Lit v)   = emitBit True  >> emit (W64 v)
  emit (Enc e)   = emitBit False >> emit e

instance ToBits [Op] where
  emit = mapM_ emit

instance ToBits Field where
  emit (Vbr n v) = emitVBR n v
  emit (Fix n v) = emitFixed n v
  emit (Chr c)   = emitChar6 c
  emit (Len n)   = emitVBR 6 n
  emit (W64 w)   = emitVBR 8 w

instance ToBits (Int, BitCode) where
  emit (width, (Located _ block))   = emit (width, block)
  emit (width, (Block id len body)) = do
    emitFixed width $ fromEnum ENTER_SUBBLOCK
    emitVBR 8 id
    emitVBR 4 len
    align
       -- note: putSized, unaligned may be off by one.
       --       as the body may not be multiple of 32
       --       bits.
    putSized $ do
      mapM_ (emit . (len,)) body
      emitVBR len $ fromEnum END_BLOCK
      align
   where putSized :: BitCodeWriter () -> BitCodeWriter ()
         putSized b = size b >>= emitWord32 >> b
  emit (width, (DefAbbrevRecord ops)) = emitFixed width (fromEnum DEFINE_ABBREV) >> emitVBR 5 (length ops) >> mapM_ emit ops
  emit (width, (UnabbrevRecord code ops)) = emitFixed width (fromEnum UNABBREV_RECORD) >> emitVBR 6 code >> emitVBR 6 (length ops) >> mapM_ (emitVBR 6) ops
  emit (width, (AbbrevRecord code fields)) = emitFixed width code >> (mapM_ emit $ filter (not . isLit) fields)
    where
          -- Do *not* emit literal values.  These are encoded in the DefAbbrevRecord
          -- and therefore should not be re-emitted in the abbreviated record.  Yet
          -- parsing will put them into the AbbreviatedRecord, such that the DefAbbrevRecord
          -- is not needed when trying to make sense of the AbbrevRecord.
          isLit :: Field -> Bool
          isLit (W64 _) = True
          isLit _       = False

