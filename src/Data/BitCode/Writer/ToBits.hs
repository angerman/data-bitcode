{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE TupleSections, RecursiveDo, FlexibleInstances #-}
module Data.BitCode.Writer.ToBits
  (module Data.BitCode.Writer.Monad
  ,ToBits(..))
where

import Prelude hiding (length, fromEnum)
import qualified Prelude as P
import Data.Word (Word64)

import Data.BitCode.Writer.Monad

import Data.BitCode hiding (toEnum, fromEnum)

import Data.BitCode.IDs.FixedAbbrev

import GHC.Stack (HasCallStack)


length :: [a] -> Word64
length = fromIntegral . P.length
fromEnum :: Enum a => a -> Word64
fromEnum = fromIntegral . P.fromEnum

class ToBits a where
  emit :: HasCallStack => a -> Bitstream ()

instance ToBits EncVal where
  emit (Fixed v) = emitFixed 3 1 >> emitVBR 5 v
  emit (VBR   v) = emitFixed 3 2 >> emitVBR 5 v
  emit Arr       = emitFixed 3 3
  emit Char6     = emitFixed 3 4
  emit Blob      = emitFixed 3 5

instance ToBits Op where
  emit (Lit v)   = emitBit True >> emit (W64 v)
  emit (Enc e)   = emitBit False >> emit e

instance ToBits [Op] where
  emit = mapM_ emit

instance ToBits Field where
  emit (Vbr n v) = emitVBR n v
  emit (Fix n v) = emitFixed n v
  emit (Chr c)   = emitChar6 c
  emit (Len n)   = emitVBR 6 n
  emit (W64 w)   = emitVBR 8 w

instance ToBits (Word64, BitCode) where
  emit (width, (Located _ block))   = emit (width, block)
  emit (width, (Block id len body)) = mdo
    emitFixed width $ fromEnum ENTER_SUBBLOCK
    emitVBR 8 id
    emitVBR 4 len
    alignWord32
       -- note: putSized, unaligned may be off by one.
       --       as the body may not be multiple of 32
       --       bits.
    emitWord32 n
    n <- withOffset 0 $ do
      mapM_ (emit . (len,)) body
      emitVBR len $ fromEnum END_BLOCK
      alignWord32
      locWords
    pure ()

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
