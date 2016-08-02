{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.BitCode where

import Data.Word  (Word32, Word64)
import Data.Maybe (catMaybes)
import Data.Bits (FiniteBits, finiteBitSize, countLeadingZeros)


--- Bit Codes ------------------------------------------------------------------
-- see BitCodes.h (e.g. http://llvm.org/docs/doxygen/html/BitCodes_8h_source.html)
-- * Bits
type Bit = Bool
type Bits = [Bool]

-- * BitCode
type BlockId = Int
type Code    = Int

-- * Source location
type Loc = (Int, Int) -- Words, Bits

-- | Bit Code data values can be 64bit wide.
type Val = Word64
data EncVal = Fixed !Val   -- code 1 fixed value
            | VBR   !Val   -- code 2 vbr value
            | Arr          -- code 3 Array -- the documentation sais, an Array needs to be followed by an op.
                                           -- when reading an array, the first is a vbr6 field indicating the length.
            | Char6        -- code 4 6-bit char
            | Blob         -- code 5 note: the value for this is: [vbr6:val,pad32bit,8bit array,pad32bit]
            deriving Show

-- | Operators for abbreviated records, are encoded as either literal (1) or encoded value (0).
data Op = Lit !Val          -- [1,vbr8:val]
        | Enc !EncVal       -- [0,f3:enc(,vbr5:val)?], vbr5 value only if given.
        deriving Show

-- | The Fields contained in an abbreviated record can be one of the following.
data Field = Vbr !Int !Val
           | Fix !Int !Val
           | Len !Val
           | Chr !Char
           | W64 !Val         -- Literal values. These are not bein emitted.
                              -- WARN: this is somewhat a hack, to make parsing and writing identical to id,
                              --       without having to track abbreviations in the writer and ensure the
                              --       abbreviated record matches the def abbrev. This could be considered
                              --       a TODO, as it would be an improvement to enforce the that AbbrevRecord
                              --       matches the actuall DefAbbrev.
           deriving Show

-- | Bit Code Data consists of a series of blocks. Their interpretation is dependent
-- on the container they are in.  The top level blocks are emitted with an abbreviation
-- width of 2. This allows the following four block types, which allow to define any
-- other set of blocks.
data BitCode
  -- | Combine ENTER_SUBBLOCK(1) with END_BLOCK(0)
  -- Layout: [1,vbr8:id,vbr4:newabbrevlen,<align32bits>,32bit:blocklen,<blocklen * words>,0,<align32bits>]
  -- 1 and 0 are vbr(current abbrev len); starting with 2 at the top level.
  = Block { blockId        :: !BlockId   -- ^ id
          , blockAbbrevLen :: !Int       -- ^ abbrev len
          , blockBody      :: ![BitCode] -- ^ body
          }
  -- | A abbreviation definition record. Layout: [2,vbr5:#ops,op0,op1,...]
  | DefAbbrevRecord { defRecordOps :: ![Op]
                    }
  -- | An unabbreviated record. Layout: [3,vbr6:code,vbr6:#ops,vbr6:op0,...]
  | UnabbrevRecord { uRecordCode :: !Val  -- ^ code         encoded vbr6
                   , uRecordOps :: ![Val] -- ^ generic ops, encoded vbr6
                   }
  -- | An abbreviated record. Layout [<abbrevcode>, fields, ...]
  | AbbrevRecord { aRecordCode   :: !Code
                 , aRecordFields :: ![Field]
                 }
  | Located { srcLoc :: (Loc, Loc), unLoc :: !BitCode }
  deriving Show

-- | BitCode contains some additional control information,
-- like abbreviation records, or the BLOCKINFO block, which
-- assist in decoding, but provide no information after
-- parsing the bitcode. Normalized bitcode is a simpler
-- structure consisting of only Blocks and Records.
--
-- Note: Normalized BitCode will erase location information.
data NBitCode
  = NBlock !BlockId ![NBitCode]
  | NRec   !Code    ![Val]
  deriving Show

idOrCode :: NBitCode -> Int
idOrCode (NBlock i _) = i
idOrCode (NRec i _) = i

normalize :: BitCode -> Maybe NBitCode
normalize (Block 0 _ _) = Nothing
normalize (Block id _ b) = Just (NBlock id (catMaybes . map normalize $ b))
normalize (DefAbbrevRecord{}) = Nothing
normalize (Located _ bs) = normalize bs
normalize (UnabbrevRecord c vs) = Just (NRec (fromIntegral c) vs)
normalize (AbbrevRecord _ flds) = let (code:ops) = map toVal . filter (not . isControl) $ flds
                                  in Just (NRec (fromIntegral code) ops)
  where
    -- As Abbreviated records can contain arrays, and
    -- arrays have thier length encoded in the field,
    -- Ops is anything but array length.
    --
    -- NOTE: This way we don't have to go back to the
    --       abbrev definition to figure out which
    --       ops are control ops and which are not.
    isControl :: Field -> Bool
    isControl (Len _) = True
    isControl _       = False

    toVal :: Field -> Val
    toVal (Vbr _ n) = n
    toVal (Fix _ n) = n
    toVal (Len _)   = error "Len is a control op"
    toVal (Chr c)   = fromIntegral . fromEnum $ c
    toVal (W64 v)   = v

bitWidth :: (FiniteBits a) => a -> Int
bitWidth x = finiteBitSize x - countLeadingZeros x

-- | Extract the id or the code for a BitCode element
denormalize :: NBitCode -> BitCode
denormalize (NBlock id bs) = let bs' = map denormalize bs
                                 ids = map idOrCode bs
                                 abbrevWidth = if ids == []
                                               then 0
                                               else max 2 (bitWidth (maximum ids))
                             in Block id abbrevWidth (map denormalize bs)
denormalize (NRec c vs) = UnabbrevRecord (fromIntegral c) vs

records :: (Enum a) => [NBitCode] -> [(a, [Val])]
records bs = [(toEnum c, vs) | NRec c vs <- bs]
blocks  :: (Enum a) => [NBitCode] -> [(a,[NBitCode])]
blocks bs = [(toEnum c, bs') | NBlock c bs' <- bs]

lookupBlock :: (Enum a) => a -> [NBitCode] -> Maybe [NBitCode]
lookupBlock e bs = lookup (fromEnum e) [(c,b) | NBlock c b <- bs]

lookupRecord :: (Enum a) => a -> [NBitCode] -> Maybe [Val]
lookupRecord e bs = lookup (fromEnum e) [(c,v) | NRec c v <- bs]

--------------------------------------------------------------------------------
-- Turn things into Val's for use in records
class ToVal a where
  toVal :: a -> [Val]

instance {-# OVERLAPPABLE #-} (Enum a) => ToVal a where
  toVal = pure . fromIntegral . fromEnum

instance {-# OVERLAPPING #-} (ToVal a) => ToVal [a] where
  toVal = concatMap toVal

--------------------------------------------------------------------------------
-- NBitCode construction
mkBlock :: (Enum a) => a -> [NBitCode] -> NBitCode
mkBlock e = NBlock (fromEnum e)

mkRec :: (Enum a, ToVal b) => a -> b -> NBitCode
mkRec e = NRec (fromEnum e) . toVal

mkEmptyRec :: (Enum a) => a -> NBitCode
mkEmptyRec e = NRec (fromEnum e) []
