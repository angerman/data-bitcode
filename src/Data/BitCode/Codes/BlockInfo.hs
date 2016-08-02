module Data.BitCode.Codes.BlockInfo where

-- | BlockInfoCodes - The blockinfo block contains metadata about user-defined
-- blocks.
--
-- DEFINE_ABBREV has magic semantics here, applying to the current SETBID'd
-- block, instead of the BlockInfo block.
data BlockInfo
  -- | Placeholder for 0. Do not use.
  = BLOCKINFO_CODE_UNDEFINED
  -- | SETBID: [blockid#]
  | BLOCKINFO_CODE_SETBID
  -- | BLOCKNAME: [name]
  | BLOCKINFO_CODE_BLOCKNAME
  -- | BLOCKINFO_CODE_SETRECORDNAME: [id, name]
  | BLOCKINFO_CODE_SETRECORDNAME
  deriving (Show, Enum)
