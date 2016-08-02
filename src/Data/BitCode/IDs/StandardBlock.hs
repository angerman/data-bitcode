module Data.BitCode.IDs.StandardBlock where
-- | StandardBlockIDs - All bitcode files can optionally include a BLOCKINFO
-- block, which contains metadata about other blocks in the file.
data StandardBlock
  -- | BLOCKINFO_BLOCK (0) is used to define metadata about blocks, for example,
  -- standard abbrevs that should be available to all blocks of a specified
  -- ID.
  = BLOCKINFO
  -- | Block IDs 1-7 are reserved for future expansion.
  | RESERVED_1 | RESERVED_2 | RESERVED_3 | RESERVED_4 | RESERVED_5 | RESERVED_6 | RESERVED_7
  -- | This is the marker for the first application block id (8).
  | FIRST_APPLICATION_BLOCKID
  deriving (Show, Enum)
