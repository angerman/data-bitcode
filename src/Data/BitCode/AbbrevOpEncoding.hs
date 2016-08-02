module Data.BitCode.AbbrevOpEncoding where

-- | BitCodeAbbrevOp - This describes one or more operands in an abbreviation.
-- This is actually a union of two different things:
--   1. It could be a literal integer value ("the operand is always 17").
--   2. It could be an encoding specification ("this operand encoded like so").
--
-- These are encoded as
data AbbrevOpEncoding
  -- | Placehodler for 0. Do not use.
  = Unused
  -- | A fixed width field, Val specifies number of bits.
  | Fixed
  -- | A VBR field where Val specifies the width of each chunk.
  | VBR
  -- | A sequence of fields, next field species elt encoding.
  | Array
  -- | A 6-bit fixed field which maps to [a-zA-Z0-9._].
  | Char6
  -- | 32-bit aligned array of 8-bit characters.
  | Blob
  deriving (Show, Enum)
