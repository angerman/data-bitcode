module Data.BitCode.IDs.FixedAbbrev where

-- | The standard abbrev namespace always has a way to exit a block, enter a
-- nested block, define abbrevs, and define an unabbreviated record.
data FixedAbbrev
  -- | Must be zero to guarantee termination for broken bitcode.
  = END_BLOCK
  | ENTER_SUBBLOCK
  -- | DEFINE_ABBREV - Defines an abbrev for the current block.  It consists
  -- of a vbr5 for # operand infos.  Each operand info is emitted with a
  -- single bit to indicate if it is a literal encoding.  If so, the value is
  -- emitted with a vbr8.  If not, the encoding is emitted as 3 bits followed
  -- by the info value as a vbr5 if needed.
  | DEFINE_ABBREV
  -- | UNABBREV_RECORDs are emitted with a vbr6 for the record code, followed by
  -- a vbr6 for the # operands, followed by vbr6's for each operand.
  | UNABBREV_RECORD
  -- | This is not a code, this is a marker for the first abbrev assignment.
  | FIRST_APPLICATION_ABBREV
  deriving (Show, Enum)
