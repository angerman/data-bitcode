module Data.BitCode.Abbreviation
  ( addAbbrev, lookupAbbrev
  , addGlobalAbbrev, lookupGlobalAbbrev
  , AbbrevMap
  , GlobalAbbrevMap
  )
where

import Data.BitCode
import Data.Maybe (fromMaybe)

newtype AbbrevMap = AbbrevMap [(Code, BitCode)] deriving Show
newtype GlobalAbbrevMap = GlobalAbbrevMap [(BlockId, AbbrevMap)] deriving Show

instance Monoid AbbrevMap where
  mempty = AbbrevMap []
  (AbbrevMap m) `mappend` (AbbrevMap n) = AbbrevMap (m ++ n)

instance Monoid GlobalAbbrevMap where
  mempty = GlobalAbbrevMap []
  (GlobalAbbrevMap m) `mappend` (GlobalAbbrevMap n) = GlobalAbbrevMap (m ++ n)

lookupGlobalAbbrev :: GlobalAbbrevMap -> BlockId -> AbbrevMap
lookupGlobalAbbrev (GlobalAbbrevMap g) blockId = fromMaybe mempty (lookup blockId g)

addGlobalAbbrev :: GlobalAbbrevMap -> BlockId -> BitCode -> GlobalAbbrevMap
addGlobalAbbrev (GlobalAbbrevMap g) blockId block = GlobalAbbrevMap g'
  where g' = go g blockId block
        go :: [(BlockId, AbbrevMap)] -> BlockId -> BitCode -> [(BlockId, AbbrevMap)]
        go [] id b = [(blockId, addAbbrev mempty block)]
        go (gb@(id', bs):g') id block | id == id' = (id, addAbbrev bs block):go g' id block
                                      | otherwise = gb:go g' id block

lookupAbbrev :: AbbrevMap -> Code -> Maybe BitCode
lookupAbbrev (AbbrevMap m) = flip lookup m

addAbbrev :: AbbrevMap -> BitCode -> AbbrevMap
addAbbrev (AbbrevMap m) r@(DefAbbrevRecord ops) = AbbrevMap $ (nextId,r):m
  where nextId = 1 + foldr max 3 (map fst m)

