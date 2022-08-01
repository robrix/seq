module Seq.Name
( -- * De Bruijn names
  Level(..)
, Index(..)
) where

-- De Bruijn names

newtype Level = Level { getLevel :: Int }
newtype Index = Index { getIndex :: Int }
