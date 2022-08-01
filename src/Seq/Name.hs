module Seq.Name
( -- * De Bruijn names
  Level(..)
, Index(..)
  -- * Names
, Name
  -- * Variables
, Var(..)
) where

-- De Bruijn names

newtype Level = Level { getLevel :: Int }
newtype Index = Index { getIndex :: Int }


-- Names

type Name = String


-- Variables

data Var a
  = Bound a
  | Free Name
