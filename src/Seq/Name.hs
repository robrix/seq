{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  deriving (Enum, Eq, Num, Ord, Show)

newtype Index = Index { getIndex :: Int }
  deriving (Enum, Eq, Num, Ord, Show)


-- Names

type Name = String


-- Variables

data Var a
  = Bound a
  | Free Name
