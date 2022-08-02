{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Name
( -- * De Bruijn names
  Level(..)
, Index(..)
, toIndexed
, toLeveled
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


toIndexed :: Level -> Level -> Index
toIndexed (Level d) (Level level) = Index $ d - level - 1

toLeveled :: Level -> Index -> Level
toLeveled (Level d) (Index index) = Level $ d - index - 1


-- Names

type Name = String


-- Variables

data Var a
  = Bound a
  | Free Name
