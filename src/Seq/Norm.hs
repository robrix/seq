module Seq.Norm
( -- * De Bruijn names
  Level(..)
, Index(..)
  -- * Values
, Value(..)
  -- * Continuations
, Continuation(..)
  -- * Commands
, Command(..)
) where

-- De Bruijn names

newtype Level = Level { getLevel :: Int }
newtype Index = Index { getIndex :: Int }


-- Values

data Value
  = Var Level
  | Mu (Continuation -> Command)


-- Continuations

data Continuation
  = Covar Level
  | Comu (Value -> Command)


-- Commands

data Command
  = Value :|: Continuation
