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
  | Lam (Value -> Continuation -> Command)


-- Continuations

data Continuation
  = Covar Level
  | Comu (Value -> Command)
  | Value :$ Continuation

infixr 9 :$


-- Commands

data Command
  = Value :|: Continuation
