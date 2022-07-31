{-# LANGUAGE MultiParamTypeClasses #-}
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
  -- * Rules
, V(..)
, K(..)
, C(..)
) where

import qualified Seq.Class as SQ

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


-- Rules

newtype V r a = V { getV :: Value }
newtype K r a = K { getK :: Continuation }
newtype C r = C { getC :: Command }

instance SQ.Mu V K C where
  µR f = V (Mu   (getC . f . K))
  µL f = K (Comu (getC . f . V))

instance SQ.Command V K C where
  V v .|. K k = C (v :|: k) -- FIXME: this is wrong; it needs to normalize
