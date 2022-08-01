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
  | InL Value
  | InR Value
  | InLR (Continuation -> Command) (Continuation -> Command)
  | Pair !Value !Value
  | CopairR (Continuation -> Continuation -> Command)


-- Continuations

data Continuation
  = Covar Level
  | Comu (Value -> Command)
  | Case (Value -> Command) (Value -> Command)
  | PrjL (Value -> Command)
  | PrjR (Value -> Command)
  | PrjLR (Value -> Value -> Command)
  | CopairL Continuation Continuation
  | Value :$ Continuation

infixr 9 :$


-- Commands

data Command
  = Value :|: Continuation

infix 2 :|:


-- Rules

newtype V r a = V { getV :: Value }
newtype K r a = K { getK :: Continuation }
newtype C r = C { getC :: Command }

instance SQ.Mu V K C where
  µR f = V (Mu   (getC . f . K))
  µL f = K (Comu (getC . f . V))

instance SQ.Command V K C where
  V v .|. K k = C (v :|: k) -- FIXME: this is wrong; it needs to normalize


-- Positive

instance SQ.Coprd V K C where
  coprdR1 = V . InL . getV
  coprdR2 = V . InR . getV
  coprdL l r = K (Case (getC . l . V) (getC . r . V))

instance SQ.Pair V K C where
  pairR (V a) (V b) = V (Pair a b)
  pairL f = K (PrjLR (\ a b -> getC (f (V a) (V b))))


-- Negative

instance SQ.Prd V K C where
  prdR l r = V (InLR (getC . l . K) (getC . r . K))
  prdL1 = K . PrjL . flip (:|:) . getK
  prdL2 = K . PrjR . flip (:|:) . getK

instance SQ.Copair V K C where
  copairR f = V (CopairR (\ l r -> getC (f (K l) (K r))))
  copairL (K l) (K r) = K (CopairL l r)
