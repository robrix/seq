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
  | MuR (Continuation -> Command)
  | FunR (Value -> Continuation -> Command)
  | CoprdR1 Value
  | CoprdR2 Value
  | PrdR (Continuation -> Command) (Continuation -> Command)
  | PairR !Value !Value
  | CopairR (Continuation -> Continuation -> Command)


-- Continuations

data Continuation
  = Covar Level
  | MuL (Value -> Command)
  | CoprdL (Value -> Command) (Value -> Command)
  | PrdL1 (Value -> Command)
  | PrdL2 (Value -> Command)
  | PairL (Value -> Value -> Command)
  | CopairL Continuation Continuation
  | FunL Value Continuation


-- Commands

data Command
  = Value :|: Continuation

infix 2 :|:


-- Rules

newtype V r a = V { getV :: Value }
newtype K r a = K { getK :: Continuation }
newtype C r = C { getC :: Command }

instance SQ.Mu V K C where
  µR f = V (MuR (getC . f . K))
  µL f = K (MuL (getC . f . V))

instance SQ.Command V K C where
  V v .|. K k = C (v :|: k) -- FIXME: this is wrong; it needs to normalize


-- Positive

instance SQ.Coprd V K C where
  coprdR1 = V . CoprdR1 . getV
  coprdR2 = V . CoprdR2 . getV
  coprdL l r = K (CoprdL (getC . l . V) (getC . r . V))

instance SQ.Pair V K C where
  pairR (V a) (V b) = V (PairR a b)
  pairL f = K (PairL (\ a b -> getC (f (V a) (V b))))


-- Negative

instance SQ.Prd V K C where
  prdR l r = V (PrdR (getC . l . K) (getC . r . K))
  prdL1 = K . PrdL1 . flip (:|:) . getK
  prdL2 = K . PrdL2 . flip (:|:) . getK

instance SQ.Copair V K C where
  copairR f = V (CopairR (\ l r -> getC (f (K l) (K r))))
  copairL (K l) (K r) = K (CopairL l r)

instance SQ.Fun V K C where
  funR f = V (FunR (\ v k -> getC (f (V v) (K k))))
  funL (V v) (K k) = K (FunL v k)
