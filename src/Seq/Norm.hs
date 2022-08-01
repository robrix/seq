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
  -- Positive
  | CoprdR1 Value
  | CoprdR2 Value
  | PairR !Value !Value
  | CofunR Value Continuation
  | OneR
  | NegateR Continuation
  -- Negative
  | PrdR (Continuation -> Command) (Continuation -> Command)
  | CopairR (Continuation -> Continuation -> Command)
  | FunR (Value -> Continuation -> Command)
  | TopR
  | BottomR Command
  | NotR (Value -> Command)


-- Continuations

data Continuation
  = Covar Level
  | MuL (Value -> Command)
  -- Positive
  | CoprdL (Value -> Command) (Value -> Command)
  | PairL (Value -> Value -> Command)
  | CofunL (Value -> Continuation -> Command)
  | ZeroL
  | OneL Command
  | NegateL (Continuation -> Command)
  -- Negative
  | PrdL1 Continuation
  | PrdL2 Continuation
  | CopairL Continuation Continuation
  | FunL Value Continuation
  | BottomL
  | NotL Value


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

instance SQ.Cofun V K C where
  cofunR (V v) (K k) = V (CofunR v k)
  cofunL f = K (CofunL (\ v k -> getC (f (V v) (K k))))

instance SQ.Zero K where
  zeroL = K ZeroL

instance SQ.One V K C where
  oneR = V OneR
  oneL = K . OneL . getC

instance SQ.Negate V K C where
  negateR (K k) = V (NegateR k)
  negateL f = K (NegateL (getC . f . K))


-- Negative

instance SQ.Prd V K C where
  prdR l r = V (PrdR (getC . l . K) (getC . r . K))
  prdL1 = K . PrdL1 . getK
  prdL2 = K . PrdL2 . getK

instance SQ.Copair V K C where
  copairR f = V (CopairR (\ l r -> getC (f (K l) (K r))))
  copairL (K l) (K r) = K (CopairL l r)

instance SQ.Fun V K C where
  funR f = V (FunR (\ v k -> getC (f (V v) (K k))))
  funL (V v) (K k) = K (FunL v k)

instance SQ.Top V where
  topR = V TopR

instance SQ.Bottom V K C where
  bottomR = V . BottomR . getC
  bottomL = K BottomL

instance SQ.Not V K C where
  notR f = V (NotR (getC . f . V))
  notL (V v) = K (NotL v)
