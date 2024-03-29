{-# LANGUAGE MultiParamTypeClasses #-}
module Seq.Untyped.Norm
( -- * Values
  Value(..)
  -- * Continuations
, Continuation(..)
  -- * Commands
, Command(..)
  -- * Rules
, V(..)
, K(..)
, C(..)
) where

import qualified Seq.Class as Typed
import           Seq.Name
import qualified Seq.Untyped.Class as Untyped

-- Values

data Value
  = VarR Level
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
  = VarL Level
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

instance Typed.Mu V K C where
  µR f = V (MuR (getC . f . K))
  µL f = K (MuL (getC . f . V))

instance Typed.Command V K C where
  V v .|. K k = C (v :|: k) -- FIXME: this is wrong; it needs to normalize


instance Untyped.Mu Value Continuation Command where
  µR = MuR
  µL = MuL

instance Untyped.Command Value Continuation Command where
  (.|.) = (:|:) -- FIXME: this is wrong; it needs to normalize


-- Positive

instance Typed.Coprd V K C where
  coprdR1 = V . CoprdR1 . getV
  coprdR2 = V . CoprdR2 . getV
  coprdL l r = K (CoprdL (getC . l . V) (getC . r . V))

instance Typed.Pair V K C where
  pairR (V a) (V b) = V (PairR a b)
  pairL f = K (PairL (\ a b -> getC (f (V a) (V b))))

instance Typed.Cofun V K C where
  cofunR (V v) (K k) = V (CofunR v k)
  cofunL f = K (CofunL (\ v k -> getC (f (V v) (K k))))

instance Typed.Zero K where
  zeroL = K ZeroL

instance Typed.One V K C where
  oneR = V OneR
  oneL = K . OneL . getC

instance Typed.Negate V K C where
  negateR (K k) = V (NegateR k)
  negateL f = K (NegateL (getC . f . K))


instance Untyped.Coprd Value Continuation Command where
  coprdR1 = CoprdR1
  coprdR2 = CoprdR2
  coprdL = CoprdL

instance Untyped.Pair Value Continuation Command where
  pairR = PairR
  pairL = PairL

instance Untyped.Cofun Value Continuation Command where
  cofunR = CofunR
  cofunL = CofunL

instance Untyped.Zero Continuation where
  zeroL = ZeroL

instance Untyped.One Value Continuation Command where
  oneR = OneR
  oneL = OneL

instance Untyped.Negate Value Continuation Command where
  negateR = NegateR
  negateL = NegateL


-- Negative

instance Typed.Prd V K C where
  prdR l r = V (PrdR (getC . l . K) (getC . r . K))
  prdL1 = K . PrdL1 . getK
  prdL2 = K . PrdL2 . getK

instance Typed.Copair V K C where
  copairR f = V (CopairR (\ l r -> getC (f (K l) (K r))))
  copairL (K l) (K r) = K (CopairL l r)

instance Typed.Fun V K C where
  funR f = V (FunR (\ v k -> getC (f (V v) (K k))))
  funL (V v) (K k) = K (FunL v k)

instance Typed.Top V where
  topR = V TopR

instance Typed.Bottom V K C where
  bottomR = V . BottomR . getC
  bottomL = K BottomL

instance Typed.Not V K C where
  notR f = V (NotR (getC . f . V))
  notL (V v) = K (NotL v)


instance Untyped.Prd Value Continuation Command where
  prdR = PrdR
  prdL1 = PrdL1
  prdL2 = PrdL2

instance Untyped.Copair Value Continuation Command where
  copairR = CopairR
  copairL = CopairL

instance Untyped.Fun Value Continuation Command where
  funR = FunR
  funL = FunL

instance Untyped.Top Value where
  topR = TopR

instance Untyped.Bottom Value Continuation Command where
  bottomR = BottomR
  bottomL = BottomL

instance Untyped.Not Value Continuation Command where
  notR = NotR
  notL = NotL
