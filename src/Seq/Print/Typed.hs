{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Seq.Print.Typed
( Prec(..)
, Print(..)
) where

import qualified Seq.Print.Untyped as U
import           Seq.Typed
import qualified Seq.Untyped as U

newtype Prec = Prec Int

newtype Print r a = Print { getPrint :: U.Print }
  deriving (Monoid, Semigroup, Show)

instance Seq Print Print (Print ()) where
  µR f = Print (U.µR (getPrint . f . Print))
  withR (Print a) (Print b) = Print (U.withR a b)
  sumR1 (Print a) = Print (U.sumR1 a)
  sumR2 (Print b) = Print (U.sumR2 b)
  funR f = Print (U.funR (\ t c -> getPrint (f (Print t) (Print c))))

  µL f = Print (U.µL (getPrint . f . Print))
  withL1 (Print a) = Print (U.withL1 a)
  withL2 (Print b) = Print (U.withL1 b)
  sumL (Print a) (Print b) = Print (U.sumL a b)
  funL (Print t) (Print c) = Print (U.funL t c)

  Print t .|. Print c = Print (t U..|. c)
