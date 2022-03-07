{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Print.Typed
( Prec(..)
, Print(..)
) where

import qualified Seq.Print.Untyped as U

newtype Prec = Prec Int

newtype Print a = Print { getPrint :: U.Print }
  deriving (Monoid, Semigroup)
