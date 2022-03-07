{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Print.Typed
( Prec(..)
, Print(..)
) where

import Seq.Doc

newtype Prec = Prec Int

newtype Print a = Print { getPrint :: Prec -> Doc }
  deriving (Monoid, Semigroup)
