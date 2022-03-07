{-# LANGUAGE DeriveFunctor #-}
module Seq.Eval.Typed
( Eval(..)
) where
import Data.Coerce (coerce)

newtype Eval a = Eval { eval :: a }
  deriving (Functor)

instance Applicative Eval where
  pure = Eval
  (<*>) = coerce
