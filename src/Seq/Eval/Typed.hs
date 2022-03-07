{-# LANGUAGE DeriveFunctor #-}
module Seq.Eval.Typed
( Eval(..)
) where

newtype Eval a = Eval { eval :: a }
  deriving (Functor)
