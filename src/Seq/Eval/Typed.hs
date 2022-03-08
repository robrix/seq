{-# LANGUAGE DeriveFunctor #-}
module Seq.Eval.Typed
( evalTerm
, Term(..)
) where

import Control.Monad (ap)

evalTerm :: Term a a -> a
evalTerm (Term r) = r id

newtype Term r a = Term { eval :: (a -> r) -> r }
  deriving (Functor)

instance Applicative (Term r) where
  pure a = Term (\ k -> k a)
  (<*>) = ap

instance Monad (Term r) where
  Term m >>= f = Term (\ k -> m (\ a -> eval (f a) k))
