{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Seq.Eval.Typed
( Eval(..)
) where

import Control.Monad (ap)

newtype Eval a = Eval { eval :: forall r . (a -> r) -> r }
  deriving (Functor)

instance Applicative Eval where
  pure a = Eval (\ k -> k a)
  (<*>) = ap

instance Monad Eval where
  Eval m >>= f = Eval (\ k -> m (\ a -> eval (f a) k))
