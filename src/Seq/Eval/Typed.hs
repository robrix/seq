{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Seq.Eval.Typed
( evalEval
, Eval(..)
) where

import Control.Monad (ap)

evalEval :: Eval a -> a
evalEval (Eval r) = r id

newtype Eval a = Eval { eval :: forall r . (a -> r) -> r }
  deriving (Functor)

instance Applicative Eval where
  pure a = Eval (\ k -> k a)
  (<*>) = ap

instance Monad Eval where
  Eval m >>= f = Eval (\ k -> m (\ a -> eval (f a) k))
