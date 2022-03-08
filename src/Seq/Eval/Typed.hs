{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Seq.Eval.Typed
( evalEval
, Term(..)
) where

import Control.Monad (ap)

evalEval :: Term a -> a
evalEval (Term r) = r id

newtype Term a = Term { eval :: forall r . (a -> r) -> r }
  deriving (Functor)

instance Applicative Term where
  pure a = Term (\ k -> k a)
  (<*>) = ap

instance Monad Term where
  Term m >>= f = Term (\ k -> m (\ a -> eval (f a) k))
