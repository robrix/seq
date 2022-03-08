{-# LANGUAGE DeriveFunctor #-}
module Seq.Eval.Typed
( evalTerm
, Term(..)
, Coterm(..)
) where

import Control.Monad (ap)
import Data.Functor.Contravariant (Contravariant(..))

evalTerm :: Term a a -> a
evalTerm (Term r) = r id

newtype Term r a = Term { eval :: (a -> r) -> r }
  deriving (Functor)

instance Applicative (Term r) where
  pure a = Term (\ k -> k a)
  (<*>) = ap

instance Monad (Term r) where
  Term m >>= f = Term (\ k -> m (\ a -> eval (f a) k))


newtype Coterm r a = Coterm { coeval :: Term r a -> r }

instance Contravariant (Coterm r) where
  contramap f (Coterm r) = Coterm (r . fmap f)
