{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Seq.Eval.Typed
( evalTerm
, Term(..)
, coeval
, coterm
, Coterm(..)
, Command(..)
) where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Functor.Contravariant (Contravariant(..))
import Seq.Typed

evalTerm :: Term a a -> a
evalTerm (Term r) = r id

newtype Term r a = Term { eval :: (a -> r) -> r }

instance Functor (Term r) where
  fmap f (Term r) = Term (r . (. f))

instance Applicative (Term r) where
  pure a = Term (\ k -> k a)
  (<*>) = ap

instance Monad (Term r) where
  m >>= f = Term (\ k -> eval m (\ a -> eval (f a) k))


coeval :: Coterm r a -> (a -> r)
coeval (Coterm r) = runNot r

coterm :: (a -> r) -> Coterm r a
coterm f = Coterm (Not f)

newtype Coterm r a = Coterm (Not r a)

instance Contravariant (Coterm r) where
  contramap f (Coterm r) = Coterm (contramap f r)


newtype Command r = Command { runCommand :: r }
  deriving (Functor)


instance Seq Term Coterm Command where
  µR f = Term (\ k -> runCommand (f (coterm k)))
  prdR = liftA2 (,)
  sumR1 = fmap Left
  sumR2 = fmap Right
  funR f = Term (\ k -> k (coapp (\ a kb -> runCommand (f (pure a) (coterm kb)))))

  µL f = coterm (runCommand . f . pure)
  prdL1 = contramap fst
  prdL2 = contramap snd
  sumL p q = coterm (either (coeval p) (coeval q))
  funL a b = coterm (\ f -> eval a (\ a -> app f a (coeval b)))

  t .|. c = Command (eval t (coeval c))
