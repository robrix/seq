{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Seq.Eval.Typed
( Term(..)
, Coterm(..)
, Command(..)
) where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Coerce (coerce)
import Data.Functor.Contravariant (Contravariant(..))
import Seq.Typed

newtype Term r a = Term { eval :: (a -> r) -> r }

instance Functor (Term r) where
  fmap f (Term r) = Term (r . (. f))

instance Applicative (Term r) where
  pure a = Term (\ k -> k a)
  (<*>) = ap

instance Monad (Term r) where
  m >>= f = Term (\ k -> eval m (\ a -> eval (f a) k))


newtype Coterm r a = Coterm { coeval :: a -> r }

instance Contravariant (Coterm r) where
  contramap f (Coterm r) = Coterm (r . f)


newtype Command r = Command { runCommand :: r }
  deriving (Functor)

instance Applicative Command where
  pure = Command
  (<*>) = coerce

instance Monad Command where
  a >>= f = coerce f a

instance Seq Term Coterm Command where
  µR f = Term (\ k -> runCommand (f (Coterm k)))
  withR = liftA2 (,)
  sumR1 = fmap Left
  sumR2 = fmap Right
  funR f = Term (\ k -> k (Fun (\ kb a -> runCommand (f (pure a) (Coterm kb)))))

  µL f = Coterm (runCommand . f . pure)
  withL1 = contramap fst
  withL2 = contramap snd
  sumL p q = Coterm (either (coeval p) (coeval q))
  funL a b = Coterm (\ f -> eval a (app f (coeval b)))

  t .|. c = Command (eval t (coeval c))
