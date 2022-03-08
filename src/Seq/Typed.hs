{-# LANGUAGE FunctionalDependencies #-}
module Seq.Typed
( Seq(..)
, Not(..)
, Fun(..)
, fun
) where

import Data.Functor.Contravariant (Contravariant(..))
import Data.Void

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  µR :: (coterm a -> command) -> term a
  prdR :: term a -> term b -> term (a, b)
  sumR1 :: term a -> term (Either a b)
  sumR2 :: term b -> term (Either a b)
  funR :: (term a -> coterm b -> command) -> term (a -> b)

  -- left rules
  µL :: (term a -> command) -> coterm a
  prdL1 :: coterm a -> coterm (a, b)
  prdL2 :: coterm b -> coterm (a, b)
  sumL :: coterm a -> coterm b -> coterm (Either a b)
  funL :: term a -> coterm b -> coterm (a -> b)

  -- commands
  (.|.) :: term a -> coterm a -> command

  infix 1 .|.


newtype Not a = Not { runNot :: a -> Void }

instance Contravariant Not where
  contramap f (Not r) = Not (r . f)


newtype Fun a b = Fun (Not b -> Not a)

fun :: (a -> b) -> Fun a b
fun f = Fun (\ kb -> Not (runNot kb . f))
