{-# LANGUAGE FunctionalDependencies #-}
module Seq.Class
( Term(..)
, Coterm(..)
, Command(..)
, (|>)
, Fun(..)
, Cofun(..)
, Not(..)
, Pair(..)
, Copair(..)
  -- * Library
, identity
, constant
  -- * Proofs
, funE
) where

class Term term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  µR :: (coterm r a -> command r) -> term r a
  prdR :: term r a -> term r b -> term r (a, b)
  coprdR1 :: term r a -> term r (Either a b)
  coprdR2 :: term r b -> term r (Either a b)
  pairR :: term r a -> term r b -> term r (Pair r a b)
  copairR :: term r (Either a b) -> term r (Copair r a b)
  notR :: coterm r a -> term r (Not r a)
  funR :: (term r a -> coterm r b -> command r) -> term r (Fun r a b)
  cofunR :: term r a -> coterm r b -> term r (Cofun r a b)

class Coterm term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  µL :: (term r a -> command r) -> coterm r a
  prdL1 :: coterm r a -> coterm r (a, b)
  prdL2 :: coterm r b -> coterm r (a, b)
  coprdL :: coterm r a -> coterm r b -> coterm r (Either a b)
  pairL :: (term r a -> term r b -> command r) -> coterm r (Pair r a b)
  copairL :: coterm r a -> coterm r b -> coterm r (Copair r a b)
  notL :: term r a -> coterm r (Not r a)
  funL :: term r a -> coterm r b -> coterm r (Fun r a b)
  cofunL :: (term r a -> coterm r b -> command r) -> coterm r (Cofun r a b)

class Command term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  (.|.) :: term r a -> coterm r a -> command r

  infix 1 .|.


-- | An infix synonym for 'funL'.
(|>) :: Coterm term coterm command => term r a -> coterm r b -> coterm r (Fun r a b)
(|>) = funL

infixr 9 |>


newtype Fun r a b = Fun { app :: (b -> r) -> (a -> r) }

data Cofun r b a = (:>-) { coreturn :: a -> r, coconst :: b }

infix 1 :>-

newtype Not r a = Not { runNot :: a -> r }

newtype Pair r a b = Pair { pair :: (a -> b -> r) -> r }

newtype Copair r a b = Copair { copair :: (a -> r) -> (b -> r) -> r }


-- Library

identity :: (Term t c d, Command t c d) => t r (Fun r a a)
identity = funR (.|.)

constant :: (Term t c d, Command t c d) => t r (Fun r a (Fun r b a))
constant = funR $ \ a k -> funR (\ _ k -> a .|. k) .|. k


-- Proofs

funE :: (Term term coterm command, Coterm term coterm command, Command term coterm command) => term r (Fun r a b) -> term r a -> term r b
funE f a = µR (\ k -> f .|. a |> k)
