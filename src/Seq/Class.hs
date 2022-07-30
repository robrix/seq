{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Seq.Class
( Seq
, Mu(..)
, Term(..)
, Coterm(..)
, Command(..)
, (|>)
, Fun(..)
, Cofun(..)
, Not(..)
, Prd(..)
, πL
, πR
, Coprd(..)
, Pair(..)
, Copair(..)
, inL
, inR
, exlr
  -- * Library
, identity
, constant
  -- * Proofs
, funE
) where

type Seq term coterm command = (Term term coterm command, Coterm term coterm command, Command term coterm command)

class Mu term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  µR :: (coterm r a -> command r) -> term r a
  µL :: (term r a -> command r) -> coterm r a

class Term term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  prdR :: term r a -> term r b -> term r (Prd r a b)
  coprdR1 :: term r a -> term r (Coprd a b)
  coprdR2 :: term r b -> term r (Coprd a b)
  pairR :: term r a -> term r b -> term r (Pair a b)
  copairR :: Either (term r a) (term r b) -> term r (Copair r a b)
  notR :: coterm r a -> term r (Not r a)
  funR :: (term r a -> coterm r b -> command r) -> term r (Fun r a b)
  cofunR :: term r a -> coterm r b -> term r (Cofun r a b)

class Coterm term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  prdL1 :: coterm r a -> coterm r (Prd r a b)
  prdL2 :: coterm r b -> coterm r (Prd r a b)
  coprdL :: coterm r a -> coterm r b -> coterm r (Coprd a b)
  pairL :: (term r a -> term r b -> command r) -> coterm r (Pair a b)
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

newtype Prd r a b = Prd { prd :: (((a -> r) -> r) -> ((b -> r) -> r) -> r) -> r }

πL :: Prd r a b -> (a -> r) -> r
πL (Prd r) k = r (\ ka _ -> ka k)

πR :: Prd r a b -> (b -> r) -> r
πR (Prd r) k = r (\ _ kb -> kb k)

data Coprd a b
  = InL !a
  | InR !b

exlr :: (a -> r) -> (b -> r) -> Coprd a b -> r
exlr l r = \case
  InL a -> l a
  InR b -> r b

data Pair a b = Pair { pair1 :: !a, pair2 :: !b }

newtype Copair r a b = Copair { copair :: (a -> r) -> (b -> r) -> r }

inL :: ((a -> r) -> r) -> Copair r a b
inL a = Copair (\ f _ -> a f)

inR :: ((b -> r) -> r) -> Copair r a b
inR b = Copair (\ _ g -> b g)


-- Library

identity :: (Term t c d, Command t c d) => t r (Fun r a a)
identity = funR (.|.)

constant :: (Term t c d, Command t c d) => t r (Fun r a (Fun r b a))
constant = funR $ \ a k -> funR (\ _ k -> a .|. k) .|. k


-- Proofs

funE :: (Mu term coterm command, Coterm term coterm command, Command term coterm command) => term r (Fun r a b) -> term r a -> term r b
funE f a = µR (\ k -> f .|. a |> k)
