{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
module Seq.Class
( Mu(..)
, Term(..)
, Coterm(..)
, Command(..)
, (|>)
, T.Fun(..)
, T.Cofun(..)
, T.Not(..)
, T.Prd(..)
, T.πL
, T.πR
, T.Coprd(..)
, T.Pair(..)
, T.Copair(..)
, T.inL
, T.inR
, T.exlr
  -- * Library
, identity
, constant
  -- * Proofs
, funE
) where

import qualified Seq.Types as T

class Mu term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  µR :: (coterm r a -> command r) -> term r a
  µL :: (term r a -> command r) -> coterm r a

class Term term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  prdR :: term r a -> term r b -> term r (T.Prd r a b)
  coprdR1 :: term r a -> term r (T.Coprd a b)
  coprdR2 :: term r b -> term r (T.Coprd a b)
  pairR :: term r a -> term r b -> term r (T.Pair a b)
  copairR :: Either (term r a) (term r b) -> term r (T.Copair r a b)
  notR :: coterm r a -> term r (T.Not r a)
  funR :: (term r a -> coterm r b -> command r) -> term r (T.Fun r a b)
  cofunR :: term r a -> coterm r b -> term r (T.Cofun r a b)

class Coterm term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  prdL1 :: coterm r a -> coterm r (T.Prd r a b)
  prdL2 :: coterm r b -> coterm r (T.Prd r a b)
  coprdL :: coterm r a -> coterm r b -> coterm r (T.Coprd a b)
  pairL :: (term r a -> term r b -> command r) -> coterm r (T.Pair a b)
  copairL :: coterm r a -> coterm r b -> coterm r (T.Copair r a b)
  notL :: term r a -> coterm r (T.Not r a)
  funL :: term r a -> coterm r b -> coterm r (T.Fun r a b)
  cofunL :: (term r a -> coterm r b -> command r) -> coterm r (T.Cofun r a b)

class Command term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  (.|.) :: term r a -> coterm r a -> command r

  infix 1 .|.


-- | An infix synonym for 'funL'.
(|>) :: Coterm term coterm command => term r a -> coterm r b -> coterm r (T.Fun r a b)
(|>) = funL

infixr 9 |>


-- Library

identity :: (Term t c d, Command t c d) => t r (T.Fun r a a)
identity = funR (.|.)

constant :: (Term t c d, Command t c d) => t r (T.Fun r a (T.Fun r b a))
constant = funR $ \ a k -> funR (\ _ k -> a .|. k) .|. k


-- Proofs

funE :: (Mu term coterm command, Coterm term coterm command, Command term coterm command) => term r (T.Fun r a b) -> term r a -> term r b
funE f a = µR (\ k -> f .|. a |> k)
