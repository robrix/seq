{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module Seq.Class
( -- * Rules
  Mu(..)
, Command(..)
  -- ** Positive
, Coprd(..)
, Pair(..)
, Negate(..)
, Cofun(..)
, Zero(..)
, One(..)
  -- ** Negative
, Prd(..)
, Copair(..)
, Not(..)
, Fun(..)
, Bottom(..)
, Top(..)
, (|>)
  -- * Library
, identity
, constant
  -- * Proofs
, funE
) where

import qualified Data.Kind as K
import qualified Seq.Types as T

-- Rules

class Mu term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  µR :: (coterm r a -> command r) -> term r a
  µL :: (term r a -> command r) -> coterm r a

class Command term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  (.|.) :: term r a -> coterm r a -> command r

  infix 1 .|.

  -- FIXME: let?


-- Positive

class Coprd term coterm (command :: K.Type -> K.Type) | term -> coterm command, coterm -> term command, command -> term coterm where
  coprdR1 :: term r a -> term r (T.Coprd a b)
  coprdR2 :: term r b -> term r (T.Coprd a b)
  coprdL :: coterm r a -> coterm r b -> coterm r (T.Coprd a b)

class Pair term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  pairR :: term r a -> term r b -> term r (T.Pair a b)
  pairL :: (term r a -> term r b -> command r) -> coterm r (T.Pair a b)

class Negate term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  negateR :: coterm r a -> term r (T.Not r a)
  negateL :: (coterm r a -> command r) -> coterm r (T.Not r a)

class Cofun term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  cofunR :: term r a -> coterm r b -> term r (T.Cofun r a b)
  cofunL :: (term r a -> coterm r b -> command r) -> coterm r (T.Cofun r a b)

class Zero coterm where
  zeroL :: coterm r T.Zero

class One term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  oneR :: term r T.One
  oneL :: command r -> coterm r T.One


-- Negative

class Prd term coterm (command :: K.Type -> K.Type) | term -> coterm command, coterm -> term command, command -> term coterm where
  prdR :: term r a -> term r b -> term r (T.Prd r a b)
  prdL1 :: coterm r a -> coterm r (T.Prd r a b)
  prdL2 :: coterm r b -> coterm r (T.Prd r a b)

class Copair term coterm (command :: K.Type -> K.Type) | term -> coterm command, coterm -> term command, command -> term coterm where
  copairR :: Either (term r a) (term r b) -> term r (T.Copair r a b)
  copairL :: coterm r a -> coterm r b -> coterm r (T.Copair r a b)

class Not term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  notR :: (term r a -> command r) -> term r (T.Not r a)
  notL :: term r a -> coterm r (T.Not r a)

class Fun term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  funR :: (term r a -> coterm r b -> command r) -> term r (T.Fun r a b)
  funL :: term r a -> coterm r b -> coterm r (T.Fun r a b)

class Bottom term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  bottomR :: command r -> term r (T.Bottom r)
  bottomL :: coterm r (T.Bottom r)

class Top term where
  topR :: term r (T.Top r)


-- | An infix synonym for 'funL'.
(|>) :: Fun term coterm command => term r a -> coterm r b -> coterm r (T.Fun r a b)
(|>) = funL

infixr 9 |>


-- Library

identity :: (Fun t c d, Command t c d) => t r (T.Fun r a a)
identity = funR (.|.)

constant :: (Fun t c d, Command t c d) => t r (T.Fun r a (T.Fun r b a))
constant = funR $ \ a k -> funR (\ _ k -> a .|. k) .|. k


-- Proofs

funE :: (Mu term coterm command, Fun term coterm command, Command term coterm command) => term r (T.Fun r a b) -> term r a -> term r b
funE f a = µR (\ k -> f .|. a |> k)
