{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
module Seq.Class
( Mu(..)
, Prd(..)
, Coprd(..)
, Pair(..)
, Copair(..)
, Not(..)
, Fun(..)
, Cofun(..)
, Command(..)
, (|>)
  -- * Library
, identity
, constant
  -- * Proofs
, funE
) where

import qualified Data.Kind as K
import qualified Seq.Types as T

class Mu term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  µR :: (coterm r a -> command r) -> term r a
  µL :: (term r a -> command r) -> coterm r a

class Prd term coterm (command :: K.Type -> K.Type) | term -> coterm command, coterm -> term command, command -> term coterm where
  prdR :: term r a -> term r b -> term r (T.Prd r a b)
  prdL1 :: coterm r a -> coterm r (T.Prd r a b)
  prdL2 :: coterm r b -> coterm r (T.Prd r a b)

class Coprd term coterm (command :: K.Type -> K.Type) | term -> coterm command, coterm -> term command, command -> term coterm where
  coprdR1 :: term r a -> term r (T.Coprd a b)
  coprdR2 :: term r b -> term r (T.Coprd a b)
  coprdL :: coterm r a -> coterm r b -> coterm r (T.Coprd a b)

class Pair term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  pairR :: term r a -> term r b -> term r (T.Pair a b)
  pairL :: (term r a -> term r b -> command r) -> coterm r (T.Pair a b)

class Copair term coterm (command :: K.Type -> K.Type) | term -> coterm command, coterm -> term command, command -> term coterm where
  copairR :: Either (term r a) (term r b) -> term r (T.Copair r a b)
  copairL :: coterm r a -> coterm r b -> coterm r (T.Copair r a b)

class Not term coterm (command :: K.Type -> K.Type) | term -> coterm command, coterm -> term command, command -> term coterm where
  notR :: coterm r a -> term r (T.Not r a)
  notL :: term r a -> coterm r (T.Not r a)

-- FIXME: negate?

class Fun term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  funR :: (term r a -> coterm r b -> command r) -> term r (T.Fun r a b)
  funL :: term r a -> coterm r b -> coterm r (T.Fun r a b)

class Cofun term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  cofunR :: term r a -> coterm r b -> term r (T.Cofun r a b)
  cofunL :: (term r a -> coterm r b -> command r) -> coterm r (T.Cofun r a b)

class Command term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  (.|.) :: term r a -> coterm r a -> command r

  infix 1 .|.


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
