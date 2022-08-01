{-# LANGUAGE FunctionalDependencies #-}
module Seq.Untyped.Syntax
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
) where

-- Rules

class Mu term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  µR :: (coterm -> command) -> term
  µL :: (term -> command) -> coterm

class Command term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  (.|.) :: term -> coterm -> command

  infix 1 .|.

  -- FIXME: let?


-- Positive

class Coprd term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  coprdR1 :: term -> term
  coprdR2 :: term -> term
  coprdL :: (term -> command) -> (term -> command) -> coterm

class Pair term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  pairR :: term -> term -> term
  pairL :: (term -> term -> command) -> coterm

class Negate term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  negateR :: coterm -> term
  negateL :: (coterm -> command) -> coterm

class Cofun term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  cofunR :: term -> coterm -> term
  cofunL :: (term -> coterm -> command) -> coterm

class Zero coterm where
  zeroL :: coterm

class One term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  oneR :: term
  oneL :: command -> coterm


-- Negative

class Prd term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  prdR :: (coterm -> command) -> (coterm -> command) -> term
  prdL1 :: coterm -> coterm
  prdL2 :: coterm -> coterm

class Copair term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  copairR :: (coterm -> coterm -> command) -> term
  copairL :: coterm -> coterm -> coterm

class Not term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  notR :: (term -> command) -> term
  notL :: term -> coterm

class Fun term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  funR :: (term -> coterm -> command) -> term
  funL :: term -> coterm -> coterm

class Bottom term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  bottomR :: command -> term
  bottomL :: coterm

class Top term where
  topR :: term
