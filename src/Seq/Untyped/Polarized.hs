{-# LANGUAGE GADTs #-}
module Seq.Untyped.Polarized
( -- * Polarities
  N
, P
  -- * Values
, Value(..)
  -- * Continuations
, Continuation(..)
  -- * Commands
, Command(..)
) where

import Seq.Name

-- Polarities

data N
data P


-- Values

data Value p where
  VarR :: Level -> Value p
  MuR :: (Continuation n -> Command) -> Value n
  -- Negative
  PrdR :: (Continuation N -> Command) -> (Continuation N -> Command) -> Value N
  CopairR :: (Continuation N -> Continuation N -> Command) -> Value N
  FunR :: (Value P -> Continuation N -> Command) -> Value N
  NotR :: (Value P -> Command) -> Value N
  UpR :: (Continuation P -> Command) -> Value N
  -- Positive
  CoprdR1 :: Value P -> Value P
  CoprdR2 :: Value P -> Value P
  PairR :: !(Value P) -> !(Value P) -> Value P
  NegateR :: Continuation N -> Value P
  DownR :: Value N -> Value P


-- Continuations

data Continuation p where
  VarL :: Level -> Continuation p
  MuL :: (Value p -> Command) -> Continuation p
  -- Negative
  PrdL1 :: Continuation N -> Continuation N
  PrdL2 :: Continuation N -> Continuation N
  CopairL :: Continuation N -> Continuation N -> Continuation N
  FunL :: Value P -> Continuation N -> Continuation N
  NotL :: Value P -> Continuation N
  UpL :: Continuation P -> Continuation N
  -- Positive
  CoprdL :: (Value P -> Command) -> (Value P -> Command) -> Continuation P
  PairL :: (Value P -> Value P -> Command) -> Continuation P
  NegateL :: (Continuation N -> Command) -> Continuation P
  DownL :: (Value N -> Command) -> Continuation P


-- Commands

data Command
  = Value N :|:- Continuation N
  | Value P :|:+ Continuation P
