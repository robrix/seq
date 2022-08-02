{-# LANGUAGE GADTs #-}
module Seq.Untyped.Polarized
( -- * Polarities
  N
, P
  -- * Types
, Type(..)
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


-- Types

data Type n where
  -- Negative
  Bottom :: Type N
  Top :: Type N
  (:&) :: Type N -> Type N -> Type N
  (:⅋) :: Type N -> Type N -> Type N
  (:→) :: Type P -> Type N -> Type N
  Not :: Type  P -> Type N
  Up :: Type P -> Type N
  ForAll :: (Type n -> Type N) -> Type N
  -- Positive
  Zero :: Type P
  One :: Type P
  (:⊕) :: Type P -> Type P -> Type P
  (:⊗) :: Type P -> Type P -> Type P
  (:⤚) :: Type N -> Type P -> Type P
  Negate :: Type N -> Type P
  Down :: Type N -> Type P
  Exists :: (Type n -> Type P) -> Type P


-- Values

data Value p where
  VarR :: Level -> Value p
  MuR :: (Continuation n -> Command) -> Value n
  -- Negative
  BottomR :: Command -> Value N
  TopR :: Value N
  PrdR :: (Continuation N -> Command) -> (Continuation N -> Command) -> Value N
  CopairR :: (Continuation N -> Continuation N -> Command) -> Value N
  FunR :: (Value P -> Continuation N -> Command) -> Value N
  NotR :: (Value P -> Command) -> Value N
  UpR :: (Continuation P -> Command) -> Value N
  ForAllR :: (Type n -> Continuation N -> Command) -> Value N
  -- Positive
  OneR :: Value P
  CoprdR1 :: Value P -> Value P
  CoprdR2 :: Value P -> Value P
  PairR :: !(Value P) -> !(Value P) -> Value P
  CofunR :: Value P -> Continuation N -> Value P
  NegateR :: Continuation N -> Value P
  DownR :: Value N -> Value P
  ExistsR :: Type n -> Value P -> Value P


-- Continuations

data Continuation p where
  VarL :: Level -> Continuation p
  MuL :: (Value p -> Command) -> Continuation p
  -- Negative
  BottomL :: Continuation N
  PrdL1 :: Continuation N -> Continuation N
  PrdL2 :: Continuation N -> Continuation N
  CopairL :: Continuation N -> Continuation N -> Continuation N
  FunL :: Value P -> Continuation N -> Continuation N
  NotL :: Value P -> Continuation N
  UpL :: Continuation P -> Continuation N
  -- Positive
  ZeroL :: Continuation P
  OneL :: Command -> Continuation P
  CoprdL :: (Value P -> Command) -> (Value P -> Command) -> Continuation P
  PairL :: (Value P -> Value P -> Command) -> Continuation P
  CofunL :: (Value P -> Continuation N -> Command) -> Continuation P
  NegateL :: (Continuation N -> Command) -> Continuation P
  DownL :: (Value N -> Command) -> Continuation P


-- Commands

data Command
  = Value N :|:- Continuation N
  | Value P :|:+ Continuation P
