{-# LANGUAGE GADTs #-}
module Seq.Untyped.Polarized
( -- * Polarities
  N
, P
  -- * Terms
, Term(..)
  -- * Coterms
, Coterm(..)
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


-- Terms

data Term p where
  SVarR :: Index -> Term p
  SMuR :: Command Term Coterm -> Term p


-- Coterms

data Coterm p where
  SVarL :: Index -> Coterm p
  SMuL :: Command Term Coterm -> Coterm p


-- Values

data Value p where
  VarR :: Level -> Value p
  MuR :: (Continuation p -> Command Value Continuation) -> Value p
  -- Negative
  BottomR :: Command Value Continuation -> Value N
  TopR :: Value N
  PrdR :: (Continuation N -> Command Value Continuation) -> (Continuation N -> Command Value Continuation) -> Value N
  CopairR :: (Continuation N -> Continuation N -> Command Value Continuation) -> Value N
  FunR :: (Value P -> Continuation N -> Command Value Continuation) -> Value N
  NotR :: (Value P -> Command Value Continuation) -> Value N
  UpR :: (Continuation P -> Command Value Continuation) -> Value N
  -- Positive
  OneR :: Value P
  CoprdR1 :: Value P -> Value P
  CoprdR2 :: Value P -> Value P
  PairR :: !(Value P) -> !(Value P) -> Value P
  CofunR :: Value P -> Continuation N -> Value P
  NegateR :: Continuation N -> Value P
  DownR :: Value N -> Value P


-- Continuations

data Continuation p where
  VarL :: Level -> Continuation p
  MuL :: (Value p -> Command Value Continuation) -> Continuation p
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
  OneL :: Command Value Continuation -> Continuation P
  CoprdL :: (Value P -> Command Value Continuation) -> (Value P -> Command Value Continuation) -> Continuation P
  PairL :: (Value P -> Value P -> Command Value Continuation) -> Continuation P
  CofunL :: (Value P -> Continuation N -> Command Value Continuation) -> Continuation P
  NegateL :: (Continuation N -> Command Value Continuation) -> Continuation P
  DownL :: (Value N -> Command Value Continuation) -> Continuation P


-- Commands

data Command v k
  = v N :|:- k N
  | v P :|:+ k P
