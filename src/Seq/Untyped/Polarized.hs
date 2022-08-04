{-# LANGUAGE LambdaCase #-}
module Seq.Untyped.Polarized
( -- * Terms
  Term(..)
, evalTerm
  -- * Coterms
, Coterm(..)
, evalCoterm
  -- * Values
, Value(..)
  -- * Continuations
, Continuation(..)
  -- * Commands
, Command(..)
, evalCommand
) where

import Seq.Name

-- Terms

data Term
  = SVarR Index
  | SMuR (Command Term Coterm)

evalTerm :: [Value] -> [Continuation] -> Term -> Value
evalTerm _Γ _Δ = \case
  SVarR i -> _Γ !! getIndex i
  SMuR b  -> MuR (\ k -> evalCommand _Γ (k:_Δ) b)


-- Coterms

data Coterm
  = SVarL Index
  | SMuL (Command Term Coterm)

evalCoterm :: [Value] -> [Continuation] -> Coterm -> Continuation
evalCoterm _Γ _Δ = \case
  SVarL i -> _Δ !! getIndex i
  SMuL b  -> MuL (\ v -> evalCommand (v:_Γ) _Δ b)


-- Values

data Value
  = VarR Level
  | MuR (Continuation -> Command Value Continuation)
  -- Negative
  | BottomR (Command Value Continuation)
  | TopR
  | PrdR (Continuation -> Command Value Continuation) (Continuation -> Command Value Continuation)
  | CopairR (Continuation -> Continuation -> Command Value Continuation)
  | FunR (Value -> Continuation -> Command Value Continuation)
  | NotR (Value -> Command Value Continuation)
  | UpR (Continuation -> Command Value Continuation)
  -- Positive
  | OneR
  | CoprdR1 Value
  | CoprdR2 Value
  | PairR !(Value) !(Value)
  | CofunR Value Continuation
  | NegateR Continuation
  | DownR Value


-- Continuations

data Continuation
  = VarL Level
  | MuL (Value -> Command Value Continuation)
  -- Negative
  | BottomL Continuation
  | PrdL1 Continuation
  | PrdL2 Continuation
  | CopairL Continuation Continuation
  | FunL Value Continuation
  | NotL Value
  | UpL Continuation
  -- Positive
  | ZeroL Continuation
  | OneL (Command Value Continuation)
  | CoprdL (Value -> Command Value Continuation) (Value -> Command Value Continuation)
  | PairL (Value -> Value -> Command Value Continuation)
  | CofunL (Value -> Continuation -> Command Value Continuation)
  | NegateL (Continuation -> Command Value Continuation)
  | DownL (Value -> Command Value Continuation)


-- Commands

data Command v k
  = v :|:- k
  | v :|:+ k

evalCommand :: [Value] -> [Continuation] -> Command Term Coterm -> Command Value Continuation
evalCommand _Γ _Δ = \case
  t :|:- c -> evalTerm _Γ _Δ t :|:- evalCoterm _Γ _Δ c
  t :|:+ c -> evalTerm _Γ _Δ t :|:+ evalCoterm _Γ _Δ c
