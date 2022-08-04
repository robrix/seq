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
  -- Negative
  | SBottomR (Command Term Coterm)
  | STopR
  | SPrdR (Command Term Coterm) (Command Term Coterm)
  | SCopairR (Command Term Coterm)
  | SFunR (Command Term Coterm)
  | SNotR (Command Term Coterm)

evalTerm :: [Value] -> [Continuation] -> Term -> Value
evalTerm _Γ _Δ = \case
  SVarR i    -> _Γ !! getIndex i
  SMuR b     -> MuR (\ k -> evalCommand _Γ (k:_Δ) b)
  -- Negative
  SBottomR b -> BottomR (evalCommand _Γ _Δ b)
  STopR      -> TopR
  SPrdR l r  -> PrdR (\ k -> evalCommand _Γ (k:_Δ) l) (\ k -> evalCommand _Γ (k:_Δ) r)
  SCopairR b -> CopairR (\ l r -> evalCommand _Γ (l:r:_Δ) b)
  SFunR b    -> FunR (\ a k -> evalCommand (a:_Γ) (k:_Δ) b)
  SNotR b    -> NotR (\ v -> evalCommand (v:_Γ) _Δ b)


-- Coterms

data Coterm
  = SVarL Index
  | SMuL (Command Term Coterm)
  -- Negative
  | SBottomL Coterm
  | SPrdL1 Coterm
  | SPrdL2 Coterm
  | SCopairL Coterm Coterm
  | SFunL Term Coterm
  | SNotL Term

evalCoterm :: [Value] -> [Continuation] -> Coterm -> Continuation
evalCoterm _Γ _Δ = \case
  SVarL i      -> _Δ !! getIndex i
  SMuL b       -> MuL (\ v -> evalCommand (v:_Γ) _Δ b)
  SBottomL b   -> BottomL (evalCoterm _Γ _Δ b)
  SPrdL1 k     -> PrdL1 (evalCoterm _Γ _Δ k)
  SPrdL2 k     -> PrdL2 (evalCoterm _Γ _Δ k)
  SCopairL l r -> CopairL (evalCoterm _Γ _Δ l) (evalCoterm _Γ _Δ r)
  SFunL a k    -> FunL (evalTerm _Γ _Δ a) (evalCoterm _Γ _Δ k)
  SNotL a      -> NotL (evalTerm _Γ _Δ a)


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
