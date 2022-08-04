module Seq.Untyped.Polarized
( -- * Terms
  Term(..)
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

-- Terms

data Term
  = SVarR Index
  | SMuR (Command Term Coterm)


-- Coterms

data Coterm
  = SVarL Index
  | SMuL (Command Term Coterm)


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
