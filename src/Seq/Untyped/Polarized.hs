module Seq.Untyped.Polarized
( -- * Values
  NValue(..)
, PValue(..)
  -- * Continuations
, NContinuation(..)
, PContinuation(..)
  -- * Commands
, Command(..)
) where

import Seq.Name

-- Values

data NValue
  = NVarR Level
  | NMuR (NContinuation -> Command)

data PValue
  = PVarR Level
  | PMuR (PContinuation -> Command)


-- Continuations

data NContinuation
  = NVarL Level
  | NMuL (NValue -> Command)

data PContinuation
  = PVarL Level
  | PMuL (PValue -> Command)


-- Commands

data Command
  = NValue :|:- NContinuation
  | PValue :|:+ PContinuation
