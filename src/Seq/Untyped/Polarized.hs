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
  | NPrdR (NContinuation -> Command) (NContinuation -> Command)

data PValue
  = PVarR Level
  | PMuR (PContinuation -> Command)
  | PCoprdR1 PValue
  | PCoprdR2 PValue


-- Continuations

data NContinuation
  = NVarL Level
  | NMuL (NValue -> Command)
  | NPrdL1 NContinuation
  | NPrdL2 NContinuation

data PContinuation
  = PVarL Level
  | PMuL (PValue -> Command)
  | PCoprdL (PValue -> Command) (PValue -> Command)


-- Commands

data Command
  = NValue :|:- NContinuation
  | PValue :|:+ PContinuation
