module Seq.Untyped.Syntax
( -- * Terms
  Term(..)
  -- * Coterms
, Coterm(..)
  -- * Commands
, Command(..)
  -- * Scopes
, Scope(..)
) where

import Seq.Name

-- Terms

data Term
  = VarR Index
  | MuR Scope


-- Coterms

data Coterm
  = VarL Index
  | MuL Scope


-- Commands

data Command
  = Term :|: Coterm

infix 2 :|:


-- Scopes

newtype Scope = Scope Command
