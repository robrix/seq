module Seq.Untyped.Syntax
( -- * Terms
  Term(..)
, µR
  -- * Coterms
, Coterm(..)
, µL
  -- * Commands
, Command(..)
  -- * Scopes
, Scope(..)
  -- ** Abstraction
, abstractL
, abstractR
, abstractLR
  -- ** Instantiation
, instantiateL
, instantiateR
, instantiateLR
) where

import Data.Bifunctor
import Data.These
import Seq.Name

-- Terms

data Term
  = VarR (Var Index)
  | MuR Scope


µR :: Name -> Command -> Term
µR n b = MuR (abstractL n b)


-- Coterms

data Coterm
  = VarL (Var Index)
  | MuL Scope

µL :: Name -> Command -> Coterm
µL n b = MuL (abstractR n b)


-- Commands

data Command
  = Term :|: Coterm

infix 2 :|:


-- Scopes

newtype Scope = Scope { getScope :: Command }


-- Abstraction

abstractL, abstractR :: Name -> (Command -> Scope)
abstractL = abstractLR . This
abstractR = abstractLR . That

abstractLR :: These Name Name -> (Command -> Scope)
abstractLR lr = Scope . replace (bimap
  (\ l -> Replacer (fmap VarL . free l) (const (VarL . Bound)))
  (\ r -> Replacer (fmap VarR . free r) (const (VarR . Bound))) lr) 0
  where
  free n outer name
    | name == n = Bound outer
    | otherwise = Free name


-- Instantiation

instantiateL :: Coterm -> (Scope -> Command)
instantiateL = instantiateLR . This

instantiateR :: Term   -> (Scope -> Command)
instantiateR = instantiateLR . That

instantiateLR :: These Coterm Term -> (Scope -> Command)
instantiateLR lr = replace (bimap
  (Replacer (const (VarL . Free)) . boundL)
  (Replacer (const (VarR . Free)) . boundR) lr) 0 . getScope
  where
  boundL c outer inner
    | outer == inner = c
    | otherwise      = VarL (Bound inner)
  boundR c outer inner
    | outer == inner = c
    | otherwise      = VarR (Bound inner)


-- Replacement

data Replacer t = Replacer
  { free  :: Index -> Name -> t
  , bound :: Index -> Index -> t
  }

class Replace t where
  replace :: These (Replacer Coterm) (Replacer Term) -> Index -> (t -> t)

instance Replace Term where
  replace lr outer within = case within of
    VarR (Free name)   -> that (const within) (`free`  outer) lr name
    VarR (Bound inner) -> that (const within) (`bound` outer) lr inner
    MuR (Scope b)      -> MuR (Scope (replace lr (succ outer) b))
    where
    that :: c -> (b -> c) -> These a b -> c
    that d f = these (const d) f (const f)

instance Replace Coterm where
  replace lr outer within = case within of
    VarL (Free name)   -> this (`free`  outer) (const within) lr name
    VarL (Bound inner) -> this (`bound` outer) (const within) lr inner
    MuL (Scope b)      -> MuL (Scope (replace lr (succ outer) b))
    where
    this :: (a -> c) -> c -> These a b -> c
    this f d = these f (const d) (const . f)

instance Replace Command where
  replace lr outer (t :|: c) = replace lr outer t :|: replace lr outer c
