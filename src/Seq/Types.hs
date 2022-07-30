{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Seq.Types
( Fun(..)
, Cofun(..)
, Not(..)
, Prd(..)
, πL
, πR
, Coprd(..)
, exlr
, Pair(..)
, Copair(..)
, inL
, inR
, Zero
, Top(..)
, Bottom(..)
, One(..)
) where

newtype Fun r a b = Fun { app :: (b -> r) -> (a -> r) }

data Cofun r b a = (:>-) { coreturn :: a -> r, coconst :: b }

infix 1 :>-

newtype Not r a = Not { runNot :: a -> r }

newtype Prd r a b = Prd { prd :: (((a -> r) -> r) -> ((b -> r) -> r) -> r) -> r }

πL :: Prd r a b -> (a -> r) -> r
πL (Prd r) k = r (\ ka _ -> ka k)

πR :: Prd r a b -> (b -> r) -> r
πR (Prd r) k = r (\ _ kb -> kb k)

data Coprd a b
  = InL !a
  | InR !b

exlr :: (a -> r) -> (b -> r) -> Coprd a b -> r
exlr l r = \case
  InL a -> l a
  InR b -> r b

data Pair a b = Pair { pair1 :: !a, pair2 :: !b }

newtype Copair r a b = Copair { copair :: (a -> r) -> (b -> r) -> r }

inL :: ((a -> r) -> r) -> Copair r a b
inL a = Copair (\ f _ -> a f)

inR :: ((b -> r) -> r) -> Copair r a b
inR b = Copair (\ _ g -> b g)


data Zero

newtype Top r = Top { getTop :: r -> r }


newtype Bottom r = Bottom { getBottom :: r }

data One = One
