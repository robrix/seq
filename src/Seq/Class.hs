{-# LANGUAGE FunctionalDependencies #-}
module Seq.Class
( Seq(..)
, (|>)
, Fun(..)
, Not(..)
, Tensor(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  µR :: (coterm r a -> command r) -> term r a
  prdR :: term r a -> term r b -> term r (a, b)
  sumR1 :: term r a -> term r (Either a b)
  sumR2 :: term r b -> term r (Either a b)
  tensorR :: term r a -> term r b -> term r (Tensor a b)
  notR :: coterm r a -> term r (Not r a)
  funR :: (term r a -> coterm r b -> command r) -> term r (Fun r a b)

  -- left rules
  µL :: (term r a -> command r) -> coterm r a
  prdL1 :: coterm r a -> coterm r (a, b)
  prdL2 :: coterm r b -> coterm r (a, b)
  sumL :: coterm r a -> coterm r b -> coterm r (Either a b)
  tensorL :: (term r a -> term r b -> command r) -> coterm r (Tensor a b)
  notL :: term r a -> coterm r (Not r a)
  funL :: term r a -> coterm r b -> coterm r (Fun r a b)

  -- commands
  (.|.) :: term r a -> coterm r a -> command r

  infix 1 .|.


-- | An infix synonym for 'funL'.
(|>) :: Seq term coterm command => term r a -> coterm r b -> coterm r (Fun r a b)
(|>) = funL

infixr 9 |>


newtype Fun r a b = Fun { app :: (b -> r) -> (a -> r) }

newtype Not r a = Not { runNot :: a -> r }

data Tensor a b = Tensor { fst' :: !a, snd' :: !b }
