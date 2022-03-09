{-# LANGUAGE FunctionalDependencies #-}
module Seq.Typed
( Seq(..)
, coapp
, app
, Fun(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  µR :: (coterm r a -> command r) -> term r a
  prdR :: term r a -> term r b -> term r (a, b)
  sumR1 :: term r a -> term r (Either a b)
  sumR2 :: term r b -> term r (Either a b)
  funR :: (term r a -> coterm r b -> command r) -> term r (Fun r a b)

  -- left rules
  µL :: (term r a -> command r) -> coterm r a
  prdL1 :: coterm r a -> coterm r (a, b)
  prdL2 :: coterm r b -> coterm r (a, b)
  sumL :: coterm r a -> coterm r b -> coterm r (Either a b)
  funL :: term r a -> coterm r b -> coterm r (Fun r a b)

  -- commands
  (.|.) :: term r a -> coterm r a -> command r

  infix 1 .|.


coapp :: (a -> (b -> r) -> r) -> Fun r a b
coapp f = Fun (\ kb a -> f a kb)

app :: Fun r a b -> (a -> (b -> r) -> r)
app (Fun r) a kb = r kb a

newtype Fun r a b = Fun { runFun :: (b -> r) -> (a -> r) }
