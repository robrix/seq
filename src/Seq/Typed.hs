{-# LANGUAGE FunctionalDependencies #-}
module Seq.Typed
( Seq(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  prdR :: term a -> term b -> term (a, b)
  sumR1 :: term a -> term (Either a b)
  sumR2 :: term b -> term (Either a b)
  funR :: (term a -> coterm b -> command) -> term (a -> b)

  -- left rules
  prdL1 :: coterm a -> coterm (a, b)
  prdL2 :: coterm b -> coterm (a, b)
  sumL :: coterm a -> coterm b -> coterm (Either a b)
  funL :: term a -> coterm b -> coterm (a -> b)

  -- commands
  (.|.) :: term a -> coterm a -> command

  infix 1 .|.
