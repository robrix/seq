{-# LANGUAGE FunctionalDependencies #-}
module Seq.Untyped
( Seq(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  µR :: (coterm -> command) -> term
  prdR :: term -> term -> term
  sumR1 :: term -> term
  sumR2 :: term -> term
  funR :: (term -> coterm -> command) -> term

  -- left rules
  µL :: (term -> command) -> coterm
  prdL1 :: coterm -> coterm
  prdL2 :: coterm -> coterm
  sumL :: coterm -> coterm -> coterm
  funL :: term -> coterm -> coterm

  (.|.) :: term -> coterm -> command

  infix 1 .|.
