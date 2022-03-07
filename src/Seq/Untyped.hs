{-# LANGUAGE FunctionalDependencies #-}
module Seq.Untyped
( Seq(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  prdR :: term -> term -> term
  sumR1 :: term -> term
  sumR2 :: term -> term
  funR :: (term -> coterm -> command) -> term

  -- left rules
  prdL1 :: coterm -> coterm
  prdL2 :: coterm -> coterm
  sumL :: coterm -> coterm -> coterm
  funL :: term -> coterm -> coterm

  (.|.) :: term -> coterm -> command

  infix 1 .|.
