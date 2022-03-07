{-# LANGUAGE FunctionalDependencies #-}
module Seq.Typed
( Seq(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  prdR :: term a -> term b -> term (a, b)

  -- left rules
  prdL1 :: coterm a -> coterm (a, b)
  prdL2 :: coterm b -> coterm (a, b)
