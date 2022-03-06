{-# LANGUAGE FunctionalDependencies #-}
module Seq
( Seq(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  prdR :: term -> term -> term
  sumR1 :: term -> term
  sumR2 :: term -> term


newtype Print = Print { print :: String -> String }

instance Semigroup Print where
  Print a <> Print b = Print (a . b)

instance Monoid Print where
  mempty = Print id

char :: Char -> Print
char c = Print (c:)

str :: String -> Print
str = foldMap char

(<+>) :: Print -> Print -> Print
p <+> q = p <> char ' ' <> q
