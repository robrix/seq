{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Seq
( Seq(..)
, Print(..)
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  prdR :: term -> term -> term
  sumR1 :: term -> term
  sumR2 :: term -> term
  funR :: (term -> coterm -> command) -> term

  -- left rules
  prdL1 :: (term -> coterm) -> coterm
  prdL2 :: (term -> coterm) -> coterm
  sumL :: (term -> coterm) -> (term -> coterm) -> coterm
  funL :: term -> coterm -> coterm


newtype Print = Print DString
  deriving (Monoid, Semigroup)

newtype DString = DString { string :: String -> String }

instance Semigroup DString where
  DString a <> DString b = DString (a . b)

instance Monoid DString where
  mempty = DString id

char :: Char -> Print
char c = Print (DString (c:))

str :: String -> Print
str = foldMap char

(<+>) :: Print -> Print -> Print
p <+> q = p <> char ' ' <> q

parens :: Print -> Print
parens p = char '(' <> p <> char ')'
