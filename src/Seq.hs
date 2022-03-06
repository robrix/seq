{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Seq
( Seq(..)
, Print(..)
, char
, str
, (<+>)
, parens
, bind
, var
, lambda
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


newtype Var = Var Int
  deriving (Enum, Eq, Ord, Show)

newtype Print = Print (Var -> DString)
  deriving (Monoid, Semigroup)

instance Show Print where
  showsPrec _ (Print p) = string (p (Var 0))

instance Seq Print Print Print where
  prdR l r = str "inlr" <+> l <+> r
  sumR1 l = str "inl" <+> l
  sumR2 r = str "inr" <+> r
  funR f = lambda (\ a -> lambda (\ b -> f (var a) (var b)))

  prdL1 f = str "exl" <+> parens (bind (f . var))
  prdL2 f = str "exr" <+> parens (bind (f . var))
  sumL l r = str "exlr" <+> parens (bind (l . var)) <+> parens (bind (r . var))
  funL a k = a <+> char '·' <+> k

newtype DString = DString { string :: String -> String }

instance Semigroup DString where
  a <> b = DString (string a . string b)

instance Monoid DString where
  mempty = DString id

char :: Char -> Print
char c = Print (const (DString (c:)))

str :: String -> Print
str = foldMap char

(<+>) :: Print -> Print -> Print
p <+> q = p <> char ' ' <> q

parens :: Print -> Print
parens p = char '(' <> p <> char ')'

bind :: (Var -> Print) -> Print
bind f = Print $ \ v -> let Print p = f v in p (succ v)

var :: Var -> Print
var (Var i) = str $ alphabet !! r : if q > 0 then show q else ""
  where
  n = length alphabet
  (q, r) = i `divMod` n

alphabet :: String
alphabet = ['a'..'z']

lambda :: (Var -> Print) -> Print
lambda f = bind (\ v -> char 'λ' <+> var v <+> char '.' <+> f v)
