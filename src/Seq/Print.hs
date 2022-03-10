{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Seq.Print
( Prec(..)
, Print(..)
, atom
, prec
, withPrec
, resetPrec
, ($$)
) where

import Seq.Class
import Seq.Doc

newtype Prec = Prec Int
  deriving (Bounded, Enum, Eq, Num, Ord)

newtype Print prec r a = Print { getPrint :: prec -> Bind }
  deriving (Monoid, Semigroup)

instance Show (Print Prec r a) where
  showsPrec d p = string (getDoc (getBind (getPrint p (Prec d)) (Var 0)))

instance Document (Print prec r a) where
  char = Print . const . char

instance Seq (Print Prec) (Print Prec) (Print Prec ()) where
  µR f = prec mu (char 'µ' <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  prdR l r = atom (tupled [resetPrec l, resetPrec r])
  coprdR1 l = str "inl" $$ l
  coprdR2 r = str "inr" $$ r
  pairR l r = atom (list [resetPrec l, resetPrec r])
  notR c = prec 11 (char '¬' <+> withPrec 12 c)
  funR f = prec lambda (char 'λ' <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  cofunR = flip (infix' cofun (char '⤚'))

  µL f = prec mu (str "µ̃" <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  prdL1 f = str "exl" $$ f
  prdL2 f = str "exr" $$ f
  coprdL l r = str "exlr" $$ l $$ r
  pairL f = prec mu (str "µ̃" <> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> withPrec 0 (f (atom (var a)) (atom (var b))))))
  copairL a b = atom (list [resetPrec a, resetPrec b])
  notL t = str "not" $$ brackets t
  funL = infixr' ap dot
  cofunL f = prec ap (str "coapp" <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))

  (.|.) = infix' cmd (str "║")


atom :: Bind -> Print prec r a
atom = Print . const

prec :: Ord prec => prec -> Bind -> Print prec r a
prec i b = Print (\ i' -> parensIf (i' > i) b)

withPrec :: prec -> Print prec r a -> Bind
withPrec = flip getPrint

resetPrec :: Enum prec => Print prec r a -> Bind
resetPrec = withPrec (toEnum 0)

($$) :: Print Prec r a -> Print Prec r b -> Print Prec r c
($$) = infixl' ap space

infixl 9 $$


ap, cmd, cofun, lambda, mu :: Prec
ap = Prec 10
cmd = Prec 0
cofun = Prec 1
lambda = Prec 0
mu = Prec 0


infixl'
  :: (Enum prec, Ord prec)
  => prec           -- ^ precedence
  -> Bind           -- ^ operator
  -> Print prec r a -- ^ left operand
  -> Print prec r b -- ^ right operand
  -> Print prec r c
infixl' p o l r = prec p (surround o (withPrec p l) (withPrec (succ p) r))

infixr'
  :: (Enum prec, Ord prec)
  => prec           -- ^ precedence
  -> Bind           -- ^ operator
  -> Print prec r a -- ^ left operand
  -> Print prec r b -- ^ right operand
  -> Print prec r c
infixr' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec p r))

infix'
  :: (Enum prec, Ord prec)
  => prec           -- ^ precedence
  -> Bind           -- ^ operator
  -> Print prec r a -- ^ left operand
  -> Print prec s b -- ^ right operand
  -> Print prec t c
infix' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec (succ p) r))
