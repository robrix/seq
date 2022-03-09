{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Seq.Print
( Prec(..)
, Print(..)
, atom
, prec
, withPrec
, ($$)
) where

import Seq.Class
import Seq.Doc

newtype Prec = Prec Int
  deriving (Eq, Num, Ord)

newtype Print r a = Print { getPrint :: Prec -> Bind }
  deriving (Monoid, Semigroup)

instance Show (Print r a) where
  showsPrec d p = string (getDoc (getBind (getPrint p (Prec d)) (Var 0)))

instance Seq Print Print (Print ()) where
  µR f = prec 0 (char 'µ' <+> bind (\ a -> brackets (var a) <+> dot <+> withPrec 0 (f (atom (var a)))))
  prdR l r = prec 10 (tupled [withPrec 11 l, withPrec 11 r])
  coprdR1 l = prec 10 (str "inl" <+> withPrec 11 l)
  coprdR2 r = prec 10 (str "inr" <+> withPrec 11 r)
  pairR l r = prec 10 (list [withPrec 11 l, withPrec 11 r])
  notR c = prec 11 (char '¬' <+> withPrec 12 c)
  funR f = prec 0 (char 'λ' <+> bind (\ a -> bind (\ b -> brackets (var a <> comma <+> var b) <+> dot <+> withPrec 0 (f (atom (var a)) (atom (var b))))))

  µL f = prec 0 (str "µ̃" <+> bind (\ a -> brackets (var a) <+> dot <+> withPrec 0 (f (atom (var a)))))
  prdL1 f = prec 10 (str "exl" <+> withPrec 11 f)
  prdL2 f = prec 10 (str "exr" <+> withPrec 11 f)
  coprdL l r = prec 10 (str "exlr" <+> withPrec 11 l <+> withPrec 11 r)
  pairL f = prec 0 (str "µ̃" <> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> withPrec 0 (f (atom (var a)) (atom (var b))))))
  notL t = prec 10 (str "not" <+> brackets (withPrec 11 t))
  funL = assocr 10 dot

  t .|. c = prec 0 (withPrec 1 t <+> str "║" <+> withPrec 1 c)


atom :: Bind -> Print r a
atom = Print . const

prec :: Prec -> Bind -> Print r a
prec i b = Print (\ i' -> parensIf (i' > i) b)

withPrec :: Prec -> Print r a -> Bind
withPrec = flip getPrint

($$) :: Print r a -> Print r b -> Print r c
($$) = assocl (Prec 10) space

infixl 9 $$


assocl
  :: Prec      -- ^ precedence
  -> Bind       -- ^ operator
  -> Print r a -- ^ left operand
  -> Print r b -- ^ right operand
  -> Print r c
assocl p o l r = prec p (surround o (withPrec p l) (withPrec (p + 1) r))

assocr
  :: Prec      -- ^ precedence
  -> Bind       -- ^ operator
  -> Print r a -- ^ left operand
  -> Print r b -- ^ right operand
  -> Print r c
assocr p o l r = prec p (surround o (withPrec (p + 1) l) (withPrec p r))
