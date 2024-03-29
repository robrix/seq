{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Seq.Print
( Level(..)
, Print(..)
, atom
, prec
, withPrec
, resetPrec
, localPrec
, ($$)
, printSeq
) where

import Seq.Class
import Seq.Doc

data Level
  = Bottom
  | Binder
  | Cofun
  | Apply
  | Prefix
  | Top
  deriving (Bounded, Enum, Eq, Ord, Show)

newtype Print r a = Print { getPrint :: Prec Level (Bind Doc) }
  deriving (Monoid, Semigroup, Precedence (Bind Doc) Level)

instance Show (Print r a) where
  showsPrec d p = showsPrec d (getBind (getPrec (getPrint p) Bottom) (Var 0))

instance Document (Print r a) where
  char = Print . Prec . const . char
  enclosing l r = enclose l r . localPrec (const minBound)
  enclosingSep l r s = encloseSep l r s . map (localPrec (const minBound))

  withIndentation f = Print (Prec (\ d -> withIndentation (withPrec d . f)))
  withColumn f = Print (Prec (\ d -> withColumn (withPrec d . f)))


-- Rules

instance Mu Print Print (Print ()) where
  µR f = prec Binder (char 'µ' <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  µL f = prec Binder (str "µ̃" <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))

instance Command Print Print (Print ()) where
  (.|.) = infix' Binder (surround pipe space space)


-- Positive

instance Coprd Print Print (Print ()) where
  coprdR1 l = str "inl" $$ l
  coprdR2 r = str "inr" $$ r
  coprdL l r = prec Binder (str "µ̃" <> list [bind (\ a -> brackets (var a) <+> dot <+> resetPrec (l (atom (var a)))), bind (\ b -> brackets (var b) <+> dot <+> resetPrec (r (atom (var b))))])

instance Pair Print Print (Print ()) where
  pairR l r = atom (list [resetPrec l, resetPrec r])
  pairL f = prec Binder (str "µ̃" <> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))

instance Negate Print Print (Print ()) where
  negateR c = infixl' Prefix space (char '¬') c
  negateL f = prec Binder (str "negate" <> brackets (bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a))))))

instance Cofun Print Print (Print ()) where
  cofunR = flip (infix' Cofun (char '⤚'))
  cofunL f = prec Apply (str "coapp" <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))

instance Zero Print where
  zeroL = str "µ̃" <> brackets mempty

instance One Print Print (Print ()) where
  oneR = parens mempty
  oneL b = prec Binder (str "µ̃" <> brackets (parens mempty <+> dot <+> resetPrec b))


-- Negative

instance Prd Print Print (Print ()) where
  prdR l r = prec Binder (char 'µ' <> tupled [bind (\ a -> brackets (var a) <+> dot <+> resetPrec (l (atom (var a)))), bind (\ b -> brackets (var b) <+> dot <+> resetPrec (r (atom (var b))))])
  prdL1 f = str "exl" $$ f
  prdL2 f = str "exr" $$ f

instance Copair Print Print (Print ()) where
  copairR f = prec Binder (char 'µ' <> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  copairL a b = atom (list [resetPrec a, resetPrec b])

instance Not Print Print (Print ()) where
  notR f = infixl' Prefix space (char '¬') (prec Binder (bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a))))))
  notL t = str "not" $$ brackets t

instance Fun Print Print (Print ()) where
  funR f = prec Binder (char 'λ' <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  funL = infixr' Apply dot

instance Bottom Print Print (Print ()) where
  bottomR b = prec Binder (char 'µ' <> brackets mempty <+> dot <+> resetPrec b)
  bottomL = brackets mempty

instance Top Print where
  topR = char 'µ' <> parens mempty


($$) :: Print r a -> Print s b -> Print t c
($$) = infixl' Apply space

infixl 9 $$

printSeq :: Print r a -> IO ()
printSeq = putStrLn . show


infixl'
  :: Level     -- ^ precedence
  -> Bind Doc  -- ^ operator
  -> Print r a -- ^ left operand
  -> Print s b -- ^ right operand
  -> Print t c
infixl' p o l r = prec p (surround o (withPrec p l) (withPrec (succ p) r))

infixr'
  :: Level     -- ^ precedence
  -> Bind Doc  -- ^ operator
  -> Print r a -- ^ left operand
  -> Print s b -- ^ right operand
  -> Print t c
infixr' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec p r))

infix'
  :: Level     -- ^ precedence
  -> Bind Doc  -- ^ operator
  -> Print r a -- ^ left operand
  -> Print s b -- ^ right operand
  -> Print t c
infix' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec (succ p) r))
