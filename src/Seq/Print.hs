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

instance Mu Print Print (Print ()) where
  µR f = prec Binder (char 'µ' <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  µL f = prec Binder (str "µ̃" <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))

instance Prd Print Print (Print ()) where
  prdR l r = atom (tupled [resetPrec l, resetPrec r])
  prdL1 f = str "exl" $$ f
  prdL2 f = str "exr" $$ f

instance Term Print Print (Print ()) where
  coprdR1 l = str "inl" $$ l
  coprdR2 r = str "inr" $$ r
  pairR l r = atom (list [resetPrec l, resetPrec r])
  copairR = either (atom . brackets . resetPrec) (atom . brackets . resetPrec)
  notR c = infixl' Prefix space (char '¬') c
  funR f = prec Binder (char 'λ' <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  cofunR = flip (infix' Cofun (char '⤚'))

instance Coterm Print Print (Print ()) where
  coprdL l r = str "exlr" $$ l $$ r
  pairL f = prec Binder (str "µ̃" <> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  copairL a b = atom (list [resetPrec a, resetPrec b])
  notL t = str "not" $$ brackets t
  funL = infixr' Apply dot
  cofunL f = prec Apply (str "coapp" <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))

instance Command Print Print (Print ()) where
  (.|.) = infix' Binder (surround pipe space space)


($$) :: Print r a -> Print r b -> Print r c
($$) = infixl' Apply space

infixl 9 $$

printSeq :: Print r a -> IO ()
printSeq = putStrLn . show


infixl'
  :: Level     -- ^ precedence
  -> Bind Doc  -- ^ operator
  -> Print r a -- ^ left operand
  -> Print r b -- ^ right operand
  -> Print r c
infixl' p o l r = prec p (surround o (withPrec p l) (withPrec (succ p) r))

infixr'
  :: Level     -- ^ precedence
  -> Bind Doc  -- ^ operator
  -> Print r a -- ^ left operand
  -> Print r b -- ^ right operand
  -> Print r c
infixr' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec p r))

infix'
  :: Level     -- ^ precedence
  -> Bind Doc  -- ^ operator
  -> Print r a -- ^ left operand
  -> Print s b -- ^ right operand
  -> Print t c
infix' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec (succ p) r))
