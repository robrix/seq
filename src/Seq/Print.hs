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

import Seq.Class hiding (Fun(..))
import Seq.Doc

data Level
  = Bottom
  | Binder
  | Cofun
  | Apply
  | Prefix
  | Top
  deriving (Bounded, Enum, Eq, Ord, Show)

newtype Print level doc r a = Print { getPrint :: Prec level doc }
  deriving (Monoid, Semigroup, Precedence doc level)

instance Show (Print Level (Bind Doc) r a) where
  showsPrec _ p = getDoc (getBind (getPrec (getPrint p) Bottom) (Var 0))

instance (Document doc, Bounded level, Ord level) => Document (Print level doc r a) where
  char = Print . Prec . const . char
  enclosing l r = enclose l r . localPrec (const minBound)
  enclosingSep l r s = encloseSep l r s . map (localPrec (const minBound))

instance Seq (Print Level (Bind Doc)) (Print Level (Bind Doc)) (Print Level (Bind Doc) ()) where
  µR f = prec Binder (char 'µ' <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  prdR l r = atom (tupled [resetPrec l, resetPrec r])
  coprdR1 l = str "inl" $$ l
  coprdR2 r = str "inr" $$ r
  pairR l r = atom (list [resetPrec l, resetPrec r])
  notR c = infixl' Prefix space (char '¬') c
  funR f = prec Binder (char 'λ' <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  cofunR = flip (infix' Cofun (char '⤚'))

  µL f = prec Binder (str "µ̃" <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  prdL1 f = str "exl" $$ f
  prdL2 f = str "exr" $$ f
  coprdL l r = str "exlr" $$ l $$ r
  pairL f = prec Binder (str "µ̃" <> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  copairL a b = atom (list [resetPrec a, resetPrec b])
  notL t = str "not" $$ brackets t
  funL = infixr' Apply dot
  cofunL f = prec Apply (str "coapp" <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))

  (.|.) = infix' Binder (surround pipe space space)


($$) :: Document doc => Print Level doc r a -> Print Level doc r b -> Print Level doc r c
($$) = infixl' Apply space

infixl 9 $$

printSeq :: Print Level (Bind Doc) r a -> IO ()
printSeq p = putStrLn (getDoc (getBind (getPrec (getPrint p) Bottom) (Var 0)) "")


infixl'
  :: (Enum level, Ord level, Document doc)
  => level               -- ^ precedence
  -> doc                -- ^ operator
  -> Print level doc r a -- ^ left operand
  -> Print level doc r b -- ^ right operand
  -> Print level doc r c
infixl' p o l r = prec p (surround o (withPrec p l) (withPrec (succ p) r))

infixr'
  :: (Enum level, Ord level, Document doc)
  => level               -- ^ precedence
  -> doc                -- ^ operator
  -> Print level doc r a -- ^ left operand
  -> Print level doc r b -- ^ right operand
  -> Print level doc r c
infixr' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec p r))

infix'
  :: (Enum level, Ord level, Document doc)
  => level               -- ^ precedence
  -> doc                -- ^ operator
  -> Print level doc r a -- ^ left operand
  -> Print level doc s b -- ^ right operand
  -> Print level doc t c
infix' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec (succ p) r))
