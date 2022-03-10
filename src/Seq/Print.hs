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
, localPrec
, ($$)
, printSeq
) where

import Seq.Class hiding (Fun(..))
import Seq.Doc

data Prec
  = Bottom
  | Command
  | Mu
  | Fun
  | Cofun
  | Apply
  | Prefix
  | Top
  deriving (Bounded, Enum, Eq, Ord, Show)

newtype Print prec doc r a = Print { getPrint :: prec -> doc }
  deriving (Monoid, Semigroup)

instance Show (Print Prec Bind r a) where
  showsPrec _ p = string (getDoc (getBind (getPrint p Bottom) (Var 0)))

instance (Document doc, Bounded prec) => Document (Print prec doc r a) where
  char = Print . const . char
  enclosing l r = enclose l r . localPrec (const minBound)
  enclosingSep l r s = encloseSep l r s . map (localPrec (const minBound))

instance Seq (Print Prec Bind) (Print Prec Bind) (Print Prec Bind ()) where
  µR f = prec Mu (char 'µ' <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  prdR l r = atom (tupled [resetPrec l, resetPrec r])
  coprdR1 l = str "inl" $$ l
  coprdR2 r = str "inr" $$ r
  pairR l r = atom (list [resetPrec l, resetPrec r])
  notR c = infixl' Prefix space (char '¬') c
  funR f = prec Fun (char 'λ' <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  cofunR = flip (infix' Cofun (char '⤚'))

  µL f = prec Mu (str "µ̃" <+> bind (\ a -> list [var a] <+> dot <+> resetPrec (f (atom (var a)))))
  prdL1 f = str "exl" $$ f
  prdL2 f = str "exr" $$ f
  coprdL l r = str "exlr" $$ l $$ r
  pairL f = prec Mu (str "µ̃" <> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))
  copairL a b = atom (list [resetPrec a, resetPrec b])
  notL t = str "not" $$ brackets t
  funL = infixr' Apply dot
  cofunL f = prec Apply (str "coapp" <+> bind (\ a -> bind (\ b -> list [var a, var b] <+> dot <+> resetPrec (f (atom (var a)) (atom (var b))))))

  (.|.) = infix' Command (surround pipe space space)


atom :: doc -> Print prec doc r a
atom = Print . const

prec :: (Document doc, Ord prec) => prec -> doc -> Print prec doc r a
prec i b = Print (\ i' -> parensIf (i' > i) b)

withPrec :: prec -> Print prec doc r a -> doc
withPrec = flip getPrint

resetPrec :: Bounded prec => Print prec doc r a -> doc
resetPrec = withPrec minBound

localPrec :: (prec -> prec) -> Print prec doc r a -> Print prec doc r a
localPrec f p = Print (getPrint p . f)

($$) :: Document doc => Print Prec doc r a -> Print Prec doc r b -> Print Prec doc r c
($$) = infixl' Apply space

infixl 9 $$

printSeq :: Print Prec Bind r a -> IO ()
printSeq p = putStrLn (string (getDoc (getBind (getPrint p Bottom) (Var 0))) "")


infixl'
  :: (Enum prec, Ord prec, Document doc)
  => prec               -- ^ precedence
  -> doc                -- ^ operator
  -> Print prec doc r a -- ^ left operand
  -> Print prec doc r b -- ^ right operand
  -> Print prec doc r c
infixl' p o l r = prec p (surround o (withPrec p l) (withPrec (succ p) r))

infixr'
  :: (Enum prec, Ord prec, Document doc)
  => prec               -- ^ precedence
  -> doc                -- ^ operator
  -> Print prec doc r a -- ^ left operand
  -> Print prec doc r b -- ^ right operand
  -> Print prec doc r c
infixr' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec p r))

infix'
  :: (Enum prec, Ord prec, Document doc)
  => prec               -- ^ precedence
  -> doc                -- ^ operator
  -> Print prec doc r a -- ^ left operand
  -> Print prec doc s b -- ^ right operand
  -> Print prec doc t c
infix' p o l r = prec p (surround o (withPrec (succ p) l) (withPrec (succ p) r))
