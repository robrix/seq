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
, printSeq
) where

import Seq.Class hiding (Fun(..))
import Seq.Doc

data Prec
  = None
  | Command
  | Mu
  | Fun
  | Cofun
  | Apply
  | Prefix
  deriving (Enum, Eq, Ord, Show)

newtype Print prec r a = Print { getPrint :: prec -> Bind }
  deriving (Monoid, Semigroup)

instance Show (Print Prec r a) where
  showsPrec _ p = string (getDoc (getBind (getPrint p None) (Var 0)))

instance Document (Print prec r a) where
  char = Print . const . char

instance Seq (Print Prec) (Print Prec) (Print Prec ()) where
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


atom :: Bind -> Print prec r a
atom = Print . const

prec :: Ord prec => prec -> Bind -> Print prec r a
prec i b = Print (\ i' -> parensIf (i' > i) b)

withPrec :: prec -> Print prec r a -> Bind
withPrec = flip getPrint

resetPrec :: Enum prec => Print prec r a -> Bind
resetPrec = withPrec (toEnum 0)

($$) :: Print Prec r a -> Print Prec r b -> Print Prec r c
($$) = infixl' Apply space

infixl 9 $$

printSeq :: Print Prec r a -> IO ()
printSeq p = putStrLn (string (getDoc (getBind (getPrint p None) (Var 0))) "")


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
