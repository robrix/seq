{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Seq.Eval
( Term(..)
, Coterm(..)
, Command(..)
) where

import           Control.Monad (ap)
import           Data.Coerce (coerce)
import           Data.Functor.Contravariant (Contravariant(..))
import           Seq.Class hiding (Command, Coterm, Term)
import qualified Seq.Class as SQ

newtype Term r a = Term { eval :: (a -> r) -> r }

instance Functor (Term r) where
  fmap f (Term r) = Term (r . (. f))

instance Applicative (Term r) where
  pure a = Term (\ k -> k a)
  (<*>) = ap

instance Monad (Term r) where
  m >>= f = Term (\ k -> eval m (\ a -> eval (f a) k))


newtype Coterm r a = Coterm { coeval :: a -> r }

instance Contravariant (Coterm r) where
  contramap f (Coterm r) = Coterm (r . f)


newtype Command r = Command { runCommand :: r }
  deriving (Functor)

instance Applicative Command where
  pure = Command
  (<*>) = coerce

instance Monad Command where
  a >>= f = coerce f a


instance SQ.Term Term Coterm Command where
  µR f = Term (runCommand . f . Coterm)
  prdR l r = Term (\ k -> k (Prd (\ k' -> k' (eval l) (eval r))))
  coprdR1 = fmap Left
  coprdR2 = fmap Right
  notR = pure . Not . coeval
  pairR (Term a) (Term b)  = Term (\ k -> a (\ a -> b (\ b -> k (Pair a b))))
  copairR (Term r) = Term (\ k -> r (\ e -> k (Copair (\ f g -> either f g e))))
  funR f = pure (Fun (\ kb a -> runCommand (f (pure a) (Coterm kb))))
  cofunR a b = (coeval b :>-) <$> a

instance SQ.Coterm Term Coterm Command where
  µL f = Coterm (runCommand . f . pure)
  prdL1 k = Coterm (\ p -> πL p (coeval k))
  prdL2 k = Coterm (\ p -> πR p (coeval k))
  coprdL p q = Coterm (either (coeval p) (coeval q))
  pairL f = Coterm (\ c -> runCommand (f (pure (pair1 c)) (pure (pair2 c))))
  copairL a b = Coterm (\ c -> copair c (coeval a) (coeval b))
  notL t = Coterm (eval t . runNot)
  funL a b = Coterm (\ f -> eval a (app f (coeval b)))
  cofunL f = Coterm (\ (b :>- a) -> runCommand (f (pure a) (Coterm b)))

instance SQ.Command Term Coterm Command where
  t .|. c = Command (eval t (coeval c))
