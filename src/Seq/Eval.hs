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
import qualified Seq.Types as T

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


instance Mu Term Coterm Command where
  µR f = Term (runCommand . f . Coterm)
  µL f = Coterm (runCommand . f . pure)

instance Prd Term Coterm Command where
  prdR l r = Term (\ k -> k (T.Prd (\ k' -> k' (eval l) (eval r))))
  prdL1 k = Coterm (\ p -> T.πL p (coeval k))
  prdL2 k = Coterm (\ p -> T.πR p (coeval k))

instance Coprd Term Coterm Command where
  coprdR1 = fmap T.InL
  coprdR2 = fmap T.InR
  coprdL p q = Coterm (T.exlr (coeval p) (coeval q))

instance Pair Term Coterm Command where
  pairR (Term a) (Term b)  = Term (\ k -> a (\ a -> b (\ b -> k (T.Pair a b))))
  pairL f = Coterm (\ c -> runCommand (f (pure (T.pair1 c)) (pure (T.pair2 c))))

instance SQ.Term Term Coterm Command where
  notR = pure . T.Not . coeval
  copairR = either (\ a -> Term (\ k -> k (T.inL (eval a)))) (\ b -> Term (\ k -> k (T.inR (eval b))))
  funR f = pure (T.Fun (\ kb a -> runCommand (f (pure a) (Coterm kb))))
  cofunR a b = (coeval b T.:>-) <$> a

instance SQ.Coterm Term Coterm Command where
  copairL a b = Coterm (\ c -> T.copair c (coeval a) (coeval b))
  notL t = Coterm (eval t . T.runNot)
  funL a b = Coterm (\ f -> eval a (T.app f (coeval b)))
  cofunL f = Coterm (\ (b T.:>- a) -> runCommand (f (pure a) (Coterm b)))

instance SQ.Command Term Coterm Command where
  t .|. c = Command (eval t (coeval c))
