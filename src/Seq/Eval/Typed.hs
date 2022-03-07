module Seq.Eval.Typed
( Eval(..)
) where

newtype Eval a = Eval { eval :: a }
