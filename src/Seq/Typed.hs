{-# LANGUAGE FunctionalDependencies #-}
module Seq.Typed
( Seq
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
