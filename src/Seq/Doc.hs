{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Doc
( -- * Documents
  Document(..)
, putDoc
, Doc(..)
  -- * Variable binding
, Var(..)
, Bind(..)
, var
, Binding(..)
  -- * Indentation
, Indent(..)
  -- * Combinators
, str
, (<+>)
, parens
, brackets
, surround
, enclose
, encloseSep
, sepBy
, list
, tupled
, parensIf
, hsep
, concatWith
  -- * Constants
, dot
, comma
, pipe
  -- ** Whitespace
, space
, line
, line'
  -- ** Bracketing
, lparen
, rparen
, lbracket
, rbracket
  -- * Precedence
, Prec(..)
, Precedence(..)
, resetPrec
) where

import Data.Semigroup (mtimesDefault)

-- Documents

class Monoid d => Document d where
  char :: Char -> d

  group :: d -> d
  group = id

  flatAlt :: d -> d -> d
  flatAlt = const

  indent :: Indent -> d -> d
  indent _ = id

  hardline :: d
  hardline = char '\n'

  enclosing
    :: d -- ^ left
    -> d -- ^ right
    -> d -- ^ separator
    -> d
  enclosing = enclose

  enclosingSep
    :: d   -- ^ left doc
    -> d   -- ^ right doc
    -> d   -- ^ separator
    -> [d] -- ^ elements
    -> d
  enclosingSep = encloseSep


putDoc :: Doc -> IO ()
putDoc = putStrLn . show

withIndentation :: (Indent -> Doc) -> Doc
withIndentation f = Doc (\ k i -> getDoc (f i) k i)

newtype Doc = Doc { getDoc :: (Indent -> ShowS) -> (Indent -> ShowS) }

instance Show Doc where
  showsPrec _ d = getDoc d (const id) mempty

instance Semigroup Doc where
  a <> b = Doc (getDoc b . getDoc a)

instance Monoid Doc where
  mempty = Doc id

instance Document Doc where
  char c = Doc (\ k i s -> k i (c:s))
  indent i d = Doc (\ k i' -> getDoc d k (i <> i'))
  hardline = withIndentation (\ (Indent i) -> char '\n' <> mtimesDefault i space)


-- Variable binding

newtype Var = Var Int
  deriving (Enum, Eq, Ord, Show)

newtype Bind doc = Bind { getBind :: Var -> doc }
  deriving (Monoid, Semigroup)

var :: Document d => Var -> d
var (Var i) = str $ alphabet !! r : if q > 0 then show q else ""
  where
  n = length alphabet
  (q, r) = i `divMod` n

alphabet :: String
alphabet = ['a'..'z']


class Document d => Binding d where
  bind :: (Var -> d) -> d

instance Document doc => Binding (Bind doc) where
  bind f = Bind $ \ v -> let Bind p = f v in p (succ v)

instance Document doc => Document (Bind doc) where
  char = Bind . const . char


-- Indentation

newtype Indent = Indent { getIndent :: Int }

instance Semigroup Indent where
  Indent i1 <> Indent i2 = Indent (i1 + i2)

instance Monoid Indent where
  mempty = Indent 0


-- Combinators

str :: Document d => String -> d
str = foldMap char

(<+>) :: Document d => d -> d -> d
p <+> q = p <> char ' ' <> q

parens :: Document d => d -> d
parens = enclosing lparen rparen

brackets :: Document d => d -> d
brackets = enclosing lbracket rbracket

surround
  :: Semigroup d
  => d -- ^ middle doc
  -> d -- ^ left doc
  -> d -- ^ right doc
  -> d
surround x l r = enclose l r x

enclose
  :: Semigroup d
  => d -- ^ left doc
  -> d -- ^ right doc
  -> d -- ^ middle doc
  -> d
enclose l r x = l <> x <> r

encloseSep
  :: Document d
  => d   -- ^ left doc
  -> d   -- ^ right doc
  -> d   -- ^ separator
  -> [d] -- ^ elements
  -> d
encloseSep l r s = enclose l r . sepBy s

sepBy :: Document d => d -> [d] -> d
sepBy sep = concatWith (surround (line' <> sep))

list :: Document d => [d] -> d
list = enclosingSep lbracket rbracket (comma <> space)

tupled :: Document d => [d] -> d
tupled = enclosingSep lparen rparen (comma <> space)

parensIf :: Document d => Bool -> d -> d
parensIf False = id
parensIf True  = parens

hsep :: Document d => [d] -> d
hsep = concatWith (<+>)

concatWith :: (Foldable t, Monoid d) => (d -> d -> d) -> t d -> d
concatWith (<>) ps
  | null ps   = mempty
  | otherwise = foldr1 (<>) ps


-- Constants

dot, comma, pipe :: Document d => d
dot = char '·'
comma = char ','
pipe = char '|'


-- Whitespace

space, line, line' :: Document d => d
space = char ' '
line = flatAlt hardline space
line' = flatAlt hardline mempty


-- Bracketing

lparen, rparen :: Document d => d
lparen = char '('
rparen = char ')'

lbracket, rbracket :: Document d => d
lbracket = char '['
rbracket = char ']'


-- Precedence

newtype Prec level doc = Prec { getPrec :: level -> doc }
  deriving (Monoid, Semigroup)

instance (Bounded level, Show doc) => Show (Prec level doc) where
  showsPrec d p = showsPrec d (getPrec p minBound)


class Document doc => Precedence doc level p | p -> doc level where
  atom :: doc -> p
  prec :: level -> doc -> p
  withPrec :: level -> p -> doc
  localPrec :: (level -> level) -> p -> p

instance (Document doc, Ord level) => Precedence doc level (Prec level doc) where
  atom = Prec . const
  prec i b = Prec (\ i' -> parensIf (i' > i) b)
  withPrec = flip getPrec
  localPrec f p = Prec (getPrec p . f)

resetPrec :: (Precedence doc level p, Bounded level) => p -> doc
resetPrec = withPrec minBound
