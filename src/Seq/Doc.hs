{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Doc
( -- * Documents
  Document(..)
, runDoc
, putDoc
, Doc(..)
  -- * Efficient strings
, dchar
, dstring
, DString(..)
  -- * Variable binding
, Var(..)
, Bind(..)
, var
, Binding(..)
  -- * Indentation
, Indent(..)
, Column(..)
  -- * Combinators
, str
, (<+>)
, (</>)
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
, vsep
, concatWith
, spaces
, align
, hang
, indent
  -- * Constants
, dot
, middot
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

  nest :: Indent -> d -> d
  nest _ = id

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

  withIndentation :: (Indent -> d) -> d
  withColumn :: (Column -> d) -> d


runDoc :: (Column -> Indent -> DString) -> Column -> Indent -> Doc -> DString
runDoc k c i d = getDoc d k c i

putDoc :: Doc -> IO ()
putDoc = putStrLn . show

newtype Doc = Doc { getDoc :: (Column -> Indent -> DString) -> (Column -> Indent -> DString) }

instance Show Doc where
  showsPrec _ d = getDString (runDoc (const (const mempty)) (Column 0) mempty d)

instance Semigroup Doc where
  Doc a <> Doc b = Doc (\ k -> a (b k))

instance Monoid Doc where
  mempty = Doc id

instance Document Doc where
  char c = Doc (\ k col i -> dchar c <> k (col <> Column 1) i)
  nest (Indent 0) d = d
  nest i d          = Doc (\ k c i' -> runDoc (\ c _ -> k c i') c (Indent (getIndent i + getIndent i')) d)
  hardline = Doc (\ k _ (Indent i) -> dstring ('\n':replicate i ' ') <> k (Column i) (Indent i))
  withIndentation f = Doc (\ k c i -> runDoc k c i (f i))
  withColumn f = Doc (\ k c i -> runDoc k c i (f c))


-- Efficient strings

dchar :: Char -> DString
dchar c = DString (c:)

dstring :: String -> DString
dstring s = DString (s <>)

newtype DString = DString { getDString :: ShowS }

instance Semigroup DString where
  DString s1 <> DString s2 = DString (s1 <> s2)

instance Monoid DString where
  mempty = DString id


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
  withIndentation f = Bind (\ v -> withIndentation (\ i -> getBind (f i) v))
  withColumn f = Bind (\ v -> withColumn (\ i -> getBind (f i) v))


-- Indentation

newtype Indent = Indent { getIndent :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup Indent where
  Indent i1 <> Indent i2 = Indent (i1 + i2)

instance Monoid Indent where
  mempty = Indent 0

newtype Column = Column { getColumn :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup Column where
  Column i1 <> Column i2 = Column (i1 + i2)

instance Monoid Column where
  mempty = Column 0


-- Combinators

str :: Document d => String -> d
str = foldMap char

(<+>) :: Document d => d -> d -> d
p <+> q = p <> space <> q

infixr 6 <+>

(</>) :: Document d => d -> d -> d
p </> q = p <> line <> q

infixr 5 </>

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

hsep, vsep :: Document d => [d] -> d
hsep = concatWith (<+>)
vsep = concatWith (</>)

concatWith :: (Foldable t, Monoid d) => (d -> d -> d) -> t d -> d
concatWith (<>) ps
  | null ps   = mempty
  | otherwise = foldr1 (<>) ps

spaces :: Document d => Int -> d
spaces i
  | i <= 0    = mempty
  | otherwise = mtimesDefault i space

align :: Document d => d -> d
align d = withColumn (\ c -> withIndentation (\ i -> nest (Indent (getColumn c - getIndent i)) d))

hang :: Document d => Indent -> d -> d
hang i d = align (nest i d)

indent :: Document d => Indent -> d -> d
indent i d = hang i (spaces (getIndent i) <> d)


-- Constants

dot, middot, comma, pipe :: Document d => d
dot = char '.'
middot = char '·'
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
