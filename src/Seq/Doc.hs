{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Doc
( Var(..)
, Bind(..)
, Doc(..)
, DString(..)
, bind
, var
, lambda
, Document(..)
, str
, (<+>)
, parens
, brackets
, space
, dot
, comma
, hardline
, line
, line'
, lparen
, rparen
, lbracket
, rbracket
, surround
, enclose
, encloseSep
, list
, tupled
, parensIf
, hsep
, concatWith
) where

newtype Var = Var Int
  deriving (Enum, Eq, Ord, Show)

newtype Bind = Bind { getBind :: Var -> Doc }
  deriving (Monoid, Semigroup)

newtype Doc = Doc { getDoc :: Var -> DString }
  deriving (Monoid, Semigroup)

newtype DString = DString { string :: ShowS }

instance Semigroup DString where
  a <> b = DString (string a . string b)

instance Monoid DString where
  mempty = DString id

bind :: (Var -> Doc) -> Doc
bind f = Doc $ \ v -> let Doc p = f v in p (succ v)

var :: Document d => Var -> d
var (Var i) = str $ alphabet !! r : if q > 0 then show q else ""
  where
  n = length alphabet
  (q, r) = i `divMod` n

alphabet :: String
alphabet = ['a'..'z']

lambda :: (Var -> Doc) -> Doc
lambda f = bind (\ v -> char 'λ' <+> var v <+> char '.' <+> f v)


class Monoid d => Document d where
  char :: Char -> d

  group :: d -> d
  group = id
  flatAlt :: d -> d -> d
  flatAlt = const

  enclosing :: d -> d -> d -> d
  enclosing = enclose

  enclosingSep
    :: d   -- ^ left doc
    -> d   -- ^ right doc
    -> d   -- ^ separator
    -> [d] -- ^ elements
    -> d
  enclosingSep = encloseSep

instance Document DString where
  char = DString . (:)

instance Document Doc where
  char = Doc . const . char

instance Document Bind where
  char = Bind . const . char

str :: Document d => String -> d
str = foldMap char

(<+>) :: Document d => d -> d -> d
p <+> q = p <> char ' ' <> q

parens :: Document d => d -> d
parens = enclosing lparen rparen

brackets :: Document d => d -> d
brackets = enclosing lbracket rbracket

space :: Document d => d
space = char ' '

dot :: Document d => d
dot = char '·'

comma :: Document d => d
comma = char ','

hardline :: Document d => d
hardline = char '\n'

line :: Document d => d
line = flatAlt hardline space

line' :: Document d => d
line' = flatAlt hardline mempty

lbracket :: Document d => d
lbracket = char '['

rbracket :: Document d => d
rbracket = char ']'

lparen :: Document d => d
lparen = char '('

rparen :: Document d => d
rparen = char ')'

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
encloseSep l r s ps = enclose l r (concatWith (surround (line' <> s)) ps)

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
