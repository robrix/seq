{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Doc
( Var(..)
, Doc(..)
, Document(..)
, DString(..)
, str
, (<+>)
, parens
, brackets
, bind
, var
, lambda
, space
, dot
, comma
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

newtype Doc = Doc { getDoc :: Var -> DString }
  deriving (Monoid, Semigroup)

newtype DString = DString { string :: ShowS }

instance Semigroup DString where
  a <> b = DString (string a . string b)

instance Monoid DString where
  mempty = DString id


class Monoid d => Document d where
  char :: Char -> d

instance Document Doc where
  char c = Doc (\ _ -> DString (c:))

str :: Document d => String -> d
str = foldMap char

(<+>) :: Doc -> Doc -> Doc
p <+> q = p <> char ' ' <> q

parens :: Doc -> Doc
parens = enclose lparen rparen

brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

bind :: (Var -> Doc) -> Doc
bind f = Doc $ \ v -> let Doc p = f v in p (succ v)

var :: Var -> Doc
var (Var i) = str $ alphabet !! r : if q > 0 then show q else ""
  where
  n = length alphabet
  (q, r) = i `divMod` n

alphabet :: String
alphabet = ['a'..'z']

lambda :: (Var -> Doc) -> Doc
lambda f = bind (\ v -> char 'λ' <+> var v <+> char '.' <+> f v)

space :: Document d => d
space = char ' '

dot :: Document d => d
dot = char '·'

comma :: Document d => d
comma = char ','

line' :: Document d => d
line' = char '\n'

lbracket :: Document d => d
lbracket = char '['

rbracket :: Document d => d
rbracket = char ']'

lparen :: Document d => d
lparen = char '('

rparen :: Document d => d
rparen = char ')'

surround
  :: Doc -- ^ middle doc
  -> Doc -- ^ left doc
  -> Doc -- ^ right doc
  -> Doc
surround x l r = enclose l r x

enclose
  :: Document d
  => d -- ^ left doc
  -> d -- ^ right doc
  -> d -- ^ middle doc
  -> d
enclose l r x = l <> x <> r

encloseSep
  :: Doc   -- ^ left doc
  -> Doc   -- ^ right doc
  -> Doc   -- ^ separator
  -> [Doc] -- ^ elements
  -> Doc
encloseSep l r s ps = enclose l r (concatWith (surround (line' <> s)) ps)

list :: [Doc] -> Doc
list = encloseSep lbracket rbracket (comma <> space)

tupled :: [Doc] -> Doc
tupled = encloseSep lparen rparen (comma <> space)

parensIf :: Bool -> Doc -> Doc
parensIf False = id
parensIf True  = parens

hsep :: [Doc] -> Doc
hsep = concatWith (<+>)

concatWith :: Foldable t => (Doc -> Doc -> Doc) -> t Doc -> Doc
concatWith (<>) ps
  | null ps   = mempty
  | otherwise = foldr1 (<>) ps
