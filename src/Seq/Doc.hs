{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Seq.Doc
( Var(..)
, Doc(..)
, DString(..)
, char
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

char :: Char -> Doc
char c = Doc (\ _ -> DString (c:))

str :: String -> Doc
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

space :: Doc
space = char ' '

dot :: Doc
dot = char '·'

comma :: Doc
comma = char ','

line' :: Doc
line' = char '\n'

lbracket :: Doc
lbracket = char '['

rbracket :: Doc
rbracket = char ']'

lparen :: Doc
lparen = char '('

rparen :: Doc
rparen = char ')'

surround
  :: Doc -- ^ middle doc
  -> Doc -- ^ left doc
  -> Doc -- ^ right doc
  -> Doc
surround x l r = enclose l r x

enclose
  :: Doc -- ^ left doc
  -> Doc -- ^ right doc
  -> Doc -- ^ middle doc
  -> Doc
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

parensIf :: Bool -> Doc -> Doc
parensIf False = id
parensIf True  = parens

hsep :: [Doc] -> Doc
hsep = concatWith (<+>)

concatWith :: Foldable t => (Doc -> Doc -> Doc) -> t Doc -> Doc
concatWith (<>) ps
  | null ps   = mempty
  | otherwise = foldr1 (<>) ps
