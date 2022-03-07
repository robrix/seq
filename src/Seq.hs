{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Seq
( Seq(..)
, Print(..)
, Doc(..)
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
, surround
, enclose
, parensIf
, hsep
, concatWith
, atom
, prec
, withPrec
, ($$)
, assocl
, assocr
) where

class Seq term coterm command | term -> coterm command, coterm -> term command, command -> term coterm where
  -- right rules
  prdR :: term -> term -> term
  sumR1 :: term -> term
  sumR2 :: term -> term
  funR :: (term -> coterm -> command) -> term

  -- left rules
  prdL1 :: coterm -> coterm
  prdL2 :: coterm -> coterm
  sumL :: coterm -> coterm -> coterm
  funL :: term -> coterm -> coterm

  (.|.) :: term -> coterm -> command

  infix 1 .|.

newtype Var = Var Int
  deriving (Enum, Eq, Ord, Show)

newtype Prec = Prec Int
  deriving (Eq, Num, Ord)

newtype Print = Print { getPrint :: Prec -> Doc }
  deriving (Monoid, Semigroup)

instance Show Print where
  showsPrec d p = string (getDoc (getPrint p (Prec d)) (Var 0))

newtype Doc = Doc { getDoc :: Var -> DString }
  deriving (Monoid, Semigroup)

instance Seq Print Print Print where
  prdR l r = prec 10 (str "inlr" <+> withPrec 11 l <+> withPrec 11 r)
  sumR1 l = prec 10 (str "inl" <+> withPrec 11 l)
  sumR2 r = prec 10 (str "inr" <+> withPrec 11 r)
  funR f = prec 0 (char 'λ' <+> bind (\ a -> bind (\ b -> brackets (var a <> comma <+> var b) <+> dot <+> withPrec 0 (f (atom (var a)) (atom (var b))))))

  prdL1 f = prec 10 (str "exl" <+> withPrec 11 f)
  prdL2 f = prec 10 (str "exr" <+> withPrec 11 f)
  sumL l r = prec 10 (str "exlr" <+> withPrec 11 l <+> withPrec 11 r)
  funL = assocr 10 dot

  t .|. c = prec 0 (withPrec 1 t <+> str "║" <+> withPrec 1 c)

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
parens = enclose (char '(') (char ')')

brackets :: Doc -> Doc
brackets = enclose (char '[') (char ']')

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

parensIf :: Bool -> Doc -> Doc
parensIf False = id
parensIf True  = parens

hsep :: [Doc] -> Doc
hsep = concatWith (<+>)

concatWith :: Foldable t => (Doc -> Doc -> Doc) -> t Doc -> Doc
concatWith (<>) ps
  | null ps   = mempty
  | otherwise = foldr1 (<>) ps



atom :: Doc -> Print
atom = Print . const

prec :: Prec -> Doc -> Print
prec i b = Print (\ i' -> parensIf (i' > i) b)

withPrec :: Prec -> Print -> Doc
withPrec = flip getPrint

($$) :: Print -> Print -> Print
($$) = assocl (Prec 10) space

infixl 9 $$

assocl
  :: Prec  -- ^ precedence
  -> Doc   -- ^ operator
  -> Print -- ^ left operand
  -> Print -- ^ right operand
  -> Print
assocl p o l r = prec p (surround o (withPrec p l) (withPrec (p + 1) r))

assocr
  :: Prec  -- ^ precedence
  -> Doc   -- ^ operator
  -> Print -- ^ left operand
  -> Print -- ^ right operand
  -> Print
assocr p o l r = prec p (surround o (withPrec (p + 1) l) (withPrec p r))
