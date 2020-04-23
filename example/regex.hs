{-# LANGUAGE
    ApplicativeDo,
    ConstraintKinds,
    FlexibleContexts,
    FlexibleInstances,
    MonoLocalBinds,
    QuantifiedConstraints,
    UndecidableInstances #-}

import Control.Applicative
import Data.Char (isAlphaNum)
import Data.Foldable (for_, asum)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS

import Data.Attoparsec.Unparse hiding (Class)
import Profunctor.Monad.Partial
import qualified Profunctor.Monad.NewCombinators as PM

type Regex = AltRegex

newtype AltRegex = AltRegex { unAltRegex :: [SeqRegex] }
  deriving (Eq, Ord, Show)

newtype SeqRegex = SeqRegex { unSeqRegex :: [QuantifiedRegex] }
  deriving (Eq, Ord, Show)

data QuantifiedRegex
  = QuantifiedRegex AtomicRegex Quantifier
  deriving (Eq, Ord, Show)

data Quantifier = Star Greedy | Plus Greedy | Question Greedy | None
  deriving (Eq, Ord, Show)

data Greedy = Greedy | Lazy
  deriving (Eq, Ord, Show)

unQuantify :: QuantifiedRegex -> AtomicRegex
unQuantify (QuantifiedRegex e _) = e

quantifier :: QuantifiedRegex -> Quantifier
quantifier (QuantifiedRegex _ q) = q

data AtomicRegex
  = Char Word8
  | Group Regex
  | Caret
  | Dollar
  | Dot
  | GClass GClass
  | Class ClassType [ClassAtom]
  deriving (Eq, Ord, Show)

isGroup :: AtomicRegex -> Bool
isGroup (Group _) = True
isGroup _ = False

groupBody :: AtomicRegex -> Regex
groupBody (Group e) = e
groupBody c = error $ "groupBody: " ++ show c

isClass :: AtomicRegex -> Bool
isClass (Class _ _) = True
isClass _ = False

classType :: AtomicRegex -> ClassType
classType (Class t _) = t
classType c = error $ "classType: " ++ show c

classAtoms :: AtomicRegex -> [ClassAtom]
classAtoms (Class _ a) = a
classAtoms c = error $ "classAtoms: " ++ show c

isChar :: AtomicRegex -> Bool
isChar (Char _) = True
isChar _ = False

char :: AtomicRegex -> Word8
char (Char c) = c
char c = error $ "char: " ++ show c

isGClass :: AtomicRegex -> Bool
isGClass (GClass _) = True
isGClass _ = False

unGClass :: AtomicRegex -> GClass
unGClass (GClass g) = g
unGClass c = error $ "unGClass: " ++ show c

data ClassType = Include | Exclude
  deriving (Eq, Ord, Show)

isExclude :: ClassType -> Bool
isExclude Exclude = True
isExclude _ = False

data ClassAtom = CRange Word8 Word8 | CChar Word8 | CGClass GClass
  deriving (Eq, Ord, Show)

isCRange :: ClassAtom -> Bool
isCRange (CRange _ _) = True
isCRange _ = False

cRangeStart :: ClassAtom -> Word8
cRangeStart (CRange start _) = start
cRangeStart c = error $ "cRangeStart: " ++ show c

cRangeEnd :: ClassAtom -> Word8
cRangeEnd (CRange _ end) = end
cRangeEnd c = error $ "cRangeEnd: " ++ show c

isCChar :: ClassAtom -> Bool
isCChar (CChar _) = True
isCChar _ = False

unCChar :: ClassAtom -> Word8
unCChar (CChar c) = c
unCChar c = error $ "unCChar: " ++ show c

unCGClass :: ClassAtom -> GClass
unCGClass (CGClass g) = g
unCGClass c = error $ "unCGClass: " ++ show c

data GClass
  = Digit
  | NotDigit
  | Space
  | NotSpace
  | Word
  | NotWord
  deriving (Eq, Ord, Show)

class    (Attoparsec p, forall x. Alternative (p x)) => RegexParser p
instance (Attoparsec p, forall x. Alternative (p x)) => RegexParser p

regex :: RegexParser p => J p Regex
regex = regex_ <* endOfInput

regex_ :: RegexParser p => J p Regex
regex_ = altRegex_

altRegex_ :: RegexParser p => J p AltRegex
altRegex_ = AltRegex <$> unAltRegex =. sepByP seqRegex_ (word8 (c_ '|'))

seqRegex_ :: RegexParser p => J p SeqRegex
seqRegex_ = SeqRegex <$> unSeqRegex =. manyP quantifiedRegex_

quantifiedRegex_ :: RegexParser p => J p QuantifiedRegex
quantifiedRegex_ = do
  e <- unQuantify =. atomicRegex_
  q <- quantifier =. quantifier_
  return (QuantifiedRegex e q)

quantifier_ :: RegexParser p => J p Quantifier
quantifier_ =
  asum (do
    (s, q) <-
      [ ("*?", Star Lazy), ("+?", Plus Lazy), ("??", Question Lazy)
      , ("*", Star Greedy), ("+", Plus Greedy), ("?", Question Greedy)
      ]
    return $
      assert "quantifier" (== q) *>
      string (BS8.pack s) *>
      pure q
  ) <|> pure None

atomicRegex_ :: RegexParser p => J p AtomicRegex
atomicRegex_ =
  special_ <|> group_ <|> charClass_ <|> gClassE_ <|> char_

group_ :: RegexParser p => J p AtomicRegex
group_ = do
  _ <- assert "group" isGroup
  _ <- word8 (c_ '(')
  e <- groupBody =. regex_
  _ <- word8 (c_ ')')
  return (Group e)

charClass_ :: RegexParser p => J p AtomicRegex
charClass_ = do
  _ <- assert "class" isClass
  _ <- word8 (c_ '[')
  t <- classType =. classType_
  e <- classAtoms =. manyP classAtom_
  _ <- word8 (c_ ']')
  return (Class t e)

classType_ :: RegexParser p => J p ClassType
classType_ = exclude_ <|> include_

include_ :: RegexParser p => J p ClassType
include_ = pure Include

exclude_ :: RegexParser p => J p ClassType
exclude_ = do
  _ <- assert "exclude" isExclude
  _ <- word8 (c_ '^')
  return Exclude

classAtom_ :: RegexParser p => J p ClassAtom
classAtom_ = cRange_ <|> cChar_ <|> cGClass_

cRange_ :: RegexParser p => J p ClassAtom
cRange_ = do
  _ <- assert "range" isCRange
  start <- cRangeStart =. char0_
  _ <- word8 (c_ '-')
  end <- cRangeEnd =. char0_
  return (CRange start end)

cChar_ :: RegexParser p => J p ClassAtom
cChar_ = do
  _ <- assert "cchar" isCChar
  c <- unCChar =. char0_
  return (CChar c)

cGClass_ :: RegexParser p => J p ClassAtom
cGClass_ = CGClass <$> unCGClass =. gClass_

char_ :: RegexParser p => J p AtomicRegex
char_ = do
  _ <- assert "char" isChar
  c <- char =. char0_
  return (Char c)

char0_ :: RegexParser p => J p Word8
char0_ = satisfy alphaNum <|> escapedChar_ (not . alphaNum)
  where
    alphaNum = isAlphaNum . toEnum . fromEnum

escapedChar_ :: RegexParser p => (Word8 -> Bool) -> J p Word8
escapedChar_ p =
  word8 (c_ '\\') *> satisfy p

gClassE_ :: RegexParser p => J p AtomicRegex
gClassE_ = do
  _ <- assert "gclass" isGClass
  g <- unGClass =. gClass_
  return (GClass g)

gClass_ :: RegexParser p => J p GClass
gClass_ =
  word8 (c_ '\\') *>
  asum (do
    (c, g) <-
      [ ('d', Digit), ('D', NotDigit)
      , ('s', Space), ('S', NotSpace)
      , ('w', Word ), ('W', NotWord )
      ]
    return $ do
      _ <- assert "class" (== g)
      _ <- word8 (c_ c)
      return g)

special_ :: RegexParser p => J p AtomicRegex
special_ = asum $ do
  (c, e) <- [ ('.', Dot), ('$', Dollar), ('^', Caret) ]
  return $ do
    _ <- assert "special" (== e)
    _ <- word8 (c_ c)
    return e

c_ :: Char -> Word8
c_ = fromIntegral . fromEnum

manyP :: RegexParser p => J p a -> J p [a]
manyP = PM.manyP (assert "empty list")

sepByP :: RegexParser p => J p a -> p () b -> J p [a]
sepByP = PM.sepByP (assert "empty list")

examples :: [BS.ByteString]
examples = fmap BS8.pack
  [ ""
  , "^ab(c[\\def])|g?h*?(i(jk)+?|l|)\\s\\w[^\\]].$"
  ]

main :: IO ()
main = for_ examples $ \s -> do
  v <- unwrap $ parse regex s
  s'_ <- unwrap $ unparse regex v
  let s' = LBS.toStrict s'_
  v' <- unwrap $ parse regex s'
  assertEqual v v' $ do
    BS8.putStrLn s
    BS8.putStrLn s'

unwrap :: Either String b -> IO b
unwrap (Right b) = pure b
unwrap (Left a) = fail a

assertEqual :: (Show a, Eq a) => a -> a -> IO () -> IO ()
assertEqual a a' ifFail =
  if a == a' then
    pure ()
  else do
    putStrLn "Not equal:"
    print a
    print a'
    ifFail
    fail "Failed"
