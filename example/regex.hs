{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Applicative
import Data.Char (isAlphaNum)
import Data.Foldable (for_, asum)
import Data.Word (Word8)
import Prelude hiding (group)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS

import Data.Attoparsec.Unparse hiding (Class)
import Profunctor.Monad

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

isClass :: AtomicRegex -> Bool
isClass (Class _ _) = True
isClass _ = False

classType :: AtomicRegex -> ClassType
classType (Class t _) = t

classAtoms :: AtomicRegex -> [ClassAtom]
classAtoms (Class _ a) = a

isChar :: AtomicRegex -> Bool
isChar (Char _) = True
isChar _ = False

char :: AtomicRegex -> Word8
char (Char c) = c

isGClass :: AtomicRegex -> Bool
isGClass (GClass _) = True
isGClass _ = False

unGClass :: AtomicRegex -> GClass
unGClass (GClass g) = g

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

cRangeEnd :: ClassAtom -> Word8
cRangeEnd (CRange _ end) = end

isCChar :: ClassAtom -> Bool
isCChar (CChar _) = True
isCChar _ = False

unCChar :: ClassAtom -> Word8
unCChar (CChar c) = c

unCGClass :: ClassAtom -> GClass
unCGClass (CGClass g) = g

data GClass
  = Digit
  | NotDigit
  | Space
  | NotSpace
  | Word
  | NotWord
  deriving (Eq, Ord, Show)

type RegexParser p = (Attoparsec p, Alternative1 p)

regex_ :: RegexParser p => J p Regex
regex_ = altRegex_

altRegex_ :: RegexParser p => J p AltRegex
altRegex_ = withFunctor $ AltRegex <$> unAltRegex =. sepP (word8 (c_ '|')) seqRegex_

seqRegex_ :: RegexParser p => J p SeqRegex
seqRegex_ = withFunctor $ SeqRegex <$> unSeqRegex =. manyP quantifiedRegex_

quantifiedRegex_ :: RegexParser p => J p QuantifiedRegex
quantifiedRegex_ = withApplicative $ do
  e <- unQuantify =. atomicRegex_
  q <- quantifier =. quantifier_
  return (QuantifiedRegex e q)

quantifier_ :: RegexParser p => J p Quantifier
quantifier_ = withAlternative $
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
atomicRegex_ = withAlternative $
  special_ <|> group_ <|> charClass_ <|> gClassE_ <|> char_

group_ :: RegexParser p => J p AtomicRegex
group_ = withApplicative $ do
  _ <- assert "group" isGroup
  _ <- word8 (c_ '(')
  e <- groupBody =. regex_
  _ <- word8 (c_ ')')
  return (Group e)

charClass_ :: RegexParser p => J p AtomicRegex
charClass_ = withApplicative $ do
  _ <- assert "class" isClass
  _ <- word8 (c_ '[')
  t <- classType =. classType_
  e <- classAtoms =. manyP (classAtom_ False)
  _ <- word8 (c_ ']')
  return (Class t e)

classType_ :: RegexParser p => J p ClassType
classType_ = withAlternative $ exclude_ <|> include_

include_ :: RegexParser p => J p ClassType
include_ = withApplicative $ pure Include

exclude_ :: RegexParser p => J p ClassType
exclude_ = withApplicative $ do
  _ <- assert "exclude" isExclude
  _ <- word8 (c_ '^')
  return Exclude

classAtom_ :: RegexParser p => Bool -> J p ClassAtom
classAtom_ first = withAlternative $ cRange_ first <|> cChar_ first <|> cGClass_

cRange_ :: RegexParser p => Bool -> J p ClassAtom
cRange_ first = withApplicative $ do
  _ <- assert "range" isCRange
  start <- cRangeStart =. char0_
  _ <- word8 (c_ '-')
  end <- cRangeEnd =. char0_
  return (CRange start end)

cChar_ :: RegexParser p => Bool -> J p ClassAtom
cChar_ first = withApplicative $ do
  _ <- assert "cchar" isCChar
  c <- unCChar =. char0_
  return (CChar c)

cGClass_ :: RegexParser p => J p ClassAtom
cGClass_ = withFunctor $ CGClass <$> unCGClass =. gClass_

char_ :: RegexParser p => J p AtomicRegex
char_ = withApplicative $ do
  _ <- assert "char" isChar
  c <- char =. char0_
  return (Char c)

char0_ :: RegexParser p => J p Word8
char0_ = withAlternative $
  satisfy alphaNum <|> escapedChar_ (not . alphaNum)
  where
    alphaNum = isAlphaNum . toEnum . fromEnum

escapedChar_ :: RegexParser p => (Word8 -> Bool) -> J p Word8
escapedChar_ p = withApplicative $
  word8 (c_ '\\') *> satisfy p

gClassE_ :: RegexParser p => J p AtomicRegex
gClassE_ = withApplicative $ do
  _ <- assert "gclass" isGClass
  g <- unGClass =. gClass_
  return (GClass g)

gClass_ :: RegexParser p => J p GClass
gClass_ = withAlternative $
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
special_ = withAlternative $ asum $ do
  (c, e) <- [ ('.', Dot), ('$', Dollar), ('^', Caret) ]
  return $ do
    _ <- assert "special" (== e)
    _ <- word8 (c_ c)
    return e

sepP
  :: RegexParser p
  => p () b -> p x a -> p [x] [a]
sepP s p = withAlternative $
  (assert "empty list" (not . null) *> liftA2 (:) (head =. p) (tail =. sepP' s p))
  <|> pure []

sepP'
  :: RegexParser p
  => p () b -> p x a -> p [x] [a]
sepP' s p = manyP (withApplicative $ const () =. s *> p)

manyP
  :: RegexParser p
  => p x a -> p [x] [a]
manyP p = withAlternative $ someP p <|> pure []

someP
  :: RegexParser p
  => p x a -> p [x] [a]
someP p = withAlternative $ do
  () <- assert "empty list" (not . null)
  a <- head =. p
  as <- tail =. manyP p
  return (a : as)

c_ :: Char -> Word8
c_ = fromIntegral . fromEnum

examples :: [BS.ByteString]
examples = fmap BS8.pack
  [ ""
  , "^ab(c[\\def])|g?h*?(i(jk)+?|l|)\\s\\w[^\\]].$"
  ]

main :: IO ()
main = for_ examples $ \s -> do
  BS8.putStrLn s
  v <- unwrap $ parse regex_ s
  print v
  s'_ <- unwrap $ unparse regex_ v
  let s' = LBS.toStrict s'_
  v' <- unwrap $ parse regex_ s'
  assertEqual v v'
  BS8.putStrLn s'

unwrap :: Either String b -> IO b
unwrap (Right b) = pure b
unwrap (Left a) = fail a

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a a' =
  if a == a' then
    pure ()
  else
    fail $ "Not equal: " ++ show (a, a')
