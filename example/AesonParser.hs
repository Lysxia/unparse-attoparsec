-- | Compare this module with aeson's 'Data.Aeson.Parser.Internal'.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AesonParser where

import Data.Aeson (Value (..), Object, Array, encode)
import qualified Data.Aeson.Parser as Aeson
import Data.Char (ord, chr)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Lazy as H
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word8)
import Data.Profunctor
import GHC.Exts (Constraint)

import Prelude hiding (head, tail)

import Profunctor.Monad.Partial

import qualified Data.Attoparsec.Unparse as A
import qualified Data.Attoparsec.Unparse.Printer as AP

pattern C :: Char -> Word8
pattern C c <- (chr . fromIntegral -> c) where
  C c = fromIntegral (ord c)

type AesonParser p = (A.Attoparsec p, Foreach Monad p As)

type family Foreach c (p :: * -> * -> *) as :: Constraint where
  Foreach c p '[] = ()
  Foreach c p (a ': as) = (c (p a), Foreach c p as)

type As = '[Object, Text, Value, [(Text, Value)], [Value]]

object_ :: AesonParser p => J p Object
object_ = objectValues jstring0 value

objectValues :: AesonParser p => J p Text -> J p Value -> J p Object
objectValues str val =
  H.toList =. do
    skipSpace
    w <- nextWord (C '"') =. A.peekWord8'
    case w of
      C '}' -> const w =. A.anyWord8 *> pure H.empty
      _ {- '"' or error -} -> loop []
  where
    loop acc = do
      k <- headM =.? fst =. str <* skipSpace <* A.word8 (C ':')
      v <- headM =.? snd =. val <* skipSpace
      tailM =.? do
        ch <- nextWord (C ',') =. A.satisfy (\w -> w == C ',' || w == C '}')
        let acc' = (k, v) : acc
        case ch of
          C ',' -> skipSpace *> loop acc'
          _ {- '}' -} -> pure (H.fromList acc')

    nextWord _ [] = C '}'
    nextWord c (_ : _) = c  -- Next character if not empty

array_ :: AesonParser p => J p Array
array_ = arrayValues value

arrayValues :: AesonParser p => J p Value -> J p Array
arrayValues val =
  V.toList =. do
    skipSpace
    w <- nextWordCloseSquare =. A.peekWord8Class' (== C ']')
    case w of
      True {- ']' -} ->
        const (C ']') =. A.anyWord8 *> pure V.empty
      False -> loop [] 1
  where
    loop acc !len = do
      v <- headM =.? val <* skipSpace
      tailM =.? do
        ch <- nextWord' =. A.satisfy (\w -> w == C ',' || w == C ']')
        let acc' = v : acc
        case ch of
          C ',' -> skipSpace *> loop acc' (len + 1)
          _ {- ']' -} -> pure (V.reverse (V.fromListN len acc'))

    nextWordCloseSquare [] = True
    nextWordCloseSquare (_ : _) = False

    nextWord' [] = C ']'
    nextWord' _ = C ','

value :: AesonParser p => J p Value
value = do
  skipSpace
  w <- firstWordClass =. A.unsafePeekWord8Class'
  case w of
    C '"' -> aString jstring0
    -- C '"' -> const w =. A.anyWord8 *> aString jstring_
    C '{' -> const w =. A.anyWord8 *> aObject object_
    C '[' -> const w =. A.anyWord8 *> aArray array_
    C 'f' -> A.string "false" *> pure (Bool False)
    C 't' -> A.string "true" *> pure (Bool True)
    C 'n' -> A.string "null" *> pure Null
    _ | w >= 48 && w <= 57 || w == 45 -> aNumber scientific
    _ -> fail "not a valid json value"
  where
    firstWordClass v = let c = A.singleton . C in case v of
      String _ -> c '"'
      Object _ -> c '{'
      Array _ -> c '['
      Bool False -> c 'f'
      Bool True -> c 't'
      Null -> c 'n'
      Number _ -> A.Class (\w -> w >= 48 && w <= 57 || w == 45) 45

    aString = dimap (\(String s) -> s) String
    aObject = dimap (\(Object o) -> o) Object
    aArray  = dimap (\(Array  a) -> a) Array
    aNumber = dimap (\(Number n) -> n) Number

-- | Parse a quoted JSON string.
jstring :: AesonParser p => J p Text
jstring = A.word8 (C '"') *> jstring_

jstring0 :: AesonParser p => J p Text
jstring0 = A.parseOrPrint Aeson.jstring $ \t ->
  AP.seeLazyBS (encode t)

data EscapeState = Escape | NoEscape

-- | Parse a quoted JSON string without the leading quote.
jstring_ :: AesonParser p => J p Text
jstring_ = do
  s <- escapeText =. A.scan startState go <* A.word8 (C '"')
  case unescapeText s of
    Right r -> pure r
    Left err -> fail err
  where
    startState = NoEscape  -- seen backslash
    go Escape _ = Just NoEscape
    go _ (C '"') = Nothing
    go _ (C '\\') = Just Escape
    go _ _ = Just NoEscape

skipSpace :: AesonParser p => p x ()
skipSpace = const "" =. A.skipWhile isSpace
  where
    isSpace w = w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

data SP = SP !Integer {-# UNPACK #-}!Int

-- Undefined because the bidirectional programming payoff is low.

unescapeText :: ByteString -> Either String Text
unescapeText = undefined

escapeText :: Text -> ByteString
escapeText = undefined

scientific :: AesonParser p => J p Scientific
scientific = A.parseOrPrint undefined undefined

-- Find a home for this

headM :: [a] -> Maybe a
headM [] = Nothing
headM (x : _) = Just x

tailM :: [a] -> Maybe [a]
tailM [] = Nothing
tailM (_ : xs) = Just xs
