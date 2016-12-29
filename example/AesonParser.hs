-- | Compare this module with aeson's 'Data.Aeson.Parser.Internal'.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module AesonParser where

import Control.Monad.Fail (MonadFail)

import Data.Aeson (Value (..), Object, Array, encode)
import qualified Data.Aeson.Parser as Aeson
import Data.Char (ord, chr)
import Data.ByteString (ByteString)
import qualified Data.HashMap.Lazy as H
import Data.Scientific (Scientific)
import Data.String (fromString)  -- OverloadedStrings + RebindableSyntax
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word8)
import Prelude hiding (fail, (<$>), (<*>), (>>=), (>>), (<*), (*>), pure, return)

import Profunctor.Monad
import Profunctor.Monad.Combinators

import qualified Data.Attoparsec.Unparse as A
import qualified Data.Attoparsec.Unparse.Printer as AP

pattern C :: Char -> Word8
pattern C c <- (chr . fromIntegral -> c) where
  C c = fromIntegral (ord c)

type P p a = p a a
type AesonParser p =
  ( A.Attoparsec p
  , Monad1 p
  , ForallF MonadFail p
  )

object_ :: AesonParser p => P p Object
object_ = objectValues jstring0 value

objectValues :: AesonParser p => P p Text -> P p Value -> P p Object
objectValues str val =
  H.toList =. do
    skipSpace
    w <- nextWord (C '"') =. A.peekWord8'
    case w of
      C '"' -> loop []
      _ {- '}' -} -> const w =. A.anyWord8 *> pure H.empty
  where
    loop acc = do
      k <- head =. fst =. str <* skipSpace <* A.word8 (C ':')
      v <- head =. snd =. val <* skipSpace
      tail =. do
        ch <- nextWord (C ',') =. A.satisfy (\w -> w == C ',' || w == C '}')
        let acc' = (k, v) : acc
        case ch of
          C ',' -> skipSpace *> loop acc'
          _ {- '}' -} -> pure (H.fromList acc')

    nextWord _ [] = C '}'
    nextWord c (_ : _) = c  -- Next character if not empty

array_ :: AesonParser p => P p Array
array_ = arrayValues value

arrayValues :: AesonParser p => P p Value -> P p Array
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
      v <- head =. val <* skipSpace
      tail =. do
        ch <- nextWord' =. A.satisfy (\w -> w == C ',' || w == C ']')
        let acc' = v : acc
        case ch of
          C ',' -> skipSpace *> loop acc' (len + 1)
          _ {- ']' -} -> pure (V.reverse (V.fromListN len acc'))

    nextWordCloseSquare [] = True
    nextWordCloseSquare (_ : _) = False

    nextWord' [] = C ']'
    nextWord' _ = C ','

value :: AesonParser p => P p Value
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

    aString = (<$>) String . (=.) (\(String s) -> s)
    aObject = (<$>) Object . (=.) (\(Object o) -> o)
    aArray  = (<$>) Array  . (=.) (\(Array  a) -> a)
    aNumber = (<$>) Number . (=.) (\(Number n) -> n)

-- | Parse a quoted JSON string.
jstring :: AesonParser p => P p Text
jstring = A.word8 (C '"') *> jstring_

jstring0 :: AesonParser p => P p Text
jstring0 = A.parseOrPrint Aeson.jstring $ \t ->
  AP.seeLazyBS (encode t)

data EscapeState = Escape | NoEscape

-- | Parse a quoted JSON string without the leading quote.
jstring_ :: AesonParser p => P p Text
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

unescapeText :: ByteString -> Either String Text
unescapeText = undefined

escapeText :: Text -> ByteString
escapeText = undefined

skipSpace :: AesonParser p => p x ()
skipSpace = const "" =. A.skipWhile isSpace
  where
    isSpace w = w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

data SP = SP !Integer {-# UNPACK #-}!Int

scientific :: AesonParser p => P p Scientific
scientific = A.parseOrPrint undefined undefined

