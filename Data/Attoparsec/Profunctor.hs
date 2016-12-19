{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Attoparsec.Profunctor where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Profunctor
import Data.Semigroup
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString as P
import Prelude hiding (take, takeWhile)

newtype Parser x a = Parser { runParser :: P.Parser a }
  deriving (
    Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail
  )

type Parser' a = Parser a a

class Profunctor p => Attoparsec p where
  -- Parsing individual bytes

  word8 :: Word8 -> p x Word8
  anyWord8 :: p Word8 Word8
  notWord8 :: Word8 -> p Word8 Word8
  notWord8 = satisfy . (/=)
  satisfy :: (Word8 -> Bool) -> p Word8 Word8
  -- satisfyWith :: (Word8 -> a) -> (a -> Bool) -> p Word8 a
  skip :: (Word8 -> Bool) -> p Word8 ()
  skip = rmap (const ()) . satisfy

  -- Lookahead

  peekWord8 :: p (Maybe Word8) (Maybe Word8)

  -- String handling

  string :: ByteString -> p x ByteString
  skipWhile :: (Word8 -> Bool) -> p ByteString ()
  take :: Int -> p ByteString ByteString
  scan :: s -> (s -> Word8 -> Maybe s) -> p ByteString ByteString
  scan = (fmap . fmap . rmap) fst runScanner
  runScanner :: s -> (s -> Word8 -> Maybe s) -> p ByteString (ByteString, s)
  takeWhile :: (Word8 -> Bool) -> p ByteString ByteString
  takeWhile1 :: (Word8 -> Bool) -> p ByteString ByteString
  takeTill :: (Word8 -> Bool) -> p ByteString ByteString
  takeTill f = takeWhile (not . f)

  takeByteString :: p ByteString ByteString

  (<?>) :: p b a -> String -> p b a
  (<?>) p _ = p

  atEnd :: p Bool Bool

instance Profunctor Parser where
  lmap _ (Parser p) = Parser p
  rmap f (Parser p) = Parser (fmap f p)

instance Attoparsec Parser where
  word8 = Parser . P.word8
  anyWord8 = Parser P.anyWord8
  notWord8 = Parser . P.notWord8
  satisfy = Parser . P.satisfy
  -- satisfyWith f = Parser . P.satisfyWith f
  skip = Parser . P.skip
  peekWord8 = Parser P.peekWord8
  string = Parser . P.string
  skipWhile = Parser . P.skipWhile
  take = Parser . P.take
  scan s = Parser . P.scan s
  runScanner s = Parser . P.runScanner s
  takeWhile = Parser . P.takeWhile
  takeWhile1 = Parser . P.takeWhile1
  takeTill = Parser . P.takeTill
  takeByteString = Parser P.takeByteString
  (<?>) (Parser p) = Parser . (P.<?>) p
  atEnd = Parser P.atEnd
