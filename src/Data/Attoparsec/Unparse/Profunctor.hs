{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Attoparsec.Unparse.Profunctor where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as P
import Data.Maybe (isJust)
import Data.Profunctor
import Prelude hiding (take, takeWhile)

import Data.Attoparsec.Unparse.Printer

class Profunctor p => Attoparsec p where

  -- Parsing individual bytes

  word8 :: Word8 -> p x Word8
  anyWord8 :: p Word8 Word8
  notWord8 :: Word8 -> p Word8 Word8
  notWord8 = satisfy . (/=)
  satisfy :: (Word8 -> Bool) -> p Word8 Word8
  -- satisfyWith :: (Word8 -> a) -> (a -> Bool) -> p Word8 a
  skip :: (Word8 -> Bool) -> p Word8 ()

  -- Lookahead

  peekWord8 :: p (Maybe Word8) (Maybe Word8)
  peekWord8' :: p Word8 Word8

  -- | Get the value of a predicate on the lookahead.
  --
  -- This extra method allows the unparser not to worry about knowing the
  -- exact value of a lookahead.
  peekWord8Class' :: (Word8 -> Bool) -> p Bool Bool

  -- | Get a representative of a character class a lookahead belongs to.
  --
  -- Care must be taken not to learn more about the resulting character
  -- than the class it belongs to.
  unsafePeekWord8Class' :: p (Class Word8) Word8

  -- String handling

  string :: ByteString -> p x ByteString
  skipWhile :: (Word8 -> Bool) -> p ByteString ()
  take :: Int -> p ByteString ByteString
  scan :: s -> (s -> Word8 -> Maybe s) -> p ByteString ByteString
  runScanner :: s -> (s -> Word8 -> Maybe s) -> p ByteString (ByteString, s)
  takeWhile :: (Word8 -> Bool) -> p ByteString ByteString
  takeWhile1 :: (Word8 -> Bool) -> p ByteString ByteString
  takeTill :: (Word8 -> Bool) -> p ByteString ByteString
  takeTill f = takeWhile (not . f)

  takeByteString :: p ByteString ByteString

  (<?>) :: p b a -> String -> p b a
  (<?>) p _ = p

  atEnd :: p Bool Bool

  -- Assertion

  -- | As a parser, do nothing (@return ()@); as a printer, fail if the
  -- predicate is not satisfied, with the given error message.
  assert :: String -> (x -> Bool) -> p x ()

  -- Separate definitions

  parseOrPrint :: P.Parser a -> (a -> Printer' ()) -> p a a

defaultSkip
  :: (Attoparsec p, Functor (p Word8))
  => (Word8 -> Bool) -> p Word8 ()
defaultSkip = fmap (const ()) . satisfy

defaultScan
  :: (Attoparsec p, Functor (p ByteString))
  => s -> (s -> Word8 -> Maybe s) -> p ByteString ByteString
defaultScan = (fmap . fmap . fmap) fst runScanner

data Class a = Class (a -> Bool) a

singleton :: Eq a => a -> Class a
singleton a = Class (a ==) a

cosingleton :: Word8 -> Class Word8
cosingleton w = Class (w /=) (w + 1)

digitsClass :: Class Word8
digitsClass = Class (\w -> w >= 48 && w <= 57) 48

-- * Parser

newtype Parser x a = Parser { runParser :: P.Parser a }
  deriving (
    Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail
  )

type Parser' a = Parser a a

parse :: Parser x a -> ByteString -> Either String a
parse (Parser p) = P.parseOnly p

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
  peekWord8' = Parser P.peekWord8'
  peekWord8Class' f = Parser (fmap f P.peekWord8')
  unsafePeekWord8Class' = Parser P.peekWord8'
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
  assert _ _ = return ()
  parseOrPrint p _ = Parser p

instance Attoparsec Printer where
  word8 w = star $ \_ -> do
    seeWord8 w
    pure w

  anyWord8 = star' $ \w -> do
    seeWord8 w

  satisfy p = star' $ \w ->
    if p w then do
      seeWord8 w
    else
      empty

  peekWord8 = star' $ \w_ -> do
    say (tell (w_ ==))

  peekWord8' = star' $ \w -> do
    say (tellWord8 w)

  peekWord8Class' f = star' $ \b ->
    say (tellSatisfy ((b ==) . f))

  unsafePeekWord8Class' = star $ \(Class f w) -> do
    say (tellSatisfy f)
    pure w

  string b = star $ \_ -> do
    see b
    pure b

  skipWhile p = star $ \b ->
    if BS.all p b then do
      see b
      say $ tellUnsatisfy p
    else
      throwError $ "unparse skipWhile: " ++ show b

  skip = defaultSkip

  take n = star' $ \b ->
    if BS.length b /= n then
      throwError $
        "unparse take: expected length " ++ show n ++
        ", got " ++ show (BS.length b, b)
    else do
      see b

  runScanner s f = star $ \b ->
    let
      g w k s = case f s w of
        Nothing ->
          throwError $ "unparse runScanner: scan terminated early on " ++ show b
        Just s' -> k s'
      k s = do
        see b
        say . tellUnsatisfy $ \w -> isJust (f s w)
        pure (b, s)
    in
      BS.foldr g k b s

  scan = defaultScan

  takeWhile p = star' $ \b ->
    if BS.all p b then do
      see b
      say $ tellUnsatisfy p
    else
      throwError $ "unparse takeWhile: " ++ show b

  takeWhile1 p = star' $ \b ->
    if BS.all p b && not (BS.null b) then do
      see b
      say $ tellUnsatisfy p
    else
      throwError $ "unparse takeWhile1: " ++ show b

  takeByteString = star' $ \b -> do
    see b
    seeEof

  atEnd = star' $ \eof -> do
    if eof then
      seeEof
    else
      say (tell isJust)

  assert e p = star $ \x -> when (not (p x)) (throwError e)

  parseOrPrint _ q = star' q
