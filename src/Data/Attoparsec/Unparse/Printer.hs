{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Attoparsec.Unparse.Printer where

import Control.Applicative
import Control.Arrow (Kleisli(..))
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Data.Monoid
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import Profunctor.Monad

import Prelude hiding (take, takeWhile)

type LazyByteString = LBS.ByteString
type Builder = Builder.Builder

type Printer' = StateT (TellAhead, Builder) (Either String)
-- Functor, Applicative, Alternative, Monad, MonadPlus

-- | A streaming predicate on a ByteString, read character by character.
-- Returns @Nothing@ when it is no longer satisfied.
-- @TellTrue@ allows to simplify @TellAhead@ values to keep a reduced
-- complexity.
data TellAhead
  = Tell (Maybe Word8 -> Maybe TellAhead)
  | TellTrue

-- | Conjunction of predicates.
instance Monoid TellAhead where
  mempty = TellTrue
  mappend (TellTrue) p = p
  mappend p (TellTrue) = p
  mappend (Tell p) (Tell p') = Tell ((liftA2 . liftA2) (<>) p p')

newtype Printer x a = Printer { runPrinter :: ReaderT x Printer' a }
  deriving (
    Functor, Applicative, Monad, Alternative, MonadPlus
  )

instance MonadFail (Printer x) where
  fail = Printer . lift . lift . Left

instance Contravariant Printer where
  type First Printer = Kleisli (Either String)
  lmap (Kleisli f) (Printer p) = Printer . ReaderT $ \y ->
    case f y of
      Right x -> runReaderT p x
      Left e -> throwError e

unparse :: Printer x a -> x -> Either String LazyByteString
unparse q x = fmap fst (unparse' q x)

unparse' :: Printer x a -> x -> Either String (LazyByteString, a)
unparse' (Printer p) x =
  fmap (\(a, (_, builder)) -> (Builder.toLazyByteString builder, a)) $
    runStateT (runReaderT p x) mempty

star :: (x -> Printer' a) -> Printer x a
star = Printer . ReaderT

star' :: (a -> Printer' ()) -> Printer a a
star' f = star $ liftA2 (*>) f pure

tell :: (Maybe Word8 -> Bool) -> TellAhead
tell p = Tell $ \w_ ->
  if p w_ then
    Just TellTrue
  else
    Nothing

tellWord8 :: Word8 -> TellAhead
tellWord8 = tell . (==) . Just

tellSatisfy :: (Word8 -> Bool) -> TellAhead
tellSatisfy p = tell $ \w_ ->
  case w_ of
    Just w -> p w
    _ -> False

-- EOF or p w == False
tellUnsatisfy :: (Word8 -> Bool) -> TellAhead
tellUnsatisfy p = tell $ \w_ ->
  case w_ of
    Just w -> not (p w)
    Nothing -> True

tellEof :: TellAhead
tellEof = tell isNothing

say :: TellAhead -> Printer' ()
say tellAhead =
  modify $ \(tellAhead', builder) -> (tellAhead' <> tellAhead, builder)

see :: ByteString -> Printer' ()
see b = BS.foldr (\w m -> check w >> m) done b
  where
    done = modify $ \(tellAhead, builder) ->
      (tellAhead, builder <> Builder.byteString b)

seeLazyBS :: LazyByteString -> Printer' ()
seeLazyBS = see . LBS.toStrict

seeWord8 :: Word8 -> Printer' ()
seeWord8 w = do
  check w
  modify $ \(tellAhead, builder) -> (tellAhead, builder <> Builder.word8 w)

check :: Word8 -> Printer' ()
check w = do
  (tellAhead, builder) <- get
  case tellAhead of
    TellTrue -> pure ()
    Tell t -> case t (Just w) of
      Nothing -> throwError $ "seeWord8: unexpected " ++ show w
      Just tellAhead' -> put (tellAhead', builder)

seeEof :: Printer' ()
seeEof = do
  (tellAhead, builder) <- get
  case tellAhead of
    Tell t | Nothing <- t Nothing ->
      throwError "seeEof: unfinished printer"
    _ -> put (tellEof, builder)

