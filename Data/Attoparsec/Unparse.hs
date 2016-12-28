{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Attoparsec.Unparse where

import Control.Applicative
import Control.Arrow (Kleisli(..))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import Data.Monoid
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import Profunctor.Monad

import Prelude hiding (take, takeWhile)

import Data.Attoparsec.Profunctor

type ByteString = BS.ByteString
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

newtype Printer x a = Printer (ReaderT x Printer' a)
  deriving (
    Functor, Applicative, Monad, Alternative, MonadPlus
  )

instance Contravariant Printer where
  type First Printer = Kleisli (Either String)
  lmap (Kleisli f) (Printer p) = Printer . ReaderT $ \y ->
    case f y of
      Right x -> runReaderT p x
      Left e -> throwError e

runPrinter :: Printer x a -> x -> Either String (LazyByteString, a)
runPrinter (Printer p) x =
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

instance Attoparsec Printer where
  word8 = pure

  anyWord8 = star' $ \w -> do
    seeWord8 w

  satisfy p = star' $ \w ->
    if p w then do
      seeWord8 w
    else
      empty

  peekWord8 = star' $ \w_ -> do
    say (tell (w_ ==))

  string b = star $ \_ -> do
    see b
    pure b

  skipWhile p = star $ \b ->
    if BS.all p b then do
      see b
      say $ tellUnsatisfy p
    else
      throwError $ "unparse skipWhile: " ++ show b

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
