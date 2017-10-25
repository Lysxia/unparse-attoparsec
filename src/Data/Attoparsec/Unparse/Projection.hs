-- | Projection of a printer to a pure function.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Attoparsec.Unparse.Projection where

import Data.Attoparsec.Unparse.Profunctor
import Data.Profunctor

newtype Projection a b = Projection (Star Maybe a b)
  deriving (Functor, Applicative, Monad, Profunctor)

asProjection :: (a -> b) -> Projection a b
asProjection f = asProjection' (Just . f)

asProjection' :: (a -> Maybe b) -> Projection a b
asProjection' f = Projection (Star f)

id_ :: Projection a a
id_ = asProjection id

apply :: Projection a b -> a -> Maybe b
apply (Projection (Star p)) = p

instance Attoparsec Projection where
  word8 = return
  anyWord8 = id_

  satisfy p = asProjection' (\w -> if p w then Just w else Nothing)

  skip = const (return ())  -- ?

  peekWord8 = id_
  peekWord8' = id_

  peekWord8Class' = const id_
  unsafePeekWord8Class' = asProjection (\(Class _ b) -> b)

  string = return

  skipWhile = const (return ())
  take = const id_
  scan _ _ = id_  -- TODO
  runScanner = error "TODO"
  takeWhile = const id_
  takeWhile1 = const id_

  takeByteString = id_

  endOfInput = return ()
  atEnd = id_

  assert _ p = asProjection' (\w -> if p w then Just () else Nothing)

  parseOrPrint _ _ = id_
