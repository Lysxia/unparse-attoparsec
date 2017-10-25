{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O3 -fplugin GHC.Proof.Plugin #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module AesonProof where

import Data.Aeson (Value(..))
import Data.Profunctor (dimap)
import GHC.Proof

import qualified Data.Attoparsec.Unparse as A
import Data.Attoparsec.Unparse.Projection

import AesonParser

-- Alignment property
-- value_aligned = value === id_
--
-- i.e.
--
-- value_aligned_eta = \x -> apply value x === Just x

-- That doesn't go through because the implementation is incomplete
-- and value is recursive so GHC doesn't inline it.

-- Restricting the domain to known working subsets
-- addresses partial implementations.
--
-- Unfolding of definitions enables inlining.
-- Unfolding could be automated.

-- Simple unfolding example

lid (x : xs) = x : lid xs
lid [] = []

-- Obviously equivalent to lid (just renamed the LHS)
lid_unfold (x : xs) = x : lid xs
lid_unfold [] = []

lid_id = lid_unfold [] === id []

{-# INLINE value_unfold #-}

-- An inlineable copy of value.
value_unfold :: AesonParser p => J p Value
value_unfold = do
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

-- A proof constructor. Parameterized by (===) so GHC.Proof doesn't
-- try to "prove" this general fact.
{-# INLINE prove_value_pAligned #-}
prove_value_pAligned (===) x = apply value_unfold x === Just x

value_pAligned_True = prove_value_pAligned (===) (Bool True)
value_pAligned_False = prove_value_pAligned (===) (Bool False)
