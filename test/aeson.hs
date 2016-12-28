{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PatternSynonyms #-}

import qualified Control.Monad as M
import Control.Monad.Fail

import Data.Aeson (Value (..))
import Data.Function ((&))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8)
import Prelude hiding (fail, (<$>), (<*>), (>>=), (<*), (*>), pure, return)

import Profunctor.Monad
import Profunctor.Monad.Combinators

import Data.Attoparsec.Profunctor (Attoparsec, Parser')
import qualified Data.Attoparsec.Profunctor as A

pattern DoubleQuote :: Word8
pattern DoubleQuote = 34

pattern BackSlash :: Word8
pattern BackSlash = 92

type P p a = p a a
type AesonParser p =
  ( Attoparsec p
  , Monad1 p
  , ForallF MonadFail p
  )

value :: AesonParser p => P p Value
value = undefined

-- | Parse a quoted JSON string.
jstring :: forall p. AesonParser p => P p Text
jstring = A.word8 DoubleQuote *> jstring_

data EscapeState = Escape | NoEscape

-- | Parse a quoted JSON string without the leading quote.
jstring_ :: forall p. AesonParser p => P p Text
jstring_ = do
  s <- escapeText =. A.scan startState go <* A.word8 DoubleQuote
  case unescapeText s of
    Right r -> pure r
    Left err -> with @MonadFail @p @Text $
      fail err
  where
    startState = NoEscape  -- seen backslash
    go Escape c = Just NoEscape
    go a DoubleQuote = Nothing
    go a BackSlash = Just Escape
    go a c = Just NoEscape

unescapeText :: ByteString -> Either String Text
unescapeText = undefined

escapeText :: Text -> ByteString
escapeText = undefined

main :: IO ()
main = M.return ()
