{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.Attoparsec.Unparse
import AesonParser

main :: IO ()
main = do
  testExamples
  testProp

examples :: [BS.ByteString]
examples =
  [ "\"Wow\""
  , "{\"mu:ch\\\" doge\": [null, false, true, \"beautiful\"]}"
  ]

testExamples :: IO ()
testExamples = for_ examples $ \s -> do
  v <- unwrap $ parse value s
  s'_ <- unwrap $ unparse value v
  let s' = LBS.toStrict s'_
  v' <- unwrap $ parse value s'
  assertEqual v v' $ do
    BS8.putStrLn s
    BS8.putStrLn s'

unwrap :: Either String b -> IO b
unwrap (Right b) = pure b
unwrap (Left a) = fail a

assertEqual :: (Show a, Eq a) => a -> a -> IO () -> IO ()
assertEqual a a' ifFail =
  if a == a' then
    pure ()
  else do
    putStrLn "Not equal:"
    print a
    print a'
    ifFail
    fail "Failed"

testProp :: IO ()
testProp = quickCheck prop_printParse

prop_printParse :: Value -> Property
prop_printParse v = ioProperty $ do
  s_ <- unwrap $ unparse value v
  let s = LBS.toStrict s_
  v' <- unwrap $ parse value s
  pure (v === v')

instance Arbitrary Value where
  arbitrary = sized $ \n -> oneof $
    [ pure Null
    , Bool <$> arbitrary
    , String <$> arbitrary
    -- TODO: , Number <$> arbitrary
    ] ++
    if n == 0 then [] else fmap (resize (n `div` 2))
    [ Array  <$> arbitrary
    , Object <$> arbitrary
    ]
