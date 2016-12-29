{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)

import Data.Attoparsec.Unparse
import AesonParser

examples :: [BS.ByteString]
examples =
  [ "\"Wow\""
  , "{\"mu:ch\\\" doge\": [null, false, true, \"beautiful\"]}"
  ]

main :: IO ()
main = for_ examples $ \s -> do
  BS8.putStrLn s
  v <- unwrap $ parse value s
  s'_ <- unwrap $ unparse value v
  let s' = LBS.toStrict s'_
  v' <- unwrap $ parse value s'
  assertEqual v v'
  BS8.putStrLn s'

unwrap :: Either String b -> IO b
unwrap (Right b) = pure b
unwrap (Left a) = fail a

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a a' =
  if a == a' then
    pure ()
  else
    fail $ "Not equal: " ++ show (a, a')
