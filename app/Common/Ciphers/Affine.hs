module Common.Ciphers.Affine
  ( AffineKey(..)
  , decode
  , encode
  , inverse
  , mkKey
  , mkKey'
  ) where

import Common.Ciphers.Utils
import Data.List ( elemIndex )

data AffineKey = AffineKey
  { a :: Int
  , b :: Int
  , ls :: String
  } deriving stock (Show)

instance Codec AffineKey where
    encode k = map (encodePart k)
    decode k xs = 
        case inverse (a k) (length (ls k)) of
            Nothing -> error $ "No inverse: " <> show k 
            Just i  -> map (decodePart (k { a = i })) xs

mkKey :: Int -> Int -> AffineKey
mkKey a' b' = AffineKey { a = a', b = b', ls = standardChars }

mkKey' :: Int -> Int -> String -> AffineKey
mkKey' a' b' ls' = AffineKey { a = a', b = b', ls = ls' }

encodePart :: AffineKey -> Char -> Char
encodePart k c = 
    case mx of
      Nothing -> c
      Just x  -> (ls k) !! e
        where
          e = (((a k) * x) + (b k)) `mod` m
  where
    mx = c `elemIndex` (ls k)
    m  = length (ls k)

decodePart :: AffineKey -> Char -> Char
decodePart k c = 
    case mx of
      Nothing -> c
      Just x  -> (ls k) !! d
        where
          d = ((a k) * (x - (b k))) `mod` m
  where
    mx = c `elemIndex` (ls k)
    m  = length (ls k)