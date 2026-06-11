module Common.Ciphers.Utils
  ( Codec(..)
  , extGCD
  , hasInverse
  , inverse
  , pad
  , replaceAll
  , shift
  , shiftChar
  , standardChars
  ) where

import Data.Char (chr)
import qualified Data.List as L

-- Returns (c,x,y): the GCD of a and b (c), along with x and y such that
--   GCD(a,b) = ax + by
-- calculated via the recursive extended Euclidean algorithm presented in
-- class.
extGCD :: Int -> Int -> Maybe (Int,Int,Int)
extGCD 0 0 = Nothing
extGCD a 0 = Just (1,0,a)
extGCD a b = case extGCD b r of
                Nothing        -> Nothing
                (Just (c,x,y)) -> Just (x,c-q*x, y)
  where 
    (q,r) = a `divMod` b

inverse :: Int -> Int -> Maybe Int
inverse a m = 
  case extGCD a m of
    Just (x,_,1) -> Just (x `mod` m)
    _            -> Nothing

hasInverse :: Int -> Int -> Bool
hasInverse a m = case extGCD a m of
                   Just (_,_,1) -> True
                   _ -> False

-- Right pad the string `xs` with 'X' characters, until it is a integer multiple of `n` long.
pad :: Int -> Char -> String -> String
pad n c xs = xs <> p
  where
    l = n - (length xs) `mod` n
    p = replicate l c

-- replaceAll old new xs
-- In the string `xs`, replace all instances of the character `old` with the character `new`.
replaceAll :: (Eq a) => a -> a -> [a] -> [a]
replaceAll old new xs = foldr (\a b -> (replace a) : b) [] xs
  where
    replace x = if x == old then new else x

-- Find a unicode character starting at offset `n` that can be used to represent charater `c` in the standard charset.
shiftChar :: Int -> Char -> Char
shiftChar n c = 
    case L.elemIndex c standardChars of
      Nothing -> c
      Just x  -> chr (x + n)

-- Run `shiftChar` on all the characters in a string.
shift :: Int -> String -> String
shift n = map (shiftChar n)
         
standardChars :: String
standardChars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['*', '$', '!', '#', '+']

class Codec a where
    encode :: a -> String -> String
    decode :: a -> String -> String