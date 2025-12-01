module Common.Ciphers.Scytale 
  ( ScytaleKey
  , encode
  , decode
  , mkKey
  ) where

import qualified Data.Matrix       as M
import Data.Char (isSpace)
import Common.Ciphers.Utils

data ScytaleKey = ScytaleKey
  { x :: Int
  }

mkKey :: Int -> ScytaleKey
mkKey x = ScytaleKey { x = x }

instance Codec ScytaleKey where
    encode (ScytaleKey n) xs = M.toList $ M.transpose zs
        where
            ys = replaceAll ' ' '_' $ pad n '_' xs
            rows = (length ys) `div` n
            zs = M.fromList rows n ys
    decode (ScytaleKey n) xs = strip $ replaceAll '_' ' ' $ M.toList $ M.transpose ys
        where
            cols = (length xs) `div` n
            ys = M.fromList n cols xs

strip :: String -> String
strip xs = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace xs