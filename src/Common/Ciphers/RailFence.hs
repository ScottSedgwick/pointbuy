{-# LANGUAGE TypeSynonymInstances #-}
module Common.Ciphers.RailFence
  ( RailFenceKey
  , decode
  , encode
  , mkKey
  ) where

import Common.Ciphers.Utils
import qualified Data.List as L
import qualified Data.Ord  as O

data RailFenceKey = RailFenceKey
  { x :: Int
  }

mkKey :: Int -> RailFenceKey
mkKey x = RailFenceKey { x = x }

instance Codec RailFenceKey where
    encode (RailFenceKey r) p = 
        if (r < 2)
        then error $ "RailFence encode error: R < 2"
        else map fst $ L.sortBy (O.comparing snd) $ zip (tidy p) (rows r)
    decode (RailFenceKey r) xs = 
        if (r < 2)
        then error $ "RailFence decode error: R < 2"
        else replaceAll '_' ' ' $ map fst $ L.sortBy (O.comparing snd) $ zip xs (derailmap (length xs) r)

-- tidy up a string for processing
tidy :: [Char] -> [Char]
tidy = replaceAll ' ' '_'

-- make the rail fence pattern in numbers
rows :: (Num a, Enum a) => a -> [a]
rows r = cycle ([0..r-2] ++ [r-1,r-2..1])

-- tidying the first message empties a list of numbers
-- this is the list of positions that characters came from
derailmap :: (Ord a, Num a, Enum a, Num b, Enum b) => b -> a -> [b]
derailmap n r = map fst $ L.sortBy (O.comparing snd) $ zip [0..n-1] $ rows r

