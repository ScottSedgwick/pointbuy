{-# LANGUAGE TypeSynonymInstances #-}
module Common.Ciphers.Caesar 
  ( CaesarKey
  , decode
  , encode
  , mkKey
  ) where

import qualified Common.Ciphers.Affine as CA
import Common.Ciphers.Utils

data CaesarKey = CaesarKey
  { x :: Int
  }

mkKey :: Int -> CaesarKey
mkKey x = CaesarKey { x = x }

instance Codec CaesarKey where
    encode key message = CA.encode (CA.mkKey 1 (x key)) message
    decode key message = CA.decode (CA.mkKey 1 (x key)) message