{-# LANGUAGE TypeSynonymInstances #-}
module Common.Ciphers.Atbash 
  ( decode
  , encode
  ) where

import qualified Common.Ciphers.Affine as CA

encode :: String -> String 
encode message = CA.encode (CA.mkKey (-1) (-1)) message

decode :: String -> String
decode message = CA.decode (CA.mkKey (-1) (-1)) message