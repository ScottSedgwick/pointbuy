module Common.Unshow
  ( Unshow
  , unshow
  ) where

import Data.Maybe (Maybe)

class Unshow a where
  unshow :: String -> Maybe a