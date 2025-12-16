module Common.Utils where

import qualified Data.Char as C

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

toLower :: String -> String
toLower = map C.toLower