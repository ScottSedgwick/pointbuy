module Common.Utils where

import qualified Data.Char as C

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

showLevel :: Int -> String
showLevel (-1) = "All"
showLevel 0 = "Cantrip"
showLevel 1 = "1st level"
showLevel 2 = "2nd level"
showLevel 3 = "3rd level"
showLevel x = show x <> "th level"

toLower :: String -> String
toLower = map C.toLower