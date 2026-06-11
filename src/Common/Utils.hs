module Common.Utils where

import           Control.Monad   ( mzero )
import qualified Data.Char       as C
import qualified Data.Map.Strict as M
import           Miso.JSON       ( Parser, Value(..) )
import           Miso.String     ( MisoString, fromMisoString, ms )

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

msToLower :: MisoString -> MisoString
msToLower = ms . toLower . fromMisoString

parseSumValue :: MisoString -> [(MisoString, a)] -> Parser a
parseSumValue _ [] = mzero
parseSumValue v ((x, y): xs) = if (v == x) then pure y else parseSumValue v xs

parseSumObject :: (M.Map MisoString Value) -> [(MisoString, Value -> Parser a)] -> Parser a
parseSumObject _ [] = mzero
parseSumObject m ((x, fx): xs) =
  case (x `M.lookup` m) of
    Just v -> fx v
    _ -> parseSumObject m xs

parseString :: (MisoString -> a) -> Value -> Parser a
parseString fx (String s) = pure $ fx s
parseString _ _ = mzero
