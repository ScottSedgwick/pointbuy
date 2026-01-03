module Common.DataTypes.Inline where

import           Common.Utils        ( maybeHead )
import           Control.Applicative ((<|>))
import           Data.Aeson         ( FromJSON, Value(..), (.:), (.:?), parseJSON, withObject )
import           Data.Aeson.Types   ( Object, Parser )
import           Data.Aeson.KeyMap  ( Key(..), (!?), keys )
import qualified Data.Maybe         as M
import qualified Data.Text          as T
import qualified Data.Vector        as V
import           GHC.Generics       ( Generic )
import           Miso               ( Attribute, Component, Effect, MisoString, Transition, View, component, initialAction, ms, text )
import           Miso.Fetch         ( Response(body, errorMessage), getJSON )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P

data Inline 
  = Plain String
  | Bold String
  | Italic String
  | UList [String]
  | RollTable [String]
  | SpellTable [String]
  | SpellTable1 [String]
  | Table [[String]]
  deriving (Show, Eq, Generic)

data TableJson = TableJson
  { headings :: [String]
  , rows :: [[String]]
  } deriving (Show, Eq, Generic)

instance FromJSON TableJson where
  parseJSON = withObject "TableJson" $ \v -> TableJson
    <$> v .: "headings"
    <*> v .: "rows"

instance FromJSON Inline where
  parseJSON = withObject "Inline" $ \v -> do
    let res = firstJust $ map (parseObject v) 
                [ ("p", (\v -> Plain  (unpackText v)))
                , ("b", (\v -> Bold   (unpackText v)))
                , ("i", (\v -> Italic (unpackText v)))
                , ("ul", (\v -> UList (unpackTextArray v)))
                , ("rt", (\v -> RollTable (unpackTextArray v)))
                , ("st", (\v -> SpellTable (unpackTextArray v)))
                , ("st1", (\v -> SpellTable1 (unpackTextArray v)))
                , ("t", (\v -> Table (unpackTextArrays v)))
                ]
    case res of
      Just r  -> pure r
      Nothing -> error "Invalid data type for Inline"

parseObject :: Object -> (Key, (Value -> Inline)) -> Maybe Inline
parseObject o (k, f) =
  case o !? k of
    (Just v) -> Just $ f v
    Nothing  -> Nothing

unpackText :: Value -> String
unpackText (String s) = T.unpack s
unpackText v = error $ "Expected string type but got " <> show v

unpackTextArray :: Value -> [String]
unpackTextArray (Array a) = map unpackText (V.toList a)
unpackTextArray v = error $ "Expected array of string but got " <> show v

unpackTextArrays :: Value -> [[String]]
unpackTextArrays (Array a) = map unpackTextArray (V.toList a)
unpackTextArrays v = error $ "Expected array of array of string but got " <> show v

firstJust :: [Maybe a] -> Maybe a
firstJust (Just x:_) = Just x
firstJust (_:xs)     = firstJust xs
firstJust []         = Nothing

renderInline :: Inline -> View m a
renderInline (Plain s)        = H.p_ [] [ text (ms s) ]
renderInline (Bold s)         = H.p_ [] [ H.b_ [] [ text (ms s) ] ]
renderInline (Italic s)       = H.p_ [] [ H.em_ [] [ text (ms s) ] ]
renderInline (UList xs)       = H.ul_ [] (map (\s -> H.li_ [] [text (ms s)]) xs)
renderInline (RollTable xs)   = rollTable "Description" xs
renderInline (SpellTable xs)  = spellTable "Spells" xs
renderInline (SpellTable1 xs) = spellTable1 "Spells" xs
renderInline (Table xs)       = table xs

tupleList :: a -> a -> [a]
tupleList a b = [a,b]

spellLevels :: [String]
spellLevels = ["Cantrip", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th"]

rollTable :: String -> [String] -> View m a
rollTable title xs = table ([("d" <> show (length xs)), title] : ys)
  where
    ys = zipWith tupleList (map show [1..]) xs

spellTable :: String -> [String] -> View m a
spellTable title xs = table (["Spell Level", title] : (zipWith tupleList spellLevels xs))

spellTable1 :: String -> [String] -> View m a
spellTable1 title xs = table (["Spell Level", title] : (zipWith tupleList (drop 1 spellLevels) xs))

stripeTable :: [String] -> [[String]] -> View m a
stripeTable ts xs =
  H.table_ [ P.class_ "stripes" ]
  [ H.thead_ []
    [ H.tr_ [] (map (\t -> H.th_ [] [ text (ms t)]) ts)
    ]
  , H.tbody_ [] (map mkStripeRow xs)
  ]

mkStripeRow :: [String] -> View m a
mkStripeRow xs = 
  H.tr_ [] (map (\x -> H.td_ [] [ text (ms x)]) xs)

table :: [[String]] -> View m a
table [] = stripeTable [] []
table (x:xs) = stripeTable x xs