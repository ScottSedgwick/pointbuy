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
  | BR
  | RollTable [String]
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
                [ ("br", (const BR)) 
                , ("p", (\v -> Plain  (unpackText v)))
                , ("b", (\v -> Bold   (unpackText v)))
                , ("i", (\v -> Italic (unpackText v)))
                , ("rt", (\v -> RollTable (unpackTextArray v)))
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

firstJust :: [Maybe a] -> Maybe a
firstJust (Just x:_) = Just x
firstJust (_:xs)     = firstJust xs
firstJust []         = Nothing

renderInline :: Inline -> View m a
renderInline (Plain s)      = text (ms s)
renderInline (Bold s)       = H.b_ [] [ text (ms s) ]
renderInline (Italic s)     = H.i_ [] [ text (ms s) ]
renderInline BR             = H.br_ []
renderInline (RollTable xs) = stripeTable "Description" xs

stripeTable :: String -> [String] -> View m a
stripeTable title xs =
  H.table_ [ P.class_ "stripes" ]
  [ H.thead_ []
    [ H.tr_ [] 
      [ H.th_ [] [ text (ms $ "d" <> show (length xs)) ]
      , H.th_ [] [ text (ms title ) ]
      ]
    ]
  , H.tbody_ [] (map mkStripeRow (zip [1..] xs))
  ]

mkStripeRow :: (Int, String) -> View m a
mkStripeRow (x,s) = 
  H.tr_ []
  [ H.td_ [] [ text (ms $ show x)]
  , H.td_ [] [ text (ms s)]
  ]