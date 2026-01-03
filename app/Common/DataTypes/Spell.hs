module Common.DataTypes.Spell where

import           Common.DataTypes.Inline ( Inline )
import           Control.Lens.TH         ( makeLenses )
import           Data.Aeson              ( FromJSON, Value(..), (.:), (.:?), parseJSON, withObject )
import           GHC.Generics            ( Generic )

data Spell = Spell
  { _title :: String
  , _source :: [String]
  , _level :: Int
  , _school :: String
  , _castingTime :: String
  , _range :: String
  , _components :: String
  , _duration :: String
  , _description :: [Inline]
  , _lists :: [String]
  } deriving (Show, Eq, Generic)
makeLenses ''Spell

instance FromJSON Spell where
  parseJSON = withObject "Spell" $ \v -> Spell
    <$> v .: "title"
    <*> v .: "source"
    <*> v .: "level"
    <*> v .: "school"
    <*> v .: "castingTime"
    <*> v .: "range"
    <*> v .: "components"
    <*> v .: "duration"
    <*> v .: "description"
    <*> v .: "lists"