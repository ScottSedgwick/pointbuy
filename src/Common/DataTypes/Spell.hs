module Common.DataTypes.Spell where

import Common.DataTypes.Inline ( Structure )
import Control.Lens.TH         ( makeLenses )
import GHC.Generics            ( Generic )
import Miso.JSON               ( FromJSON, (.:), (.:?), (.!=), parseJSON, withObject )
import Miso.String             ( MisoString )

data Spell = Spell
  { _title :: MisoString
  , _source :: [MisoString]
  , _level :: Int
  , _school :: MisoString
  , _castingTime :: MisoString
  , _range :: MisoString
  , _components :: MisoString
  , _duration :: MisoString
  , _description :: [Structure]
  , _lists :: [MisoString]
  } deriving (Show, Eq, Generic)
makeLenses ''Spell

instance FromJSON Spell where
  parseJSON = withObject "Spell" $ \v -> Spell
    <$> v .: "title"
    <*> v .:? "source" .!= []
    <*> v .: "level"
    <*> v .: "school"
    <*> v .: "castingTime"
    <*> v .: "range"
    <*> v .: "components"
    <*> v .: "duration"
    <*> v .: "description"
    <*> v .: "lists"
    