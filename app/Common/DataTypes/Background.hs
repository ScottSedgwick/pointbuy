module Common.DataTypes.Background where

import           Common.DataTypes.Inline
import           Control.Lens.TH    ( makeLenses )
import           Data.Aeson         ( FromJSON, Value(..), (.:), (.:?), parseJSON, withObject )
import           Data.Aeson.KeyMap  ( Key(..), (!?), keys )
import qualified Data.Text          as T
import           GHC.Generics       ( Generic )

data BackgroundProficiency = BackgroundProficiency
  { _skill :: [String]
  , _tool :: [String]
  , _languages :: [String]
  } deriving (Show, Eq, Generic)
makeLenses ''BackgroundProficiency

instance FromJSON BackgroundProficiency where
  parseJSON = withObject "BackgroundProficiency" $ \v -> BackgroundProficiency
    <$> v .: "skills"
    <*> v .: "tools"
    <*> v .: "languages"

data BackgroundFeature = BackgroundFeature
  { _featureTitle :: String
  , _featureDescription :: [Inline]    
  } deriving (Show, Eq, Generic)
makeLenses ''BackgroundFeature

instance FromJSON BackgroundFeature where
  parseJSON = withObject "BackgroundFeature" $ \v -> BackgroundFeature
    <$> v .: "title"
    <*> v .: "description"

data BackgroundTraits = BackgroundTraits
  { _personality :: [String]
  , _ideals :: [String]
  , _bonds :: [String]
  , _flaws :: [String]
  } deriving (Show, Eq, Generic)
makeLenses ''BackgroundTraits

instance FromJSON BackgroundTraits where
  parseJSON = withObject "BackgroundTraits" $ \v -> BackgroundTraits
    <$> v .: "personality"
    <*> v .: "ideals"
    <*> v .: "bonds"
    <*> v .: "flaws"

data Background = Background
  { _title :: String
  , _description :: [String]
  , _source :: String
  , _sourceurl :: String
  , _proficiencies :: Maybe BackgroundProficiency
  , _equipment :: Maybe [String]
  , _features :: Maybe [BackgroundFeature]
  , _suggested :: Maybe [String]
  , _traits :: Maybe BackgroundTraits
  } deriving (Show, Eq, Generic)
makeLenses ''Background

instance FromJSON Background where
  parseJSON = withObject "Background" $ \v -> Background
    <$> v .: "title"
    <*> v .: "description"
    <*> v .: "source"
    <*> v .: "sourceurl"
    <*> v .:? "proficiencies"
    <*> v .:? "equipment"
    <*> v .:? "features"
    <*> v .:? "suggested"
    <*> v .:? "traits"