module Common.DataTypes.Background where

import           Common.DataTypes.Inline
import           Control.Lens.TH    ( makeLenses )
import           Data.Aeson.KeyMap  ( Key(..), (!?), keys )
import qualified Data.Text          as T
import           GHC.Generics       ( Generic )
import           Miso               ( MisoString )
import           Miso.JSON          ( Parser(..), FromJSON, parseEither, parseJSON, (.:), (.:?), (.!=), withObject )

data BackgroundProficiency = BackgroundProficiency
  { _skill :: [MisoString]
  , _tool :: [MisoString]
  , _languages :: [MisoString]
  } deriving (Show, Eq, Generic)
makeLenses ''BackgroundProficiency

instance FromJSON BackgroundProficiency where
  parseJSON = withObject "BackgroundProficiency" $ \v -> BackgroundProficiency
    <$> v .: "skills"
    <*> v .: "tools"
    <*> v .: "languages"

data BackgroundFeature = BackgroundFeature
  { _featureTitle :: MisoString
  , _featureDescription :: [Structure]    
  } deriving (Show, Eq, Generic)
makeLenses ''BackgroundFeature

instance FromJSON BackgroundFeature where
  parseJSON = withObject "BackgroundFeature" $ \v -> BackgroundFeature
    <$> v .: "title"
    <*> v .: "description"

data BackgroundTraits = BackgroundTraits
  { _personality :: [MisoString]
  , _ideals :: [MisoString]
  , _bonds :: [MisoString]
  , _flaws :: [MisoString]
  } deriving (Show, Eq, Generic)
makeLenses ''BackgroundTraits

emptyTraits :: BackgroundTraits
emptyTraits = BackgroundTraits { _personality = [], _ideals = [], _bonds = [], _flaws = []}

instance FromJSON BackgroundTraits where
  parseJSON = withObject "BackgroundTraits" $ \v -> BackgroundTraits
    <$> v .:? "personality" .!= []
    <*> v .:? "ideals" .!= []
    <*> v .:? "bonds" .!= []
    <*> v .:? "flaws" .!= []

data Background = Background
  { _title :: MisoString
  , _description :: [MisoString]
  , _source :: MisoString
  , _sourceurl :: MisoString
  , _proficiencies :: Maybe BackgroundProficiency
  , _equipment :: [MisoString]
  , _features :: [BackgroundFeature]
  , _suggested :: [MisoString]
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
    <*> v .:? "equipment" .!= []
    <*> v .:? "features" .!= []
    <*> v .:? "suggested" .!= []
    <*> v .:? "traits"
    