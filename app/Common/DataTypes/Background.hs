module Common.DataTypes.Background where

import           Control.Lens.TH    ( makeLenses )
import           Data.Aeson         ( FromJSON, Value(..), (.:), parseJSON, withObject )
import           Data.Aeson.KeyMap  ( Key(..), (!?), keys )
import qualified Data.Text          as T
import           GHC.Generics       ( Generic )

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

data Inline 
  = Plain String
  | Bold String
  | Italic String
  | BR
  deriving (Show, Eq, Generic)
instance FromJSON Inline where
  parseJSON = withObject "Inline" $ \v -> do
    let ks = keys v
    case maybeHead ks of
      Nothing -> error "No data in Inline JSON structure"
      Just k  -> do
        let kn = show k
        if kn == "\"br\"" then pure BR
        else
          case v !? k of
            Nothing    -> error "No value in Inline JSON structure"
            Just (String value) -> do
              pure $ if     kn == "\"p\"" then Plain (T.unpack value)
                    else if kn == "\"b\"" then Bold (T.unpack value)
                    else if kn == "\"i\"" then Italic (T.unpack value)
                    else error ("Inline JSON Structure type invalid: " <> kn)
            Just _ -> error "Invalid data type for Inline"

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
  , _proficiencies :: BackgroundProficiency
  , _equipment :: [String]
  , _features :: [BackgroundFeature]
  , _suggested :: [String]
  , _traits :: BackgroundTraits
  } deriving (Show, Eq, Generic)
makeLenses ''Background

instance FromJSON Background where
  parseJSON = withObject "Background" $ \v -> Background
    <$> v .: "title"
    <*> v .: "description"
    <*> v .: "source"
    <*> v .: "sourceurl"
    <*> v .: "proficiencies"
    <*> v .: "equipment"
    <*> v .: "features"
    <*> v .: "suggested"
    <*> v .: "traits"