{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveGeneric #-}
module Types where

import           Control.Lens    ( Lens', lens )
import           Control.Lens.TH ( makeLenses )
import           Data.Aeson      ( FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding )
import           Data.Default    ( Default, def )
import qualified Data.IntMap     as IM
import qualified Data.Map        as M
import           Data.Serialize  ( Serialize )
import           GHC.Generics    ( Generic )
import           Miso            ( MisoString, Response )

import           Types.Races     ( Race )
import           Types.Stats     ( Stat(..), StatBlock(..), str, con, dex, int, wis, cha )

data Tab 
  = Calculator
  | Custom
  | Raw
  | Races
  deriving (Eq, Enum, Bounded, Generic)
instance Serialize Tab where
instance Show Tab where
  show Calculator = "Calculator"
  show Custom     = "Custom Rules"
  show Raw        = "Rules as Written"
  show Races      = "Racial Information"
instance ToJSON Tab where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Tab
tabClass :: Tab -> String
tabClass Calculator = "Calculator"
tabClass Custom = "Custom"
tabClass Raw = "Raw"
tabClass Races = "Races"

statLens :: Stat -> Lens' StatBlock Int
statLens Strength = str
statLens Dexterity = dex
statLens Constitution = con
statLens Intelligence = int
statLens Wisdom = wis
statLens Charisma = cha

defPointBuyCosts :: IM.IntMap Int
defPointBuyCosts = IM.fromList [ (3,-9), (4,-6), (5,-4), (6,-2), (7,-1), (8,0), (9,1), (10,2), (11,3), (12,4), (13,5), (14,7), (15,9), (16,12), (17,15), (18,19) ]

allStats :: [Stat]
allStats = [minBound .. maxBound]

defaultStats :: StatBlock
defaultStats = StatBlock { _str = 8, _dex = 8, _con = 8, _int = 8, _wis = 8, _cha = 8 }

defaultBonuses :: StatBlock
defaultBonuses = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 0 }

data StateData 
  = StateData
  {_tab :: Tab
  , _availablePoints :: Int
  , _maxPurchasableAttribute :: Int
  , _minPurchasableAttribute :: Int
  , _pointBuyCosts :: IM.IntMap Int
  , _stats :: StatBlock
  , _race :: Race
  , _racialBonuses :: StatBlock
  } deriving (Show, Eq, Generic)
instance Serialize StateData where
instance ToJSON StateData where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON StateData

instance Default StateData where
  def = StateData { _tab = Calculator
              , _availablePoints = 27
              , _maxPurchasableAttribute = 15
              , _minPurchasableAttribute = 8
              , _pointBuyCosts = defPointBuyCosts
              , _stats = defaultStats
              , _race = def
              , _racialBonuses = defaultBonuses
              }

makeLenses ''StateData

data Model
  = Model
  { _stateData :: StateData
  , _defaultRacialBonuses :: M.Map Race StatBlock
  } deriving (Show, Eq, Generic)
instance Serialize Model where
instance ToJSON Model where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Model

instance Default Model where
  def = Model { _stateData = def
              , _defaultRacialBonuses = M.empty
              }

makeLenses ''Model

pointBuyCostValue :: Int -> Lens' Model Int
pointBuyCostValue k = 
  let
    getter record =
      let
        pbc = _pointBuyCosts (_stateData record)
      in 
        IM.findWithDefault 0 k pbc
    setter record field =
      let
        sd  = _stateData record
        pbc = _pointBuyCosts sd
      in
        record { _stateData = sd { _pointBuyCosts = IM.insert k field pbc } }
  in
    lens getter setter

data Action
  = ChangeTitle MisoString
  | ChangeTab (Lens' Model Tab) Tab
  | ChangeInt (Lens' Model Int) MisoString
  | ChangeRace MisoString
  | LoadModel
  | Reset
  | SaveModel
  | SetModel Model
  | FetchData
  | SetData (Response Model)
  | ErrorHandler (Response MisoString)
  | Log String