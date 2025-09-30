{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveGeneric #-}
module Types where

import           Control.Lens    ( Lens', lens )
import           Control.Lens.TH ( makeLenses )
import           Data.Default    ( Default, def )
import qualified Data.IntMap     as IM
import           Data.Serialize  ( Serialize )
import           GHC.Generics    ( Generic )
import           Miso            ( MisoString )

import           Types.Races     ( Race )
import           Types.Stats     ( Stat(..), StatBlock(..), str, con, dex, int, wis, cha )

data Action
  = ChangeTitle MisoString
  | ChangeTab (Lens' Model Tab) Tab
  | ChangeInt (Lens' Model Int) MisoString
  | ChangeRace MisoString
  | LoadModel
  | Reset
  | SaveModel
  | SetModel Model
  | Log String

data Tab 
  = Calculator
  | Custom
  | Raw
  deriving (Eq, Enum, Bounded, Generic)
instance Serialize Tab where
instance Show Tab where
  show Calculator = "Calculator"
  show Custom     = "Custom Rules"
  show Raw        = "Rules as Written"

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

data Model
  = Model
  { _tab :: Tab
  , _availablePoints :: Int
  , _maxPurchasableAttribute :: Int
  , _minPurchasableAttribute :: Int
  , _pointBuyCosts :: IM.IntMap Int
  , _stats :: StatBlock
  , _race :: Race
  , _racialBonuses :: StatBlock
  } deriving (Show, Eq, Generic)
instance Serialize Model where

instance Default Model where
  def = Model { _tab = Calculator
              , _availablePoints = 27
              , _maxPurchasableAttribute = 15
              , _minPurchasableAttribute = 8
              , _pointBuyCosts = defPointBuyCosts
              , _stats = defaultStats
              , _race = def
              , _racialBonuses = defaultBonuses
              }

makeLenses ''Model

pointBuyCostValue :: Int -> Lens' Model Int
pointBuyCostValue k = lens (\record -> IM.findWithDefault 0 k (_pointBuyCosts record)) (\record field -> record { _pointBuyCosts = IM.insert k field (_pointBuyCosts record) } )
