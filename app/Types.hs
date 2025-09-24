module Types where

import           Data.Default
import qualified Data.IntMap as IM
import           Miso
import           Miso.Lens

import Types.Races
import Types.Stats

data Action
  = ChangeTitle MisoString
  | ChangeTab (Lens Model Tab) Tab
  | ChangeInt (Lens Model Int) MisoString
  | ChangeRace MisoString
  | ResetCustomValues

data Tab 
  = Calculator
  | Custom
  | Raw
  deriving (Eq, Enum, Bounded)

instance Show Tab where
  show Calculator = "Calculator"
  show Custom     = "Custom Rules"
  show Raw        = "Rules as Written"

statLens :: Stat -> Lens StatBlock Int
statLens Strength = str
statLens Dexterity = dex
statLens Constitution = con
statLens Intelligence = int
statLens Wisdom = wis
statLens Charisma = cha

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
  } deriving (Show, Eq)

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

tab :: Lens Model Tab
tab = lens _tab $ \record field -> record { _tab = field }

availablePoints :: Lens Model Int
availablePoints = lens _availablePoints $ \record field -> record { _availablePoints = field }

maxPurchasableAttribute :: Lens Model Int
maxPurchasableAttribute = lens _maxPurchasableAttribute $ \record field -> record { _maxPurchasableAttribute = field }

minPurchasableAttribute :: Lens Model Int
minPurchasableAttribute = lens _minPurchasableAttribute $ \record field -> record { _minPurchasableAttribute = field }

pointBuyCosts :: Lens Model (IM.IntMap Int)
pointBuyCosts = lens _pointBuyCosts $ \record field -> record { _pointBuyCosts = field }

pointBuyCostValue :: Int -> Lens Model Int
pointBuyCostValue k = lens (\record -> IM.findWithDefault 0 k (_pointBuyCosts record)) (\record field -> record { _pointBuyCosts = IM.insert k field (_pointBuyCosts record) } )

race :: Lens Model Race
race = lens _race $ \record field -> record { _race = field }

stat :: Lens Model StatBlock
stat = lens _stats $ \record field -> record { _stats = field }

racialBonuses :: Lens Model StatBlock
racialBonuses = lens _racialBonuses $ \record field -> record { _racialBonuses = field }

racialBonus :: Lens StatBlock Int -> Lens Model Int
racialBonus l = compose l racialBonuses

defPointBuyCosts :: IM.IntMap Int
defPointBuyCosts = IM.fromList [ (3,-9), (4,-6), (5,-4), (6,-2), (7,-1), (8,0), (9,1), (10,2), (11,3), (12,4), (13,5), (14,7), (15,9), (16,12), (17,15), (18,19) ]

allStats :: [Stat]
allStats = [minBound .. maxBound]

defaultStats :: StatBlock
defaultStats = StatBlock { _str = 8, _dex = 8, _con = 8, _int = 8, _wis = 8, _cha = 8 }

defaultBonuses :: StatBlock
defaultBonuses = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 0 }
