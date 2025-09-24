module Types.Stats where

import           Miso.Lens

data Stat
  = Strength
  | Dexterity
  | Constitution
  | Intelligence
  | Wisdom
  | Charisma
  deriving (Eq, Enum, Bounded, Ord, Read, Show)

data StatBlock = StatBlock
  { _str :: Int
  , _dex :: Int
  , _con :: Int
  , _int :: Int
  , _wis :: Int
  , _cha:: Int
  } deriving (Show, Eq)

str :: Lens StatBlock Int
str = lens _str $ \record field -> record { _str = field }

dex :: Lens StatBlock Int
dex = lens _dex $ \record field -> record { _dex = field }

con :: Lens StatBlock Int
con = lens _con $ \record field -> record { _con = field }

int :: Lens StatBlock Int
int = lens _int $ \record field -> record { _int = field }

wis :: Lens StatBlock Int
wis = lens _wis $ \record field -> record { _wis = field }

cha :: Lens StatBlock Int
cha = lens _cha $ \record field -> record { _cha = field }