{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveGeneric #-}
module Types.Stats where

import Control.Lens.TH ( makeLenses )
import Data.Serialize  ( Serialize )
import GHC.Generics    ( Generic )

data Stat
  = Strength
  | Dexterity
  | Constitution
  | Intelligence
  | Wisdom
  | Charisma
  deriving (Eq, Enum, Bounded, Ord, Read, Show)

showPretty :: Stat -> String
showPretty Strength = "Str"
showPretty Dexterity = "Dex"
showPretty Constitution = "Con"
showPretty Intelligence = "Int"
showPretty Wisdom = "Wis"
showPretty Charisma = "Cha"

data StatBlock = StatBlock
  { _str :: Int
  , _dex :: Int
  , _con :: Int
  , _int :: Int
  , _wis :: Int
  , _cha:: Int
  } deriving (Show, Eq, Generic)
instance Serialize StatBlock where

makeLenses ''StatBlock
