module Common.Applications 
  ( Appl(..)
  , applIcon
  ) where

import           Common.Classes
import           Data.Default       ( Default, def )
import           GHC.Generics       ( Generic )
import           Miso               ( MisoString )

data Appl 
  = Backgrounds
  | Crafting
  | DiceRoller
  | Feats
  | Insults
  | Lineages
  | MagicItems
  | PointBuy
  | Spells
  | Ciphers
  deriving stock (Show, Eq, Enum, Bounded, Generic)
instance Default Appl where
  def = Crafting
instance ShowPretty Appl where
  showPretty Backgrounds = "Backgrounds"
  showPretty Crafting    = "Crafting"
  showPretty DiceRoller  = "Dice Roller"
  showPretty Feats       = "Feats"
  showPretty Insults     = "Insults"
  showPretty Lineages    = "Lineages"
  showPretty MagicItems  = "Magic Items"
  showPretty PointBuy    = "Point Buy Calculator"
  showPretty Spells      = "Spells"
  showPretty Ciphers     = "Ciphers"

applIcon :: Appl -> MisoString
applIcon Backgrounds = "history"
applIcon Crafting    = "construction"
applIcon DiceRoller  = "casino"
applIcon Feats       = "trophy"
applIcon Insults     = "partner_reports"
applIcon Lineages    = "group"
applIcon MagicItems  = "star"
applIcon PointBuy    = "calculate"
applIcon Spells      = "explosion"
applIcon Ciphers     = "password"