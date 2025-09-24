{-# LANGUAGE OverloadedStrings #-}
module View.Raw where

import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P

import           Types

viewRaw :: Model -> View Model Action
viewRaw x =
  H.article_ [ P.className "fill" ]
  [ H.h2_ [] [ text "Rules as Written" ]
  , H.h4_ [] [ text "Default Point Buy Rules" ]
  , H.ul_ []
    [ H.li_ [] [ text "Available Points - 27"]
    , H.li_ [] [ text "Maximum Attriute Before Racial Bonus - 15"]
    , H.li_ [] [ text "Minimum Attribute Before Racial Bonus - 8"]
    ]
  , H.h4_ [] [ text "Subraces and Variants" ]
  , H.ul_ []
    [ H.li_ [] [ text "Subraces must be chosen for a race. Variants are considered optional. Currently Half-Elves, Humans, Orcs, and Tieflings have optional racial Variants. Other races that have no Subrace or Variant include Aarakocra, Bugbear, Centaur, Changeling, Dragonborn, Firbolg, Goblin, Goliath, Grung, Half-Orc, Hobgoblin, Kalashtar, Kenku, Kobold, Leonin, Lizardfolk, Locathah, Loxodon, Minotaur, Orc, Satyr, Simic Hybrid, Tabaxi, Tortle, Triton, Vedalken, Warforged, and Yuan-Ti Pureblood."]
    ]
  , H.h4_ [] [ text "Ask" ]
  , H.ul_ []
    [ H.li_ [] [ text "Always ask your DM first before picking a race, especially a non-standard race or subrace. Some races, a Loxodon as an example, require the character to come from a different world setting than the traditional Forgotten Realms."]
    ]
  ]