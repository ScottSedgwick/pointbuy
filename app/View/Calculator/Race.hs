{-# LANGUAGE OverloadedStrings #-}
module View.Calculator.Race where

import           Control.Lens       ( (^.) )
import qualified Data.Map           as M
import           Miso               ( View, ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P

import           Types              ( Action(..), Model, race, stateData )
import           Types.Races        ( Race, allRaces, racialData, showPretty )

raceSelector :: Model -> View Model Action
raceSelector x =
  case x ^. (stateData . race) of
    _ -> standardSelector x

standardSelector :: Model -> View Model Action
standardSelector x =
  H.div_ [ P.className "grid" ]
  [ raceSelect x
  ]

raceSelect :: Model -> View Model Action
raceSelect x =
  H.div_ [ P.className "field label suffix border s12"] 
  [ H.select_ [ E.onInput ChangeRace ] ( map (mkRaceOption (x ^. (stateData . race))) allRaces )
  , H.label_ [] [ text"Select Race" ]
  , H.i_ [] [ text "arrow_drop_down" ]
  ]

mkRaceOption :: Race -> Race -> View Model Action
mkRaceOption c r = H.option_ [ P.selected_ (c == r), P.value_ (ms $ show r) ] [ text (ms $ showPretty r) ]

aasimarSelector :: Model -> View Model Action
aasimarSelector x =
  H.div_ [ P.className "grid" ]
  [ raceSelect x
  , H.div_ [ P.className "s6" ] [ text "Aasimar" ]
  ]


racialTraits :: Race -> [View Model Action]
racialTraits = formatTraits . racialData

formatTraits :: [(String, [String])] -> [View Model Action]
formatTraits = concatMap (\(x,xs) -> [ H.h6_ [] [ text (ms x) ], H.ul_ [] (map (\y -> H.li_ [] [ text (ms y)]) xs)])
