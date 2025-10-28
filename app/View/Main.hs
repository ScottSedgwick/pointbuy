{-# LANGUAGE OverloadedStrings #-}

module View.Main where

import           Control.Lens       ( (^.) )
import           Miso               ( View, ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Property as P

import           Types              ( Action(..), Model, Tab(..), stateData, tab, tabClass )
import           View.Calculator    ( viewCalculator )
import           View.Custom        ( viewCustom )
import           View.Raw           ( viewRaw )

viewModel :: Model -> View Model Action
viewModel x = H.div_ [] [ H.div_ [] ( viewTabs x ) ]

viewTabs :: Model -> [ View Model Action ]
viewTabs x = 
  let
    allTabs = [minBound .. maxBound]
    tabset = H.div_ [ P.className "tabs" ] (map (viewTab x) allTabs)
    tabbody = map (viewTabForm x) allTabs
  in
    tabset : tabbody

viewTab :: Model -> Tab -> View Model Action
viewTab x a =
  if (x ^. (stateData . tab) == a)
    then H.a_ [ P.className "active", H.onClick (ChangeTab (stateData . tab) a) ] [ text (ms $ show a) ]
    else H.a_ [ H.onClick (ChangeTab (stateData . tab) a) ] [ text (ms $ show a) ]

viewTabForm :: Model -> Tab -> View Model Action
viewTabForm m t =
  let 
    cls = if (m ^. (stateData . tab)) == t then "page padding active " else "page padding"
  in
    H.div_ [ P.className (cls <> " " <> (ms $ tabClass t)) ] [ viewTabBody m t ]

viewTabBody :: Model -> Tab -> View Model Action
viewTabBody x Calculator = H.div_ [] [ viewCalculator x ]
viewTabBody x Custom     = H.div_ [] [ viewCustom x ]
viewTabBody x Raw        = H.div_ [] [ viewRaw x ]
viewTabBody x Races      = H.div_ [] [ text "Races" ]