{-# LANGUAGE OverloadedStrings #-}
module View.Calculator where

import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens
import           Miso.Property ( textProp )

import Types
import Types.Races
import Types.Stats
import View.Calculator.Race

viewCalculator :: Model -> View Model Action
viewCalculator x =
  H.article_ [ P.className "grid" ]
  [ H.h2_ [ P.className "s12" ] [ text "Calculator" ]
  , H.article_ [ P.className "s6 fill" ]
    [ H.article_ [] [ raceSelector x ]
    , H.article_ []
      [ H.table_ [ P.className "stripes" ]
        [ H.thead_ []
          [ H.tr_ []
            [ H.th_ [] [ text "Attribute" ]
            , H.th_ [ P.className "centertext" ] [ text "Base Score" ]
            , H.th_ [ P.className "centertext" ] [ text "" ]
            , H.th_ [ P.className "centertext" ] [ text "Racial Bonus" ]
            , H.th_ [ P.className "centertext" ] [ text "" ]
            , H.th_ [ P.className "centertext" ] [ text "Total Score" ]
            , H.th_ [ P.className "centertext" ] [ text "Ability Modifier" ]
            , H.th_ [ P.className "centertext" ] [ text "Point Cost" ]
            ]
          ]
        , H.tbody_ [] ((map (mkAttributeRow x) allStats) ++ [ totalRow x ])
        ]
      ]
    ]
  , H.article_ [ P.className "s6 fill" ] ( H.h4_ [] [ text "Racial Traits"] : racialTraits (x ^. race) )
  ]

mkAttributeRow :: Model -> Stat -> View Model Action
mkAttributeRow x s =
    H.tr_ []
    [ H.td_ [] [ text (ms $ show s) ]
    , H.td_ [ P.className "centertext" ] [ numberField x sl (min, max) (ChangeInt sl) ]
    , H.td_ [ P.className "centertext" ] [ text "+" ]
    , H.td_ [ P.className "centertext" ] [ text (msshow (x ^. rbl)) ] -- in special cases, this can change (for selected races)
    , H.td_ [ P.className "centertext" ] [ text "=" ]
    , H.td_ [ P.className "centertext" ] [ H.b_ [] [ text (msshow total) ] ]
    , H.td_ [ P.className "centertext" ] [ text (msshow mod) ]
    , H.td_ [ P.className "centertext" ] [ text (msshow cost) ]
    ]
  where
    total = (x ^. sl) + (x ^. rbl)
    mod = modifier total
    cost = x ^. (pointBuyCostValue (x ^. sl))
    max = x ^. maxPurchasableAttribute
    min = x ^. minPurchasableAttribute
    rbl = compose (statLens s) racialBonuses
    sl = compose (statLens s) stat

totalRow :: Model -> View Model Action
totalRow x =
  H.tr_ []
  [ H.td_ [] []
  , H.td_ [ P.className "centertext" ] [ H.button_ [ E.onClick ResetCustomValues ] [ text "Reset" ] ]
  , H.td_ [] []
  , H.td_ [] []
  , H.td_ [] []
  , H.td_ [] []
  , H.td_ [] [ H.b_ [] [ text "Total Points" ] ]
  , H.td_ [ P.className "centertext" ] [ text $ ms (show totalCost <> " / " <> show (x ^. availablePoints)) ]
  ]
  where
    totalCost = sum (map (\s -> x ^. (pointBuyCostValue (x ^. (compose (statLens s) stat)))) allStats)

numberField :: Model -> Lens Model Int -> (Int, Int) -> (MisoString -> Action) -> View Model Action
numberField x l (minv, maxv) a =
  H.div_ [ P.className "field border" ] 
  [ H.input_ [ P.type_ "number", P.value_ ((msshow) (x ^. l)), P.min_ (msshow minv), P.max_ (msshow maxv), P.step_ "1", H.onInput a, P.className "showspinner centertext" ]
  ]
  
msshow :: Show a => a -> MisoString
msshow = ms . show

modifier :: Int -> Int
modifier n = (n - 10) `div` 2