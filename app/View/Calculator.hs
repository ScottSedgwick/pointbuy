{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module View.Calculator where

import           Control.Lens         ( Lens', (^.) )
import           Miso                 ( MisoString, View, ms, text )
import qualified Miso.Html            as H
import qualified Miso.Html.Event      as E
import qualified Miso.Html.Property   as P
import qualified Miso.Property        as MP

import           Types                ( Action(..), Model, allStats, availablePoints, maxPurchasableAttribute, minPurchasableAttribute, pointBuyCostValue, race, racialBonuses, statLens, stats )
import           Types.Stats          ( Stat, showPretty )
import           View.Calculator.Race ( raceSelector, racialTraits )

viewCalculator :: Model -> View Model Action
viewCalculator x =
  H.article_ [ P.className "grid" ]
  [ H.div_ [ P.className "s12" ] [ H.h2_ [] [ text "Calculator" ] ]
  , H.div_ [ P.className "s12 m6" ] 
    [ H.article_ [ P.className "s12 fill" ]
      [ H.article_ [] [ raceSelector x ]
      , H.article_ [ P.className "grid s12" ]
        ( [ H.div_ [ P.className "s1" ] [ H.p_ [] [ H.b_ [] [ text "Attribute" ] ] ]
          , H.div_ [ P.className "s2" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "Base Score" ] ] ]
          , H.div_ [ P.className "s1" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "" ] ] ]
          , H.div_ [ P.className "s2" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "Racial Bonus" ] ] ]
          , H.div_ [ P.className "s1" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "" ] ] ]
          , H.div_ [ P.className "s2" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "Total Score" ] ] ]
          , H.div_ [ P.className "s1" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "Ability Modifier" ] ] ]
          , H.div_ [ P.className "s2" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "Point Cost" ] ] ]
          ] 
          <> (map (mkAttributeRow x) allStats) 
          <> totalRow x
        )
      ]
    ]
  , H.div_ [ P.className "s12 m6" ] 
    [ H.article_ [ P.className "s12 fill" ] 
      [ H.h4_ [] [ text "Racial Traits"]
      , H.article_ [] ( racialTraits (x ^. race) ) 
      ]
    ]
  ]

mkAttributeRow :: Model -> Stat -> View Model Action
mkAttributeRow x s =
  let
    total = (x ^. (sl s)) + (x ^. (rl s))
    mod = modifier total
    cost = x ^. (pointBuyCostValue (x ^. (sl s)))
    max = x ^. maxPurchasableAttribute
    min = x ^. minPurchasableAttribute
  in
    H.div_ [ P.className "s12 grid fill", MP.textProp "style" "padding-left: 10px; padding-bottom: 0px; padding-top: 10px;" ]
    [ H.div_ [ P.className "s1 small" ] [ H.p_ [ P.className "small" ] [ H.b_ [] [ text (ms $ showPretty s) ] ] ]
    , H.div_ [ P.className "s2 small" ] [ numberField x (sl s) (min, max) (ChangeInt (sl s)) ]
    , H.div_ [ P.className "s1 small" ] [ H.p_ [ P.className "center-align small" ] [ text "+" ] ]
    , H.div_ [ P.className "s2 small" ] [ H.p_ [ P.className "center-align small" ] [ text (msshow (x ^. (rl s))) ]]  -- in special cases, this can change (for selected races)
    , H.div_ [ P.className "s1 small" ] [ H.p_ [ P.className "center-align small" ] [ text "=" ] ]
    , H.div_ [ P.className "s2 small" ] [ H.p_ [ P.className "center-align small" ] [ H.b_ [] [ text (msshow total) ] ] ]
    , H.div_ [ P.className "s1 small" ] [ H.p_ [ P.className "center-align small" ] [ text (msshow mod) ] ]
    , H.div_ [ P.className "s2 small" ] [ H.p_ [ P.className "center-align small" ] [ text (msshow cost) ] ]
    ]

sl :: Stat -> Lens' Model Int
sl s = stats . (statLens s)

rl :: Stat -> Lens' Model Int
rl s = racialBonuses . (statLens s)

totalRow :: Model -> [View Model Action]
totalRow x =
  [ H.div_ [ P.className "s2" ] [ H.button_ [ E.onClick Reset ] [ text "Reset" ] ]
  , H.div_ [ P.className "s6" ] []
  , H.div_ [ P.className "s2" ] [ H.p_ [ P.className "center-align" ] [ H.b_ [] [ text "Total Points" ] ] ]
  , H.div_ [ P.className "s2" ] [ H.p_ [ P.className "center-align" ] [ text $ ms (show totalCost <> " / " <> show (x ^. availablePoints)) ] ]
  ]
  where
    totalCost = sum (map (\s -> x ^. (pointBuyCostValue (x ^. (stats . (statLens s))))) allStats)

numberField :: Model -> Lens' Model Int -> (Int, Int) -> (MisoString -> Action) -> View Model Action
numberField x l (minv, maxv) a =
  H.div_ [ P.className "field border small" ] 
  [ H.input_ [ P.type_ "number", P.value_ ((msshow) (x ^. l)), P.min_ (msshow minv), P.max_ (msshow maxv), P.step_ "1", H.onInput a, P.className "showspinner centertext" ]
  ]
  
msshow :: Show a => a -> MisoString
msshow = ms . show

modifier :: Int -> Int
modifier n = (n - 10) `div` 2