{-# LANGUAGE OverloadedStrings #-}
module View.Custom where

import qualified Data.IntMap as IM
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens

import           Types

viewCustom :: Model -> View Model Action
viewCustom x =
  H.article_ [ P.className "fill" ]
  [ H.h2_ [] [ text "Custom Rules" ]
  , H.article_ [] [ H.p_ [] [ text "Your DM may use House Rules regarding the Point Buy system. Adjust the numbers below to meet those rules." ] ]
  , H.article_ [ P.className "grid"]
    [ H.div_ [ P.className "s3" ] 
      [ H.div_ [ P.className "field border" ] 
        [ H.input_ [ P.type_ "number", P.value_ ((ms . show) (x ^. availablePoints)), P.min_ "0", P.max_ "100", P.step_ "1", H.onInput (ChangeInt availablePoints) ]
        , H.label_ [] [ text "Available Points" ]
        ]
      , H.div_ [ P.className "field border" ] 
        [ H.input_ [ P.type_ "number", P.value_ ((ms . show) (x ^. maxPurchasableAttribute)), P.min_ ((ms . show) (x ^. minPurchasableAttribute)), P.max_ "18", P.step_ "1", H.onInput (ChangeInt maxPurchasableAttribute) ] 
        , H.label_ [] [ text "Maximum Purchasable Attribute Before Bonuses" ]
        ]
      , H.div_ [ P.className "field border" ] 
        [ H.input_ [ P.type_ "number", P.value_ ((ms . show) (x ^. minPurchasableAttribute)), P.min_ "3", P.max_ ((ms . show) (x ^. maxPurchasableAttribute)), P.step_ "1", H.onInput (ChangeInt minPurchasableAttribute) ] 
        , H.label_ [] [ text "Minimum Purchasable Attribute Before Bonuses" ]
        ]
      , H.div_ [ P.className "field border" ]
        [ H.button_ [ P.className "responsive", H.onClick ResetCustomValues ] [ text "Reset to Default Values" ]
        ]
      ]
    , H.div_ [ P.className "s1" ] []
    , H.div_ [ P.className "s8 grid" ] (map (viewPointBuyCost x) (reverse $ IM.keys (x ^. pointBuyCosts)))
    ]
  ]

viewPointBuyCost :: Model -> Int -> View Model Action
viewPointBuyCost x k =
  H.div_ [ P.className "s3" ] 
  [ H.div_ [ P.className "field border" ] 
    [ H.input_ [ P.type_ "number", P.value_ ((ms.show) (x ^. (pointBuyCostValue k))), P.min_ "-20", P.max_ "20", P.step_ "1", H.onInput (ChangeInt (pointBuyCostValue k)), P.className "showspinner" ]
    , H.label_ [] [ text (ms $ "Cost for " ++ show k) ]
    ]
  ]
