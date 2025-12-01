module Common.Components
  ( banner
  ) where

import           Miso               ( ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Property as P

import           Common.Applications ( Appl, applIcon )
import           Common.Classes

-- banner :: forall m.Appl -> View m a
banner a = 
  H.div_ [ P.class_ "banner tiny-height middle-align center-align" ]
  [ H.i_ [ P.class_ "small-margin" ] [ text (applIcon a) ]
  , H.div_ [ P.class_ "small-margin" ] 
    [ H.span_ [] [ text (ms $ showPretty a) ]
    ]
  ]