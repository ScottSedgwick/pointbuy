module Common.Components
  ( banner
  ) where

import           Miso               ( ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Property as P
import           Miso.Property      ( textProp )

import           Common.Applications ( Appl, applIcon )
import           Common.Classes

-- banner :: forall m.Appl -> View m a
banner a = 
  H.header_ [ P.class_ "primary-container" ]
  [ H.nav_ [] 
    [ H.button_ [ P.class_ "circle transparent" ] [ H.i_ [ P.class_ "responsive" ] [ text (applIcon a) ] ]
    , H.h6_ [ P.class_ "max center-align" ] [ text (ms $ showPretty a) ]
    , H.button_ [ P.class_ "circle transparent" ] [ H.i_ [ P.class_ "responsive" ] [ text (applIcon a) ] ]
    ]
  ]