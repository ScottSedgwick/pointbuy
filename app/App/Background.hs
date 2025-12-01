module App.Background 
  ( Model
  , page
  ) where

import           Control.Lens       ( (.=), (^.) )
import           Control.Lens.TH    ( makeLenses )
import           Data.Default       ( Default, def )
import           GHC.Generics       ( Generic )
import           Miso               ( Component, Effect, MisoString, Transition, View, component, ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P

import           Common.Applications ( Appl( Backgrounds ) )
import           Common.Classes
import           Common.Components   ( banner )
import           Common.Sources      ( Source, allSources )
import           Common.Unshow       ( unshow )

data Action
  = ChangeTitle String
  | ChangeSource String
  deriving (Show, Eq)

data Model = Model
  { _filterTitle :: String
  , _filterSource :: Maybe Source
  } deriving (Show, Eq, Generic)
instance Default Model where
  def = Model 
        { _filterTitle = ""
        , _filterSource = Nothing
        }
makeLenses ''Model

updateModel :: Action -> Effect a Model Action
updateModel (ChangeTitle  s) = filterTitle .= s
updateModel (ChangeSource s) = filterSource .= unshow s

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Backgrounds
  , H.p_ [] [ text (ms (show (m ^. filterTitle))) ]
  , H.button_ [ ] [ text "Increment" ]
  , H.button_ [ ] [ text "Decrement" ]
  ]

page :: Model -> Component a Model Action
page model = component model updateModel viewModel