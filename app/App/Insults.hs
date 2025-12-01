module App.Insults 
  ( Model
  , page
  ) where

import           Common.Applications ( Appl( Insults ) )
import           Common.Classes
import           Common.Components   ( banner )
import           Control.Lens       ( (+=), (^.) )
import           Control.Lens.TH    ( makeLenses )
import           Data.Default       ( Default, def )
import           GHC.Generics       ( Generic )
import           Miso               ( Component, Effect, MisoString, Transition, View, component, ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P

data Action
  = Increment
  | Decrement
  deriving (Show, Eq)

data Model = Model
  { _value :: Int
  } deriving (Show, Eq, Generic)
instance Default Model where
  def = Model { _value = 5 }
makeLenses ''Model

updateModel :: Action -> Effect a Model Action
updateModel Increment = value += 1
updateModel Decrement = value += (-1)

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Insults
  , H.p_ [] [ text (ms (show (m ^. value))) ]
  , H.button_ [ E.onClick Increment ] [ text "Increment" ]
  , H.button_ [ E.onClick Decrement ] [ text "Decrement" ]
  ]

page :: Model -> Component a Model Action
page model = component model updateModel viewModel