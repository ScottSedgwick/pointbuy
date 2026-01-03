module App.Insults 
  ( Model
  , page
  ) where

import           Common.Applications ( Appl( Insults ) )
import           Common.Classes
import           Common.Components   ( banner )
import           Control.Lens       ( (.=), (+=), (^.), (?=) )
import           Control.Lens.TH    ( makeLenses )
import           Data.Aeson         ( FromJSON )
import           Data.Default       ( Default, def )
import           GHC.Generics       ( Generic )
import           Miso.DSL           ( FromJSVal )
import           Miso               ( Component ( initialAction ), Effect, MisoString, Transition, View, (<#), component, fromMisoString, get, io, ms, text )
import           Miso.Fetch         ( Response(body, errorMessage), getJSON )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P
import           Language.Javascript.JSaddle.Monad ( JSM, liftJSM )
import           System.Random      ( randomRIO )

data InsultJSON = InsultJSON
  { insults :: [String]
  } deriving (Show, Eq, Generic)
instance FromJSON InsultJSON
instance FromJSVal InsultJSON

data Action
  = GetInsults
  | Generate
  | SetCurrent String
  | ErrorHandler (Response InsultJSON)
  | SetInsults (Response InsultJSON)

data Model = Model
  { _current :: String
  , _options :: [ String ]
  } deriving (Show, Eq, Generic)
instance Default Model where
  def = Model { _current = "", _options = [] }
makeLenses ''Model

updateModel :: Action -> Effect a Model Action
updateModel GetInsults       = getJSON "data/insults.json" [] SetInsults ErrorHandler
updateModel (SetCurrent x)   = current .= x
updateModel (ErrorHandler r) = exec (SetCurrent (fromMisoString $ maybe "Unknown error." id (errorMessage r)))
updateModel (SetInsults r)   = options .= (insults (body r))
updateModel Generate         = do
  m <- get
  io $ do
    s <- pickRandom ( m ^. options )
    pure (SetCurrent s)

exec :: Action -> Effect a Model Action
exec a = get >>= \model -> model <# (pure a)

pickRandom :: [a] -> IO a
pickRandom xs = do
  i <- randomRIO (0, length xs - 1)
  pure $ xs !! i

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Insults
  , H.div_ [ P.class_ "field textarea border" ] [ H.textarea_ [ P.value_ (ms $ m ^. current) ] [] ]
  , H.button_ [ E.onClick Generate ] [ text "Generate Insult" ]
  ]

-- Component constructor ---------------------------------
page :: Model -> Component a Model Action
page model = p { initialAction = Just GetInsults }
  where
    p = component model updateModel viewModel