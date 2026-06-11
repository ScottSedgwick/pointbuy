module App.Items 
  ( Model
  , page
  ) where

import           Common.Applications   ( Appl( MagicItems ) )
import           Common.Classes
import           Common.Components     ( banner )
import           Common.DataTypes.Item ( Item )
import           Control.Lens          ( (+=), (^.), (.=) )
import           Control.Lens.TH       ( makeLenses )
import           Data.Default          ( Default, def )
import           GHC.Generics          ( Generic )
import           Miso                  ( Component (mount), Effect, MisoString, Response, View, body, component, errorMessage, getText, ms, text )
import qualified Miso.CSS              as C
import qualified Miso.Html             as H
import qualified Miso.Html.Event       as E
import qualified Miso.Html.Property    as P
import           Miso.JSON             ( eitherDecode )

data Action
  = GetItems
  | SetItems (Response MisoString)
  | ErrorHandler (Response MisoString)

data Model = Model
  { _filterName :: MisoString
  , _filterRarity :: MisoString
  , _filterType :: MisoString
  , _filterAttunement :: MisoString
  , _filterSource :: MisoString
  , _items :: Either MisoString [Item]
  , _errMessage :: Maybe MisoString
  } deriving (Show, Eq, Generic)
instance Default Model where
  def = Model 
    { _filterName = ""
    , _filterRarity = "All"
    , _filterType = "All"
    , _filterAttunement = "All"
    , _filterSource = "All"
    , _items = Right []
    , _errMessage = Nothing
    }
makeLenses ''Model

updateModel :: Action -> Effect a Model Action
updateModel GetItems              = getText "data/items.json" [] SetItems ErrorHandler
updateModel (SetItems r)          = items .= (eitherDecode (body r))
updateModel (ErrorHandler s)      = errMessage .= (errorMessage s)

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner MagicItems
  , H.div_ [] (filterView m : (map itemView (filteredItems m)))
  , H.div_ [] [ H.p_ [] [ text ( maybe "" id (m ^. errMessage) ) ] ]
  ]

filterView :: Model -> View Model Action
filterView m =
  H.header_ [ P.class_ "fixed tiny-margin-top" ]
  [ H.article_ [ P.class_ "white" ]
    [ H.div_ [ P.class_ "grid" ]
      [ H.div_ [ P.class_ "s3" ] [] -- [ mkTextFilter "Spell" UpdateTitleFilter ]
      , H.div_ [ P.class_ "s2" ] [] -- [ mkSelectFilter "Spell Level" UpdateLevelFilter (map (\x -> (show x, showLevel x)) [(-1)..9]) ]
      , H.div_ [ P.class_ "s3" ] [] -- [ mkSelectFilter "Class" UpdateClassFilter (zip allClasses allClasses) ]
      , H.div_ [ P.class_ "s2" ] [] -- [ mkSelectFilter "School" UpdateSchoolFilter (zip allSchools allSchools) ]
      , H.div_ [ P.class_ "s2" ] [] -- [ mkTextFilter "Source" UpdateSourceFilter ]
      ]
    ]
  ]

filteredItems :: Model -> [Item]
filteredItems m =
  case (m ^. items) of
    Left err -> []
    Right xs -> xs

itemView :: Item -> View Model Action
itemView x =
  H.article_ [ P.class_ "grey3" ]
  [ H.details_ [] 
    [ H.summary_ []
      [ H.div_ [ P.class_ "grid tiny-line" ]
        [ H.div_ [ P.class_ "s3" ] [] -- [ H.strong_ [] [ text ( ms $ s ^. title ) ] ]
        , H.div_ [ P.class_ "s2" ] [] -- [ text ( ms $ showLevel (s ^. level) ) ]
        , H.div_ [ P.class_ "s3" ] [] -- [ text ( intercalate ", " (s ^. lists) ) ]
        , H.div_ [ P.class_ "s2" ] [] -- [ text ( s ^. school ) ]
        , H.div_ [ P.class_ "s2" ] [] -- [ text ( intercalate ", " (s ^. source) ) ]
        ]
      ]
    , H.article_ [ P.class_ "white" ] 
      [ H.div_ [ P.class_ "grid", C.style_ [ C.gap "0rem" ] ]
        [ H.b_ [ P.class_ "s1" ]    [] -- [ text "Casting Time:" ]
        , H.p_ [ P.class_ "s11" ]   [] -- [ text (ms $ s ^. castingTime ) ]
        , H.b_ [ P.class_ "s1" ]    [] -- [ text "Range:" ]
        , H.p_ [ P.class_ "s11" ]   [] -- [ text (ms $ s ^. range ) ]
        , H.b_ [ P.class_ "s1" ]    [] -- [ text "Duration:" ]
        , H.p_ [ P.class_ "s11" ]   [] -- [ text (ms $ s ^. duration ) ]
        , H.b_ [ P.class_ "s1" ]    [] -- [ text "Components:" ]
        , H.p_ [ P.class_ "s11" ]   [] -- [ text (ms $ s ^. components ) ]
        , H.div_ [ P.class_ "s12" ] [] -- (map renderStructure (s ^. description))
        ]
      ]
    ]
  ] 

page :: Model -> Component a Model Action
page model = p { mount = Just GetItems }
  where
    p = component model updateModel viewModel