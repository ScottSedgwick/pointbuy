module App.Background 
  ( Model
  , page
  ) where

import           Control.Lens       ( (.=), (^.) )
import           Control.Lens.TH    ( makeLenses )
import           Data.Aeson         ( FromJSON )
import qualified Data.List          as L
import qualified Data.Map           as M
import           Data.Default       ( Default, def )
import           GHC.Generics       ( Generic )
import           GHCJS.Marshal      ( FromJSVal )
import           Miso               ( Attribute, Component, Effect, MisoString, Transition, View, component, fromMisoString, initialAction, ms, text )
import           Miso.Fetch         ( Response(body, errorMessage), getJSON )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P

import           Common.Applications ( Appl( Backgrounds ) )
import           Common.Classes
import           Common.Components   ( banner )
import           Common.DataTypes.Background 
import           Common.DataTypes.Inline
import           Common.Sources      ( Source, allSources )
import           Common.Unshow       ( unshow )
import           Common.Utils        ( maybeHead, toLower )

data Action
  = GetBackgrounds
  | SetBackgrounds (Response [Background])
  | ErrorHandler (Response MisoString)
  | UpdateFilter MisoString
  | SetPage String

data Model = Model
  { _filterTitle :: String
  , _backgrounds :: [Background]
  , _selectedfile :: Maybe String
  , _selecteddata :: Maybe String
  , _errMessage :: Maybe MisoString
  } deriving (Show, Eq, Generic)
instance Default Model where
  def = Model 
        { _filterTitle = ""
        , _backgrounds = []
        , _selectedfile = Nothing
        , _selecteddata = Nothing
        , _errMessage = Nothing
        }
makeLenses ''Model

updateModel :: Action -> Effect a Model Action
updateModel (GetBackgrounds)     = getJSON "data/backgrounds.json" [] SetBackgrounds ErrorHandler
updateModel (SetBackgrounds r)   = backgrounds .= (body r)
updateModel (ErrorHandler s)     = errMessage .= (errorMessage s)
updateModel (UpdateFilter s)     = filterTitle .= (fromMisoString s)
updateModel (SetPage s)          = selecteddata .= Just s

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Backgrounds
  , H.div_ [] (filterView : (map backgroundView (filteredBackgrounds m)))
  , H.div_ [] [ H.p_ [] [ text ( maybe "" id (m ^. errMessage) ) ] ]
  ]

filterView :: View Model Action
filterView =
  H.header_ [ P.class_ "fixed" ]
  [ H.article_ [ P.class_ "white" ]
    [ H.div_ [ P.class_ "grid" ]
      [ H.div_ [ P.class_ "s12" ]
        [ H.div_ [ P.class_ "field label prefix border" ]
          [ H.input_ [ P.type_ "text", E.onInput UpdateFilter ]
          , H.label_ [] [ text "Background" ]
          , H.i_ [ P.class_ "front" ] [ text "search" ]
          ]
        ]
      ]
    ]
  ]

filteredBackgrounds :: Model -> [Background]
filteredBackgrounds m = filter (\b -> (toLower $ m ^. filterTitle) `L.isInfixOf` (toLower $ b ^. title)) (m ^. backgrounds)

backgroundView :: Background -> View Model Action
backgroundView b = 
  H.article_ [ P.class_ "grey3" ]
  [ H.details_ [] 
    [ H.summary_ []
      [ H.div_ [ P.class_ "grid tiny-line" ]
        [ H.div_ [ P.class_ "s6" ] [ H.strong_ [] [ text ( ms $ b ^. title ) ] ]
        , H.div_ [ P.class_ "s6" ] [ text ( ms $ b ^. source ) ]
        ]
      ]
    , H.article_ [ P.class_ "white" ] 
      ( descriptionView b
      <> sourceView b
      <> proficienciesView b
      <> featuresView (b ^. features)
      <> suggestedView (b ^. suggested)
      <> traitsView (b ^. traits)
      )
    ]
  ]

descriptionView :: Background -> [View Model Action]
descriptionView b = map (\d -> H.p_ [] [ H.strong_ [] [ text ( ms d ) ] ]) (b ^. description)

sourceView :: Background -> [View Model Action]
sourceView b = 
  [ H.p_ [] 
    [ H.strong_ [] [ text "Source: " ]
    , H.a_ [ P.src_ (ms $ b ^. sourceurl) ] [ text (ms $ b ^. source ) ]
    ] 
  ]

proficienciesView :: Background -> [View Model Action]
proficienciesView b = 
  [ H.h4_ [] [ text "Proficiencies" ] 
  , H.p_ [] 
    [ H.strong_ [] [ text "Skill Proficiencies: " ], text (ms $ L.intercalate ", " $ b ^. (proficiencies . skill)), H.br_ []
    , H.strong_ [] [ text "Tool Proficiencies: " ], text (ms $ L.intercalate ", " $ b ^. (proficiencies . tool)), H.br_ []
    , H.strong_ [] [ text "Languages: " ], text (ms $ L.intercalate ", " $ b ^. (proficiencies . languages)), H.br_ []
    , H.strong_ [] [ text "Equipment: " ], text (ms $ L.intercalate ", " $ b ^. equipment)
    ]
  ]

featuresView :: [BackgroundFeature] -> [View Model Action]
featuresView [] = []
featuresView xs = ( H.h4_ [] [ text "Features" ] ) : (concatMap featureView xs)

featureView :: BackgroundFeature -> [View Model Action]
featureView f = ( H.h6_ [] [ text (ms $ f ^. featureTitle) ] ) : (map renderInline (f ^. featureDescription))

suggestedView :: [String] -> [View Model Action]
suggestedView [] = []
suggestedView xs = ( H.h4_ [] [ text "Suggested Characteristics"] ) : map f xs
  where
    f x = H.p_ [] [ text ( ms $ x ) ]   

traitsView :: Maybe BackgroundTraits -> [View Model Action]
traitsView Nothing  = []
traitsView (Just t) = 
  [ H.div_ [ P.class_ "grid" ]
    [ traitTable "Personality Trait" ( t ^. personality )
    , traitTable "Ideal" ( t ^. ideals )
    , traitTable "Bond" ( t ^. bonds )
    , traitTable "Flaw" ( t ^. flaws )
    ]
  ]

traitTable :: String -> [String] -> View Model Action
traitTable tableName xs =
  H.div_ [ P.class_ "s6" ]
  [ H.h4_ [] [ text $ ms (tableName <> "s") ]
  , stripeTable tableName xs
  ]

page :: Model -> Component a Model Action
page model = p { initialAction = Just GetBackgrounds }
  where
    p = component model updateModel viewModel