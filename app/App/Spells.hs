module App.Spells 
  ( Model
  , page
  ) where

import           Common.Applications     ( Appl( Spells ) )
import           Common.Classes
import           Common.Components       ( banner )
import           Common.DataTypes.Inline ( renderStructure )
import           Common.DataTypes.Spell  ( Spell(..), castingTime, components, description, duration, level, lists, range, school, source, title )
import           Common.Utils            ( showLevel )
import           Control.Lens            ( (+=), (.=), (^.) )
import           Control.Lens.TH         ( makeLenses )
import           Data.Default            ( Default, def )
import           Data.Either             ( either )
import qualified Data.List               as L
import           GHC.Generics            ( Generic )
import           Miso                    ( Component (mount), Effect, MisoString, View, component, fromMisoString, ms, text )
import qualified Miso.CSS                as C
import           Miso.Fetch              ( Response(body, errorMessage), getJSON, getText )
import qualified Miso.Html               as H
import qualified Miso.Html.Event         as E
import qualified Miso.Html.Property      as P
import           Miso.JSON               ( eitherDecode )
import           Miso.String             ( intercalate, isInfixOf, toLower )

data Action
  = GetSpells
  | SetSpells (Response MisoString)
  | ErrorHandler (Response MisoString)
  | UpdateTitleFilter MisoString
  | UpdateLevelFilter MisoString
  | UpdateClassFilter MisoString
  | UpdateSchoolFilter MisoString
  | UpdateSourceFilter MisoString
  | SetPage MisoString

data Model = Model
  { _filterTitle :: MisoString
  , _filterLevel :: Int
  , _filterClass :: MisoString
  , _filterSchool :: MisoString
  , _filterSource :: MisoString
  , _spells :: Either MisoString [Spell]
  , _errMessage :: Maybe MisoString
  , _selecteddata :: Maybe MisoString
  } deriving (Show, Eq, Generic)
instance Default Model where
  def = Model 
    { _filterTitle = ""
    , _filterLevel = -1
    , _filterClass = "All"
    , _filterSchool = "All"
    , _filterSource = ""
    , _spells = Right []
    , _errMessage = Nothing
    , _selecteddata = Nothing
    }
makeLenses ''Model

updateModel :: Action -> Effect a Model Action
updateModel GetSpells              = getText "data/spells.json" [] SetSpells ErrorHandler
updateModel (SetSpells r)          = spells .= (eitherDecode (body r))
updateModel (ErrorHandler s)       = errMessage .= (errorMessage s)
updateModel (UpdateTitleFilter s)  = filterTitle .= (fromMisoString s)
updateModel (UpdateLevelFilter s)  = filterLevel .= (read $ fromMisoString s)
updateModel (UpdateClassFilter s)  = filterClass .= (fromMisoString s)
updateModel (UpdateSchoolFilter s) = filterSchool .= (fromMisoString s)
updateModel (UpdateSourceFilter s) = filterSource .= (fromMisoString s)
updateModel (SetPage s)            = selecteddata .= Just s

viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Spells
  , H.div_ [] (filterView m : (map spellView (filteredSpells m)))
  , H.div_ [] [ H.p_ [] [ text ( maybe "" id (m ^. errMessage) ) ] ]
  ]

filterView :: Model -> View Model Action
filterView m =
  H.header_ [ P.class_ "fixed tiny-margin-top" ]
  [ H.article_ [ P.class_ "white" ]
    [ H.div_ [ P.class_ "grid" ]
      [ H.div_ [ P.class_ "s3" ] [ mkTextFilter "Spell" UpdateTitleFilter ]
      , H.div_ [ P.class_ "s2" ] [ mkSelectFilter "Spell Level" UpdateLevelFilter (map (\x -> (show x, showLevel x)) [(-1)..9]) ]
      , H.div_ [ P.class_ "s3" ] [ mkSelectFilter "Class" UpdateClassFilter (zip allClasses allClasses) ]
      , H.div_ [ P.class_ "s2" ] [ mkSelectFilter "School" UpdateSchoolFilter (zip allSchools allSchools) ]
      , H.div_ [ P.class_ "s2" ] [ mkTextFilter "Source" UpdateSourceFilter ]
      ]
    ]
  ]

allClasses :: [String]
allClasses = ["All", "Artificer", "Bard", "Cleric", "Druid", "Paladin", "Ranger", "Sorcerer", "Warlock", "Wizard"]

allSchools :: [String]
allSchools = ["All", "Abjuration", "Conjuration", "Divination", "Enchantment", "Evocation", "Illusion", "Necromancy", "Transmutation"]

mkTextFilter :: MisoString -> (MisoString -> Action) -> View Model Action
mkTextFilter caption action =
  H.div_ [ P.class_ "field label prefix border" ]
  [ H.input_ [ P.type_ "text", E.onInput action ]
  , H.label_ [] [ text caption ]
  , H.i_ [ P.class_ "front" ] [ text "search" ]
  ]

mkSelectFilter :: MisoString -> (MisoString -> Action) -> [(String, String)] -> View Model Action
mkSelectFilter caption action items = 
  H.div_ [ P.class_ "field label suffix border" ]
  [ H.select_ [ E.onChange action ] (map (\(v,s) -> H.option_ [ P.value_ (ms v) ] [ text (ms s) ]) items)
  , H.label_ [] [ text caption ]
  , H.i_ [] [ text "arrow_drop_down" ]
  ]

filteredSpells :: Model -> [Spell]
filteredSpells m = filter (\s -> titleFilter m s && levelFilter m s && classFilter m s && schoolFilter m s && sourceFilter m s) (either (const []) id (m ^. spells))

titleFilter :: Model -> Spell -> Bool
titleFilter m s = (toLower $ m ^. filterTitle) `isInfixOf` (toLower $ s ^. title)

levelFilter :: Model -> Spell -> Bool
levelFilter m s = (m ^. filterLevel == -1) || (m ^. filterLevel == s ^. level)

classFilter :: Model -> Spell -> Bool
classFilter m s = (m ^. filterClass == "All") || (m ^. filterClass `elem` s ^. lists)

schoolFilter :: Model -> Spell -> Bool
schoolFilter m s = (m ^. filterSchool == "All") || (m ^. filterSchool == s ^. school)

sourceFilter :: Model -> Spell -> Bool
sourceFilter m s = L.any (\t -> (toLower (m ^. filterSource)) `isInfixOf` (toLower t)) (s ^. source)

spellView :: Spell -> View Model Action
spellView s = 
  H.article_ [ P.class_ "grey3" ]
  [ H.details_ [] 
    [ H.summary_ []
      [ H.div_ [ P.class_ "grid tiny-line" ]
        [ H.div_ [ P.class_ "s3" ] [ H.strong_ [] [ text ( ms $ s ^. title ) ] ]
        , H.div_ [ P.class_ "s2" ] [ text ( ms $ showLevel (s ^. level) ) ]
        , H.div_ [ P.class_ "s3" ] [ text ( intercalate ", " (s ^. lists) ) ]
        , H.div_ [ P.class_ "s2" ] [ text ( s ^. school ) ]
        , H.div_ [ P.class_ "s2" ] [ text ( intercalate ", " (s ^. source) ) ]
        ]
      ]
    , H.article_ [ P.class_ "white" ] 
      [ H.div_ [ P.class_ "grid", C.style_ [ C.gap "0rem" ] ]
        [ H.b_ [ P.class_ "s1" ] [ text "Casting Time:" ]
        , H.p_ [ P.class_ "s11" ] [ text (ms $ s ^. castingTime ) ]
        , H.b_ [ P.class_ "s1" ] [ text "Range:" ]
        , H.p_ [ P.class_ "s11" ] [ text (ms $ s ^. range ) ]
        , H.b_ [ P.class_ "s1" ] [ text "Duration:" ]
        , H.p_ [ P.class_ "s11" ] [ text (ms $ s ^. duration ) ]
        , H.b_ [ P.class_ "s1" ] [ text "Components:" ]
        , H.p_ [ P.class_ "s11" ] [ text (ms $ s ^. components ) ]
        , H.div_ [ P.class_ "s12" ] (map renderStructure (s ^. description))
        ]
      ]
    ]
  ]

page :: a -> Model -> Component a Model Action
page parent model = p { mount = Just GetSpells }
  where
    p = component model updateModel viewModel