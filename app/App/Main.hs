module App.Main where

import           Control.Lens        ( (&), (.=), (^.) )
import           Control.Lens.TH     ( makeLenses )
import           Data.Bifunctor      ( bimap, second )
import           Data.Default        ( Default, def )
import           Data.Maybe          ( mapMaybe )
import           GHC.Generics        ( Generic )
import           Miso                ( Effect, MisoString, View, get, io_, mount_, ms, text )
import qualified Miso.Html           as H
import qualified Miso.Html.Event     as E
import qualified Miso.Html.Property  as P

import qualified App.Background      as AB
import qualified App.Ciphers         as AE
import qualified App.Crafting        as AC
import qualified App.Dice            as AD
import qualified App.Feats           as AF
import qualified App.Insults         as AI
import qualified App.Items           as AM
import qualified App.Lineages        as AL
import qualified App.PointBuy        as AP
import qualified App.Spells          as AS
import           Common.Applications ( Appl(..), applIcon )
import           Common.Classes
import           Common.Components   ( banner )

data Action 
  = ChangeAppl Appl
  deriving stock (Show, Eq, Generic)

data Model = Model
  { _selectedAppl :: Appl
  , _background :: AB.Model
  , _crafting :: AC.Model
  , _dice :: AD.Model
  , _feat :: AF.Model
  , _insult :: AI.Model
  , _lineage :: AL.Model
  , _magic :: AM.Model
  , _pointbuy :: AP.Model
  , _spell :: AS.Model
  , _ciphers :: AE.Model
  } deriving stock (Show, Eq, Generic)
makeLenses ''Model
instance Default Model where
  def = Model 
        { _selectedAppl = Backgrounds
        , _background = def
        , _crafting = def
        , _dice = def
        , _feat = def
        , _insult = def
        , _lineage = def
        , _magic = def
        , _pointbuy = def
        , _spell = def
        , _ciphers = def
        }

updateModel :: Action -> Effect a Model Action
updateModel (ChangeAppl x) = selectedAppl .= x -- >> io_ (putStrLn "Hello?")

viewModel :: Model -> View Model Action
viewModel model =
  H.div_ [ P.class_ "grid" ] 
  [ viewNav model
  , H.main_ [ P.class_ "s11" ] ( map (viewAppl model) [minBound..maxBound] )
  ]

viewNav :: Model -> View Model Action
viewNav m = 
  H.nav_ [ P.class_ "s1 m l left" ] [ H.table_ [] (mapMaybe viewApplOption [minBound .. maxBound]) ]
  

viewApplOption :: Appl -> Maybe (View Model Action)
viewApplOption a = 
  if not (isApplEnabled a)
    then Nothing
    else Just $ H.button_ [ E.onClick (ChangeAppl a), P.class_ "border small-round responsive tiny-margin" ]
      [ H.i_ [] [ text (applIcon a)]
      , H.span_ [] [ text ( ms (showPretty a) ) ]
      ]

viewAppl :: Model -> Appl -> View Model Action
viewAppl m Backgrounds = H.div_ [ P.hidden_ (m ^. selectedAppl /= Backgrounds) ] [ mount_ ( AB.page m (m ^. background) ) ]
viewAppl m Ciphers     = H.div_ [ P.hidden_ (m ^. selectedAppl /= Ciphers    ) ] [ mount_ ( AE.page (m ^. ciphers) ) ]
viewAppl m Crafting    = H.div_ [ P.hidden_ (m ^. selectedAppl /= Crafting   ) ] [ mount_ ( AC.page (m ^. crafting) ) ]
viewAppl m DiceRoller  = H.div_ [ P.hidden_ (m ^. selectedAppl /= DiceRoller ) ] [ mount_ ( AD.page (m ^. dice) ) ]
viewAppl m Feats       = H.div_ [ P.hidden_ (m ^. selectedAppl /= Feats      ) ] [ mount_ ( AF.page (m ^. feat) ) ]
viewAppl m Insults     = H.div_ [ P.hidden_ (m ^. selectedAppl /= Insults    ) ] [ mount_ ( AI.page (m ^. insult) ) ]
viewAppl m Lineages    = H.div_ [ P.hidden_ (m ^. selectedAppl /= Lineages   ) ] [ mount_ ( AL.page (m ^. lineage) ) ]
viewAppl m MagicItems  = H.div_ [ P.hidden_ (m ^. selectedAppl /= MagicItems ) ] [ mount_ ( AM.page (m ^. magic) ) ]
viewAppl m PointBuy    = H.div_ [ P.hidden_ (m ^. selectedAppl /= PointBuy   ) ] [ mount_ ( AP.page (m ^. pointbuy) ) ]
viewAppl m Spells      = H.div_ [ P.hidden_ (m ^. selectedAppl /= Spells     ) ] [ mount_ ( AS.page m (m ^. spell) ) ]

isApplEnabled :: Appl -> Bool
isApplEnabled Backgrounds = True
isApplEnabled Ciphers     = True
isApplEnabled Crafting    = False
isApplEnabled DiceRoller  = False
isApplEnabled Feats       = False
isApplEnabled Insults     = True
isApplEnabled Lineages    = False
isApplEnabled MagicItems  = True
isApplEnabled PointBuy    = False
isApplEnabled Spells      = True