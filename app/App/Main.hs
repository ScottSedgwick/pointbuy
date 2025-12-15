module App.Main where

import           Control.Lens        ( (&), (.=), (^.) )
import           Control.Lens.TH     ( makeLenses )
import           Data.Bifunctor      ( bimap, second )
import           Data.Default        ( Default, def )
import           GHC.Generics        ( Generic )
import           Miso                ( MisoString, Transition, View, (+>), get, ms, text )
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

updateModel :: Action -> Transition Model Action
updateModel (ChangeAppl x) = selectedAppl .= x

viewModel :: Model -> View Model Action
viewModel model =
  H.div_ [] 
  [ H.nav_ [ P.class_ "m l left" ] (map viewApplOption [minBound .. maxBound])
  , H.main_ [] ( map (viewAppl model) [minBound..maxBound] )
  ]

viewApplOption :: Appl -> View Model Action
viewApplOption a = 
  if isApplEnabled a
  then H.a_ [] [ H.i_ [ E.onClick (ChangeAppl a) ] [ text (applIcon a) ], H.span_ [] [ text ( ms (showPretty a) ) ] ]
  else H.div_ [] []

viewAppl :: Model -> Appl -> View Model Action
viewAppl m Backgrounds = H.div_ [ P.hidden_ (m ^. selectedAppl /= Backgrounds) ] +> AB.page (m ^. background)
viewAppl m Crafting    = H.div_ [ P.hidden_ (m ^. selectedAppl /= Crafting   ) ] +> AC.page (m ^. crafting)
viewAppl m DiceRoller  = H.div_ [ P.hidden_ (m ^. selectedAppl /= DiceRoller ) ] +> AD.page (m ^. dice)
viewAppl m Feats       = H.div_ [ P.hidden_ (m ^. selectedAppl /= Feats      ) ] +> AF.page (m ^. feat)
viewAppl m Insults     = H.div_ [ P.hidden_ (m ^. selectedAppl /= Insults    ) ] +> AI.page (m ^. insult)
viewAppl m Lineages    = H.div_ [ P.hidden_ (m ^. selectedAppl /= Lineages   ) ] +> AL.page (m ^. lineage)
viewAppl m MagicItems  = H.div_ [ P.hidden_ (m ^. selectedAppl /= MagicItems ) ] +> AM.page (m ^. magic)
viewAppl m PointBuy    = H.div_ [ P.hidden_ (m ^. selectedAppl /= PointBuy   ) ] +> AP.page (m ^. pointbuy)
viewAppl m Spells      = H.div_ [ P.hidden_ (m ^. selectedAppl /= Spells     ) ] +> AS.page (m ^. spell)
viewAppl m Ciphers     = H.div_ [ P.hidden_ (m ^. selectedAppl /= Ciphers    ) ] +> AE.page (m ^. ciphers)

isApplEnabled :: Appl -> Bool
isApplEnabled Backgrounds = True
isApplEnabled Crafting    = False
isApplEnabled DiceRoller  = False
isApplEnabled Feats       = False
isApplEnabled Insults     = True
isApplEnabled Lineages    = False
isApplEnabled MagicItems  = False
isApplEnabled PointBuy    = False
isApplEnabled Spells      = False
isApplEnabled Ciphers     = True