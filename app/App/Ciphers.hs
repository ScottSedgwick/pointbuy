module App.Ciphers 
  ( Model
  , page
  ) where

import           Control.Lens       ( (&), (.=), (^.) )
import           Control.Lens.TH    ( makeLenses )
import           Data.Default       ( Default, def )
import           Data.List          ( isPrefixOf )
import           GHC.Generics       ( Generic )
import           GHC.Read           ( Read, lexLitChar )
import           Miso               ( Component, Effect, MisoString, Transition, View, (<#), component, fromMisoString, get, io, ms, put, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P
import           Text.ParserCombinators.ReadPrec ( (+++) )

import           Common.Applications ( Appl( Ciphers ) )
import           Common.Classes
import qualified Common.Ciphers.Affine    as CAff
import qualified Common.Ciphers.Atbash    as CAtb
import qualified Common.Ciphers.Caesar    as CCsr
import qualified Common.Ciphers.RailFence as CRf
import qualified Common.Ciphers.Scytale   as CScy
import           Common.Components   ( banner )
import           Common.Sources     ( Source, allSources )
import           Common.Unshow      ( unshow )

-- Model -----------------------------------
data Cipher
  = Atbash
  | Caesar Int
  | Affine Int Int
  | RailFence Int
  | Scytale Int
  deriving (Eq, Generic)
instance Show Cipher where
  show Atbash = "Atbash"
  show (Caesar x) = "Caesar"
  show (Affine x y) = "Affine"
  show (RailFence x) = "RailFence"
  show (Scytale x) = "Scytale"

data Model = Model
  { _message :: MisoString
  , _cipher :: Cipher
  , _cipherText :: MisoString
  , _caesar :: Int
  , _affineA :: Int
  , _affineB :: Int
  , _railfence :: Int
  , _scytale :: Int
  } deriving (Show, Eq, Generic)
instance Default Model where
  def = Model 
        { _message = ""
        , _cipher = Atbash
        , _cipherText = ""
        , _caesar = 1
        , _affineA = 2
        , _affineB = 0
        , _railfence = 3
        , _scytale = 5
        }
makeLenses ''Model

readCipher :: Model -> MisoString -> Cipher
readCipher m s = 
  if isPrefixOf "Caesar" (fromMisoString s) then Caesar (m ^. caesar)
  else if isPrefixOf "Affine" (fromMisoString s) then Affine (m ^. affineA) (m ^. affineB)
  else if isPrefixOf "RailFence" (fromMisoString s) then RailFence (m ^. railfence)
  else if isPrefixOf "Scytale" (fromMisoString s) then Scytale ( m ^. scytale)
  else Atbash

-- Actions -----------------------------------
data Action
  = SetMessage MisoString
  | SetCipher Cipher
  | CalculateCipherText
  | SetCipherText MisoString
  | SetCaesar Int
  | SetAffine Int Int
  | SetRailFence Int
  | SetScytale Int
  | UpdateCiphers
  deriving (Show, Eq)

-- Update -----------------------------------
updateModel :: Action -> Effect a Model Action
updateModel (SetMessage m)      = message .= m     >> exec CalculateCipherText
updateModel (SetCipher c)       = cipher .= c      >> exec CalculateCipherText
updateModel CalculateCipherText =                     exec' (SetCipherText . calculateCipher)
updateModel (SetCipherText x)   = cipherText .= x
updateModel (SetCaesar x)       = caesar .= x      >> exec UpdateCiphers
updateModel (SetAffine x y)     = updateAffine x y >> exec UpdateCiphers
updateModel (SetRailFence x)    = railfence .= x   >> exec UpdateCiphers
updateModel (SetScytale x)      = scytale .= x     >> exec UpdateCiphers
updateModel UpdateCiphers       = updateCiphers    >> exec CalculateCipherText

calculateCipher :: Model -> MisoString
calculateCipher model = 
  let 
    msg = (fromMisoString $ model ^. message) :: String
    encode =
      case (model ^. cipher) of
        Atbash        -> CAtb.encode
        (Caesar x)    -> CCsr.encode (CCsr.mkKey x)
        (Affine x y)  -> CAff.encode (CAff.mkKey x y)
        (RailFence x) -> CRf.encode  (CRf.mkKey x)
        (Scytale x)   -> CScy.encode (CScy.mkKey x)
  in
    ms $ encode msg

exec :: Action -> Effect a Model Action
exec a = get >>= \model -> model <# (pure a)

exec' :: (Model -> Action) -> Effect a Model Action
exec' a = get >>= \model -> model <# (pure (a model))

updateAffine :: Int -> Int -> Effect a Model Action
updateAffine x y = affineA .= x >> affineB .= y

updateCiphers :: Effect a Model Action
updateCiphers = do
  m <- get
  let c = case (m ^. cipher) of
            Atbash      -> Atbash
            Caesar _    -> Caesar (m ^. caesar)
            Affine _ _  -> Affine (m ^. affineA) (m ^. affineB)
            RailFence _ -> RailFence (m ^. railfence)
            Scytale _   -> Scytale (m ^. scytale)
  cipher .= c


-- View -----------------------------------
viewModel :: Model -> View Model Action
viewModel m = 
  H.div_ [] 
  [ banner Ciphers
  , H.p_ [] [ text "Message:" ]
  , H.div_ [ P.class_ "field textarea border" ] 
    [ H.textarea_ [ E.onInput SetMessage ] [] 
    ]
  , viewCipher m
  , H.p_ [] [ text "Cipher Text:" ]
  , H.div_ [ P.class_ "field textarea border" ] [ H.textarea_ [ P.readonly_ True ] [ text (m ^. cipherText)] ]
  ]

viewCipher :: Model -> View Model Action
viewCipher m = 
  H.div_ [] 
  [ H.p_ [] [ text "Cipher:" ]
  , radioItem m Atbash
  , radioItem m (Caesar (m ^. caesar))
  , radioItem m (Affine (m ^. affineA) (m ^. affineB))
  , radioItem m (RailFence (m ^. railfence))
  , radioItem m (Scytale (m ^. scytale))
  ]

radioItem :: Model -> Cipher -> View Model Action
radioItem m c = 
  H.article_ [ P.class_ "grid border" ] 
  [ H.label_ [ P.class_ "radio s3 grid" ]
    [ H.input_ [ P.class_ "s3", P.type_ "radio", P.id_ (ms $ show c), P.name_ "cipher", P.value_ (ms $ show c), P.checked_ ( cipherSelected c (m ^. cipher) ), E.onChange (SetCipher . readCipher m) ]
    , H.span_  [ P.class_ "s9" ] [ text (ms $ show c) ]
    ]
  , H.div_ [ P.class_ "s9 grid field small" ] (cipherDetails m c)
  ]

cipherDetails :: Model -> Cipher -> [View Model Action]
cipherDetails model Atbash        = []
cipherDetails model (Caesar _   ) = 
  [ H.label_ [ P.class_ "s2" ] [ text "Offset:"        ]
  , H.input_ [ P.class_ "s2", P.type_ "number", P.value_ (ms $ show $ model ^. caesar ), E.onChange (\s -> SetCaesar (read (fromMisoString s) :: Int)) ]
  , H.div_ [ P.class_ "s8" ] [] ]
cipherDetails model (Affine _ _ ) = 
  [ H.label_ [ P.class_ "s2" ] [ text "A:"             ]
  , H.input_ [ P.class_ "s2", P.type_ "number", P.value_ (ms $ show $ model ^. affineA ), E.onChange (\s -> SetAffine (read (fromMisoString s) :: Int) (model ^. affineB)) ]
  , H.div_ [ P.class_ "s2" ] []
  , H.label_ [ P.class_ "s2" ] [ text "B:" ]
  , H.input_ [ P.class_ "s4", P.type_ "number", P.value_ (ms $ show $ model ^. affineB ), E.onChange (\s -> SetAffine(model ^. affineA) (read (fromMisoString s) :: Int)) ] ]
cipherDetails model (RailFence _) = 
  [ H.label_ [ P.class_ "s2" ] [ text "Rails:"         ]
  , H.input_ [ P.class_ "s2", P.type_ "number", P.value_ (ms $ show $ model ^. railfence ), P.min_ "2", E.onChange (\s -> SetRailFence (read (fromMisoString s) :: Int)) ]
  , H.div_ [ P.class_ "s8" ] [] ]
cipherDetails model (Scytale _  ) = 
  [ H.label_ [ P.class_ "s2" ] [ text "Circumference:" ]
  , H.input_ [ P.class_ "s2", P.type_ "number", P.value_ (ms $ show $ model ^. scytale ), P.min_ "2", E.onChange (\s -> SetScytale (read (fromMisoString s) :: Int)) ]
  , H.div_ [ P.class_ "s8" ] [] ]

cipherSelected :: Cipher -> Cipher -> Bool
cipherSelected Atbash Atbash = True
cipherSelected (Caesar _) (Caesar _) = True
cipherSelected (Affine _ _) (Affine _ _) = True
cipherSelected (RailFence _) (RailFence _) = True
cipherSelected (Scytale _) (Scytale _) = True
cipherSelected _ _ = False

-- Component constructor ---------------------------------
page :: Model -> Component a Model Action
page model = component model updateModel viewModel