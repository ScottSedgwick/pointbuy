module Common.DataTypes.Item where

import Common.DataTypes.Inline   ( Structure )
import Common.Sources            ( Source(..) )
import Common.Utils              ( parseSumValue, parseSumObject, parseString )
import Control.Lens.TH           ( makeLenses )
import Control.Monad             ( mzero )
import Data.Default              ( Default, def )
import qualified Data.Map.Strict as M
import GHC.Generics              ( Generic )
import Miso.JSON                 ( FromJSON, Object, Parser, ToJSON, Value(..), (.:), (.=), object, parseJSON, toJSON, withObject )
import Miso.String               ( MisoString )

data ItemRarity 
  = RarityCommon
  | RarityUncommon
  | RarityRare
  | RarityVeryRare
  | RarityLegendary
  | RarityArtifact
  | RarityUnique
  | RarityUnknown MisoString
  deriving (Show, Eq, Generic)

instance FromJSON ItemRarity where
  parseJSON (String v) = parseSumValue v
    [ ("Common",    RarityCommon)
    , ("Uncommon",  RarityUncommon)
    , ("Rare",      RarityRare)
    , ("Very Rare", RarityVeryRare)
    , ("Legendary", RarityLegendary)
    , ("Artifact",  RarityArtifact)
    , ("Unique",    RarityUnique)
    ]
  parseJSON (Object v) = parseSumObject v [("Unknown", parseString RarityUnknown)]
  parseJSON _ = mzero

instance ToJSON ItemRarity where
  toJSON RarityCommon      = String "Common"
  toJSON RarityUncommon    = String "Uncommon"
  toJSON RarityRare        = String "Rare"
  toJSON RarityVeryRare    = String "Very Rare"
  toJSON RarityLegendary   = String "Legendary"
  toJSON RarityArtifact    = String "Artifact"
  toJSON RarityUnique      = String "Unique"
  toJSON (RarityUnknown s) = object [ "Unknown" .= String s ] 

data ItemType
  = TypeWeapon (Maybe MisoString)
  | TypeItem (Maybe MisoString)
  | TypeArmor (Maybe MisoString)
  | TypePotion
  | TypeRing
  | TypeRod
  | TypeScroll
  | TypeStaff
  | TypeWand
  deriving (Show, Eq, Generic)

instance FromJSON ItemType where
  parseJSON (String s) = parseSumValue  s 
    [ ("Weapon", TypeWeapon Nothing)
    , ("Item", TypeItem Nothing)
    , ("Armor", TypeArmor Nothing)
    , ("Potion", TypePotion)
    , ("Ring", TypeRing)
    , ("Rod", TypeRod)
    , ("Scroll", TypeScroll)
    , ("Staff", TypeStaff)
    , ("Wand", TypeWand)
    ]
  parseJSON (Object m) = parseSumObject m 
    [ ("Weapon", parseString (\s -> TypeWeapon (Just s)))
    , ("Item", parseString (\s -> TypeItem (Just s)))
    , ("Armor", parseString (\s -> TypeArmor (Just s)))
    ]
  parseJSON _ = mzero

instance ToJSON ItemType where
  toJSON (TypeWeapon (Just s))  = object [("Weapon", String s)]
  toJSON (TypeWeapon Nothing )  = String "Weapon"
  toJSON (TypeItem   (Just s))  = object [("Item", String s)]
  toJSON (TypeItem   Nothing )  = String "Item"
  toJSON (TypeArmor   (Just s)) = object [("Armor", String s)]
  toJSON (TypeArmor   Nothing ) = String "Armor"
  toJSON TypePotion             = String "Potion"
  toJSON TypeRing               = String "Ring"
  toJSON TypeRod                = String "Rod"
  toJSON TypeScroll             = String "Scroll"
  toJSON TypeStaff              = String "Staff"
  toJSON TypeWand               = String "Wand"

data ItemAttunement
  = Attune (Maybe MisoString)
  | AttuneNone
  deriving (Show, Eq, Generic)

instance Default ItemAttunement where
  def = AttuneNone

instance FromJSON ItemAttunement where  
  parseJSON (String s) = parseSumValue  s [ ("Attune", Attune Nothing), ("AttuneNone", AttuneNone) ]
  parseJSON (Object m) = parseSumObject m [ ("Attune", parseString (\s -> Attune (Just s)))]
  parseJSON _ = mzero

instance ToJSON ItemAttunement where
  toJSON (Attune (Just s)) = object [("Attune", String s)]
  toJSON (Attune Nothing ) = String "Attune"
  toJSON AttuneNone        = String "AttuneNone"

data Item = Item
  { _itemTitle :: MisoString
  , _itemUrl :: MisoString
  , _itemRarity :: [ ItemRarity ]
  , _itemType :: ItemType
  , _itemAttunement :: ItemAttunement
  , _itemSource :: [ Source ]
  , _itemDescription :: [ Structure ]
  } deriving (Show, Eq, Generic)

instance Default Item where
  def = Item
    { _itemTitle = ""
    , _itemUrl = ""
    , _itemRarity = []
    , _itemType = TypeItem Nothing
    , _itemAttunement = def
    , _itemSource = []
    , _itemDescription = []
    }

instance FromJSON Item where  
  parseJSON = withObject "Item" $ \v -> Item
    <$> v .: "title"
    <*> v .: "url"
    <*> v .: "rarity"
    <*> v .: "type"
    <*> v .: "attune"
    <*> v .: "source"
    <*> v .: "description"

instance ToJSON Item where
  toJSON x = object
    [ "title" .= _itemTitle x
    , "url" .= _itemUrl x
    , "rarity" .= _itemRarity x
    , "type" .= _itemType x
    , "attune" .= _itemAttunement x
    , "source" .= _itemSource x
    , "description" .= _itemDescription x
    ]

makeLenses ''Item
