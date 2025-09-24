module Types.Races where

import           Data.Default
import qualified Data.Map as M

import Types.Stats

data HalfElfDMGStat
  = HalfElfDMGStrength
  | HalfElfDMGDexterity
  | HalfElfDMGConstitution
  | HalfElfDMGIntelligence
  | HalfElfDMGWisdom
  deriving (Eq, Ord, Show, Read)

data HalfElfDetectionStat
  = HalfElfDetectionStrength
  | HalfElfDetectionDexterity
  | HalfElfDetectionConstitution
  | HalfElfDetectionIntelligence
  | HalfElfDetectionCharisma
  deriving (Eq, Ord, Show, Read)
  
data HumanHandlingStat
  = HumanHandlingStrength
  | HumanHandlingDexterity
  | HumanHandlingConstitution
  | HumanHandlingIntelligence
  | HumanHandlingCharisma
  deriving (Eq, Ord, Show, Read)

data HumanMakingStat
  = HumanMakingStrength
  | HumanMakingDexterity
  | HumanMakingConstitution
  | HumanMakingWisdom
  | HumanMakingCharisma
  deriving (Eq, Ord, Show, Read)

data HumanPassageStat
  = HumanPassageStrength
  | HumanPassageConstitution
  | HumanPassageIntelligence
  | HumanPassageWisdom
  | HumanPassageCharisma
  deriving (Eq, Ord, Show, Read)

data TieflingNormalType
  = TieflingNormalDevilsTongue
  | TieflingNormalHellfire
  | TieflingNormalWinged
  | TieflingNormalAsmodeus
  | TieflingNormalBaalzebul
  | TieflingNormalDispater
  | TieflingNormalFierna
  | TieflingNormalGlasya
  | TieflingNormalLevistus
  | TieflingNormalMammon
  | TieflingNormalMephistopheles
  | TieflingNormalZariel
  deriving (Eq, Ord, Show, Read)

data TieflingFeralType
  = TieflingFeralDevilsTongue
  | TieflingFeralHellfire
  | TieflingFeralWinged
  deriving (Eq, Ord, Show, Read)
  
data Race
  = CustomRace
  | Aarakocra
  | AasimarDMG
  | AasimarProtector
  | AasimarScourge
  | AasimarFallen
  | Bugbear
  | Centaur
  | ChangelingStrength
  | ChangelingDexterity
  | ChangelingConstitution
  | ChangelingIntelligence
  | ChangelingWisdom
  | Dragonborn
  | DwarfHill
  | DwarfMountain
  | DwarfDuergar
  | DwarfWarding
  | ElfHigh
  | ElfWood
  | ElfEladrin
  | ElfEladrinMtof
  | ElfDrow
  | ElfSea
  | ElfShadarKai
  | ElfShadow
  | Firbolg
  | GenasiAir
  | GenasiEarth
  | GenasiFire
  | GenasiWater
  | Githyanki
  | Githzerai
  | GnomeForest
  | GnomeRock
  | GnomeDeep
  | GnomeScribing
  | Goblin
  | Goliath
  | Grung
  | HalfElfDMG HalfElfDMGStat HalfElfDMGStat
  | HalfElfVariant HalfElfDMGStat HalfElfDMGStat
  | HalfElfDetection HalfElfDetectionStat
  | HalfElfStorm
  | HalfOrcStandard
  | HalfOrcFinding
  | HalflingLightfoot
  | HalflingStout
  | HalflingGhostwise
  | HalflingHealing
  | HalflingHospitality
  | Hobgoblin
  | HumanStandard
  | HumanVariant Stat Stat
  | HumanFinding
  | HumanHandling HumanHandlingStat
  | HumanMaking HumanMakingStat
  | HumanPassage HumanPassageStat
  | HumanSentinel
  | Kalashtar
  | Kenku
  | Kobold
  | Leonin
  | Lizardfolk
  | Locathah
  | Loxodon
  | Minotaur
  | OrcStandard
  | OrcEberron
  | Satyr
  | ShifterBeasthide
  | ShifterLongtooth
  | ShifterSwiftstride
  | ShifterWildhunt
  | SimicStrength
  | SimicDexterity
  | SimicIntelligence
  | SimicWisdom
  | SimicCharisma
  | Tabaxi
  | TieflingNormal TieflingNormalType
  | TieflingFeral TieflingFeralType
  | Tortle
  | Triton
  | Vedalken
  | WarforgedStrength
  | WarforgedDexterity
  | WarforgedIntelligence
  | WarforgedWisdom
  | WarforgedCharisma
  | YuanTiPureblood
  deriving (Eq, Ord, Show, Read)

instance Default Race where
  def = CustomRace

showPretty :: Race -> String
showPretty CustomRace = "Custom"
showPretty Aarakocra = "Aarakocra"
showPretty AasimarDMG = "Aasimar (DMG)"
showPretty AasimarProtector = "Aasimar (Protector)"
showPretty AasimarScourge = "Aasimar (Scourge)"
showPretty AasimarFallen = "Aasimar (Fallen)"
showPretty Bugbear = "Bugbear"
showPretty Centaur = "Centaur"
showPretty ChangelingStrength = "Changeling (Str)"
showPretty ChangelingDexterity = "Changeling (Dex)"
showPretty ChangelingConstitution = "Changeling (Con)"
showPretty ChangelingIntelligence = "Changeling (Int)"
showPretty ChangelingWisdom = "Changeling (Wis)"
showPretty Dragonborn = "Dragonborn"
showPretty DwarfHill = "Dwarf (Hill)"
showPretty DwarfMountain = "Dwarf (Mountain)"
showPretty DwarfDuergar = "Dwarf (Duergar)"
showPretty DwarfWarding = "Dwarf (Warding)"
showPretty ElfHigh = "Elf (High)"
showPretty ElfWood = "Elf (Wood)"
showPretty ElfEladrin = "Elf (Eladrin)"
showPretty ElfEladrinMtof = "Elf (Eladrin - Mtof)"
showPretty ElfDrow = "Elf (Drow)"
showPretty ElfSea = "Elf (Sea)"
showPretty ElfShadarKai = "Elf (Shadar-Kai)"
showPretty ElfShadow = "Elf (Shadow)"
showPretty Firbolg = "Firbolg"
showPretty GenasiAir = "Genasi (Air)"
showPretty GenasiEarth = "Genasi (Earth)"
showPretty GenasiFire = "Genasi (Fire)"
showPretty GenasiWater = "Genasi (Water)"
showPretty Githyanki = "Githyanki"
showPretty Githzerai = "Githzerai"
showPretty GnomeForest = "Gnome (Forest)"
showPretty GnomeRock = "Gnome (Rock)"
showPretty GnomeDeep = "Gnome (Deep)"
showPretty GnomeScribing = "Gnome (Scribing)"
showPretty Goblin = "Goblin"
showPretty Goliath = "Goliath"
showPretty Grung = "Grung"
showPretty (HalfElfDMG _ _) = "Half-Elf (DMG)"
showPretty (HalfElfVariant _ _) = "Half-Elf (Variant)"
showPretty (HalfElfDetection _) = "Half-Elf (Dragonmark of Detection)"
showPretty HalfElfStorm = "Half-Elf (Dragonmark of Storm)"
showPretty HalfOrcStandard = "Half-Orc"
showPretty HalfOrcFinding = "Half-Orc (Dragonmark of Finding)"
showPretty HalflingLightfoot = "Halfling (Lightfoot)"
showPretty HalflingStout = "Halfling (Stout)"
showPretty HalflingGhostwise = "Halfling (Ghostwise)"
showPretty HalflingHealing = "Halfling (Dragonmark of Healing)"
showPretty HalflingHospitality = "HalflingHospitality"
showPretty Hobgoblin = "Hobgoblin"
showPretty HumanStandard = "Human"
showPretty (HumanVariant _ _) = "Human (Variant)"
showPretty HumanFinding = "Human (Dragonmark of Finding)"
showPretty (HumanHandling _) = "Human (Dragonmark of Handling)"
showPretty (HumanMaking _) = "Human (Dragonmark of Making)"
showPretty (HumanPassage _) = "Human (Dragonmark of Passage)"
showPretty HumanSentinel = "Human (Dragonmark of Sentinel)"
showPretty Kalashtar = "Kalashtar"
showPretty Kenku = "Kenku"
showPretty Kobold = "Kobold"
showPretty Leonin = "Leonin"
showPretty Lizardfolk = "Lizardfolk"
showPretty Locathah = "Locathah"
showPretty Loxodon = "Loxodon"
showPretty Minotaur = "Minotaur"
showPretty OrcStandard = "Orc"
showPretty OrcEberron = "Orc (Eberron)"
showPretty Satyr = "Satyr"
showPretty ShifterBeasthide = "Shifter (Beasthide)"
showPretty ShifterLongtooth = "Shifter (Longtooth)"
showPretty ShifterSwiftstride = "Shifter (Swiftstride)"
showPretty ShifterWildhunt = "Shifter (Wildhunt)"
showPretty SimicStrength = "Simic (Str)"
showPretty SimicDexterity = "Simic (Dex)"
showPretty SimicIntelligence = "Simic (Int)"
showPretty SimicWisdom = "Simic (Wis)"
showPretty SimicCharisma = "Simic (Cha)"
showPretty Tabaxi = "Tabaxi"
showPretty (TieflingNormal _) = "Tiefling"
showPretty (TieflingFeral _) = "Tiefling (Feral)"
showPretty Tortle = "Tortle"
showPretty Triton = "Triton"
showPretty Vedalken = "Vedalken"
showPretty WarforgedStrength = "Warforged (Str)"
showPretty WarforgedDexterity = "Warforged (Dex)"
showPretty WarforgedIntelligence = "Warforged (Int)"
showPretty WarforgedWisdom = "Warforged (Wis)"
showPretty WarforgedCharisma = "Warforged (Cha)"
showPretty YuanTiPureblood = "Yuan-Ti Pureblood"

allRaces :: [Race]
allRaces = 
  [ CustomRace
  , Aarakocra
  , AasimarDMG
  , AasimarProtector
  , AasimarScourge
  , AasimarFallen
  , Bugbear
  , Centaur
  , ChangelingStrength
  , ChangelingDexterity
  , ChangelingConstitution
  , ChangelingIntelligence
  , ChangelingWisdom
  , Dragonborn
  , DwarfHill
  , DwarfMountain
  , DwarfDuergar
  , DwarfWarding
  , ElfHigh
  , ElfWood
  , ElfEladrin
  , ElfEladrinMtof
  , ElfDrow
  , ElfSea
  , ElfShadarKai
  , ElfShadow
  , Firbolg
  , GenasiAir
  , GenasiEarth
  , GenasiFire
  , GenasiWater
  , Githyanki
  , Githzerai
  , GnomeForest
  , GnomeRock
  , GnomeDeep
  , GnomeScribing
  , Goblin
  , Goliath
  , Grung
  , HalfElfDMG HalfElfDMGStrength HalfElfDMGStrength
  , HalfElfVariant HalfElfDMGStrength HalfElfDMGStrength
  , HalfElfDetection HalfElfDetectionStrength
  , HalfElfStorm
  , HalfOrcStandard
  , HalfOrcFinding
  , HalflingLightfoot
  , HalflingStout
  , HalflingGhostwise
  , HalflingHealing
  , HalflingHospitality
  , Hobgoblin
  , HumanStandard
  , HumanVariant Strength Strength
  , HumanFinding
  , HumanHandling HumanHandlingStrength
  , HumanMaking HumanMakingStrength
  , HumanPassage HumanPassageStrength
  , HumanSentinel
  , Kalashtar
  , Kenku
  , Kobold
  , Leonin
  , Lizardfolk
  , Locathah
  , Loxodon
  , Minotaur
  , OrcStandard
  , OrcEberron
  , Satyr
  , ShifterBeasthide
  , ShifterLongtooth
  , ShifterSwiftstride
  , ShifterWildhunt
  , SimicStrength
  , SimicDexterity
  , SimicIntelligence
  , SimicWisdom
  , SimicCharisma
  , Tabaxi
  , TieflingNormal TieflingNormalDevilsTongue
  , TieflingFeral TieflingFeralDevilsTongue
  , Tortle
  , Triton
  , Vedalken
  , WarforgedStrength
  , WarforgedDexterity
  , WarforgedIntelligence
  , WarforgedWisdom
  , WarforgedCharisma
  , YuanTiPureblood
  ]

defaultRacialBonuses :: Race -> StatBlock
defaultRacialBonuses (CustomRace) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (Aarakocra) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses (AasimarDMG) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (AasimarProtector) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (AasimarScourge) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (AasimarFallen) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses (Bugbear) = StatBlock { _str = 2, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }
  
defaultRacialBonuses (Centaur) = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses (ChangelingStrength) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (ChangelingDexterity) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (ChangelingConstitution) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (ChangelingIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (ChangelingWisdom) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }

defaultRacialBonuses (Dragonborn) = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses (DwarfHill) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses (DwarfMountain) = StatBlock { _str = 2, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (DwarfDuergar) = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (DwarfWarding) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }

defaultRacialBonuses (ElfHigh) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses (ElfWood) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses (ElfEladrin) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses (ElfEladrinMtof) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses (ElfDrow) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses (ElfSea) = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (ElfShadarKai) = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (ElfShadow) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses (Firbolg) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 0 }

defaultRacialBonuses (GenasiAir) = StatBlock { _str = 0, _dex = 1, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (GenasiEarth) = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (GenasiFire) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses (GenasiWater) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
  
defaultRacialBonuses (Githyanki) = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses (Githzerai) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 2, _cha = 0 }

defaultRacialBonuses (GnomeForest) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses (GnomeRock) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses (GnomeDeep) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses (GnomeScribing) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 1 }

defaultRacialBonuses (Goblin) = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (Goliath) = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (Grung) = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (HalfElfDMG HalfElfDMGStrength HalfElfDMGStrength) = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGStrength HalfElfDMGDexterity) = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGStrength HalfElfDMGConstitution) = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGStrength HalfElfDMGIntelligence) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGStrength HalfElfDMGWisdom) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGDexterity HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGDexterity HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGDexterity HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGDexterity HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGDexterity HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGConstitution HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGConstitution HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGConstitution HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGConstitution HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGConstitution HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGIntelligence HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGIntelligence HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGIntelligence HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGIntelligence HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGIntelligence HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGWisdom HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGWisdom HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGWisdom HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGWisdom HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfDMG HalfElfDMGWisdom HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGStrength HalfElfDMGStrength) = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGStrength HalfElfDMGDexterity) = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGStrength HalfElfDMGConstitution) = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGStrength HalfElfDMGIntelligence) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGStrength HalfElfDMGWisdom) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGDexterity HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGDexterity HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGDexterity HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGDexterity HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGDexterity HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGConstitution HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGConstitution HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGConstitution HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGConstitution HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGConstitution HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGIntelligence HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGIntelligence HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGIntelligence HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGIntelligence HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGIntelligence HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGWisdom HalfElfDMGStrength) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGWisdom HalfElfDMGDexterity) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGWisdom HalfElfDMGConstitution) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGWisdom HalfElfDMGIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 2 }
defaultRacialBonuses (HalfElfVariant HalfElfDMGWisdom HalfElfDMGWisdom) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 2 }
defaultRacialBonuses (HalfElfDetection HalfElfDetectionStrength) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses (HalfElfDetection HalfElfDetectionDexterity) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses (HalfElfDetection HalfElfDetectionConstitution) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses (HalfElfDetection HalfElfDetectionIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 2, _cha = 0 }
defaultRacialBonuses (HalfElfDetection HalfElfDetectionCharisma) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 1 }
defaultRacialBonuses (HalfElfStorm) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses (HalfOrcStandard) = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (HalfOrcFinding) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }

defaultRacialBonuses (HalflingLightfoot) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses (HalflingStout) = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (HalflingGhostwise) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses (HalflingHealing) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses (HalflingHospitality) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses (Hobgoblin) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }

defaultRacialBonuses (HumanStandard) = StatBlock { _str = 1, _dex = 1, _con = 1, _int = 1, _wis = 1, _cha = 1 }
defaultRacialBonuses ((HumanVariant Strength Strength)) = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Strength Dexterity)) = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Strength Constitution)) = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Strength Intelligence)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Strength Wisdom)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Strength Charisma)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Dexterity Strength)) = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Dexterity Dexterity)) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Dexterity Constitution)) = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Dexterity Intelligence)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Dexterity Wisdom)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Dexterity Charisma)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Constitution Strength)) = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Constitution Dexterity)) = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Constitution Constitution)) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Constitution Intelligence)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Constitution Wisdom)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Constitution Charisma)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Intelligence Strength)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Intelligence Dexterity)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Intelligence Constitution)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Intelligence Intelligence)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanVariant Intelligence Wisdom)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Intelligence Charisma)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Wisdom Strength)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Wisdom Dexterity)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Wisdom Constitution)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Wisdom Intelligence)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanVariant Wisdom Wisdom)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses ((HumanVariant Wisdom Charisma)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 1 }
defaultRacialBonuses ((HumanVariant Charisma Strength)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Charisma Dexterity)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Charisma Constitution)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Charisma Intelligence)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanVariant Charisma Wisdom)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 1 }
defaultRacialBonuses ((HumanVariant Charisma Charisma)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses (HumanFinding) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses ((HumanHandling HumanHandlingStrength)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses ((HumanHandling HumanHandlingDexterity)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses ((HumanHandling HumanHandlingConstitution)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses ((HumanHandling HumanHandlingIntelligence)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 2, _cha = 0 }
defaultRacialBonuses ((HumanHandling HumanHandlingCharisma)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 1 }
defaultRacialBonuses ((HumanMaking HumanMakingStrength)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanMaking HumanMakingDexterity)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanMaking HumanMakingConstitution)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanMaking HumanMakingWisdom)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanMaking HumanMakingCharisma)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 1 }
defaultRacialBonuses ((HumanPassage HumanPassageStrength)) = StatBlock { _str = 1, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanPassage HumanPassageConstitution)) = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanPassage HumanPassageIntelligence)) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((HumanPassage HumanPassageWisdom)) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ((HumanPassage HumanPassageCharisma)) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses (HumanSentinel) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses (Kalashtar) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 1 }

defaultRacialBonuses (Kenku) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses (Kobold) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (Leonin) = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (Lizardfolk) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses (Locathah) = StatBlock { _str = 2, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (Loxodon) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses (Minotaur) = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (OrcStandard) = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (OrcEberron) = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses (Satyr) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses (ShifterBeasthide) = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (ShifterLongtooth) = StatBlock { _str = 2, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (ShifterSwiftstride) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses (ShifterWildhunt) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 2, _cha = 0 }

defaultRacialBonuses (SimicStrength) = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (SimicDexterity) = StatBlock { _str = 0, _dex = 1, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (SimicIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses (SimicWisdom) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses (SimicCharisma) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses (Tabaxi) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses ((TieflingNormal TieflingNormalDevilsTongue)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalHellfire)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalWinged)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalAsmodeus)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalBaalzebul)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalDispater)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalFierna)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalGlasya)) = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalLevistus)) = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalMammon)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalMephistopheles)) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ((TieflingNormal TieflingNormalZariel)) = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses ((TieflingFeral TieflingFeralDevilsTongue)) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((TieflingFeral TieflingFeralHellfire)) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ((TieflingFeral TieflingFeralWinged)) = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }

defaultRacialBonuses (Tortle) = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses (Triton) = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 1 }
  
defaultRacialBonuses (Vedalken) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 1, _cha = 0 }
  
defaultRacialBonuses (WarforgedStrength) = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (WarforgedDexterity) = StatBlock { _str = 0, _dex = 1, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses (WarforgedIntelligence) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses (WarforgedWisdom) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses (WarforgedCharisma) = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses (YuanTiPureblood) = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
  