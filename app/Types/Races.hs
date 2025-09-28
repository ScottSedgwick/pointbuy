module Types.Races where

import Data.Default ( Default, def )
import Types.Stats  ( StatBlock(..) )
  
data Race
  = CustomRace
  | Aarakocra
  | AasimarDMG
  | AasimarProtector
  | AasimarScourge
  | AasimarFallen
  | Bugbear
  | Centaur
  | ChangelingStr
  | ChangelingDex
  | ChangelingCon
  | ChangelingInt
  | ChangelingWis
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
  | HalfElfDMGStrDex
  | HalfElfDMGStrCon
  | HalfElfDMGStrInt
  | HalfElfDMGStrWis
  | HalfElfDMGDexCon
  | HalfElfDMGDexInt
  | HalfElfDMGDexWis
  | HalfElfDMGConInt
  | HalfElfDMGConWis
  | HalfElfDMGIntWis
  | HalfElfVariantStrDex
  | HalfElfVariantStrCon
  | HalfElfVariantStrInt
  | HalfElfVariantStrWis
  | HalfElfVariantDexCon
  | HalfElfVariantDexInt
  | HalfElfVariantDexWis
  | HalfElfVariantConInt
  | HalfElfVariantConWis
  | HalfElfVariantIntWis
  | HalfElfDetectionStr
  | HalfElfDetectionDex
  | HalfElfDetectionCon
  | HalfElfDetectionInt
  | HalfElfDetectionCha
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
  | HumanVariantStrDex
  | HumanVariantStrCon
  | HumanVariantStrInt
  | HumanVariantStrWis
  | HumanVariantStrCha
  | HumanVariantDexCon
  | HumanVariantDexInt
  | HumanVariantDexWis
  | HumanVariantDexCha
  | HumanVariantConInt
  | HumanVariantConWis
  | HumanVariantConCha
  | HumanVariantIntWis
  | HumanVariantIntCha
  | HumanVariantWisCha
  | HumanFinding
  | HumanHandlingStr
  | HumanHandlingDex
  | HumanHandlingCon
  | HumanHandlingInt
  | HumanHandlingCha
  | HumanMakingStr
  | HumanMakingDex
  | HumanMakingCon
  | HumanMakingWis
  | HumanMakingCha
  | HumanPassageStr
  | HumanPassageCon
  | HumanPassageInt
  | HumanPassageWis
  | HumanPassageCha
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
  | SimicStr
  | SimicDex
  | SimicInt
  | SimicWis
  | SimicCha
  | Tabaxi
  | TieflingNormalDevilsTongue
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
  | TieflingFeralDevilsTongue
  | TieflingFeralHellfire
  | TieflingFeralWinged
  | Tortle
  | Triton
  | Vedalken
  | WarforgedStr
  | WarforgedDex
  | WarforgedInt
  | WarforgedWis
  | WarforgedCha
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
showPretty ChangelingStr = "Changeling (Str)"
showPretty ChangelingDex = "Changeling (Dex)"
showPretty ChangelingCon = "Changeling (Con)"
showPretty ChangelingInt = "Changeling (Int)"
showPretty ChangelingWis = "Changeling (Wis)"
showPretty Dragonborn = "Dragonborn"
showPretty DwarfHill = "Dwarf (Hill)"
showPretty DwarfMountain = "Dwarf (Mountain)"
showPretty DwarfDuergar = "Dwarf (Duergar)"
showPretty DwarfWarding = "Dwarf (Dragonmark of Warding)"
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
showPretty HalfElfDMGStrDex = "Half-Elf (DMG) - (Str & Dex)"
showPretty HalfElfDMGStrCon = "Half-Elf (DMG) - (Str & Con)"
showPretty HalfElfDMGStrInt = "Half-Elf (DMG) - (Str & Int)"
showPretty HalfElfDMGStrWis = "Half-Elf (DMG) - (Str & Wis)"
showPretty HalfElfDMGDexCon = "Half-Elf (DMG) - (Dex & Con)"
showPretty HalfElfDMGDexInt = "Half-Elf (DMG) - (Dex & Int)"
showPretty HalfElfDMGDexWis = "Half-Elf (DMG) - (Dex & Wis)"
showPretty HalfElfDMGConInt = "Half-Elf (DMG) - (Con & Int)"
showPretty HalfElfDMGConWis = "Half-Elf (DMG) - (Con & Wis)"
showPretty HalfElfDMGIntWis = "Half-Elf (DMG) - (Int & Wis)"
showPretty HalfElfVariantStrDex = "Half-Elf (Variant) - (Str & Dex)"
showPretty HalfElfVariantStrCon = "Half-Elf (Variant) - (Str & Con)"
showPretty HalfElfVariantStrInt = "Half-Elf (Variant) - (Str & Int)"
showPretty HalfElfVariantStrWis = "Half-Elf (Variant) - (Str & Wis)"
showPretty HalfElfVariantDexCon = "Half-Elf (Variant) - (Dex & Con)"
showPretty HalfElfVariantDexInt = "Half-Elf (Variant) - (Dex & Int)"
showPretty HalfElfVariantDexWis = "Half-Elf (Variant) - (Dex & Wis)"
showPretty HalfElfVariantConInt = "Half-Elf (Variant) - (Con & Int)"
showPretty HalfElfVariantConWis = "Half-Elf (Variant) - (Con & Wis)"
showPretty HalfElfVariantIntWis = "Half-Elf (Variant) - (Int & Wis)"
showPretty HalfElfDetectionStr = "Half-Elf (Dragonmark of Detection) - (Str)"
showPretty HalfElfDetectionDex = "Half-Elf (Dragonmark of Detection) - (Dex)"
showPretty HalfElfDetectionCon = "Half-Elf (Dragonmark of Detection) - (Con)"
showPretty HalfElfDetectionInt = "Half-Elf (Dragonmark of Detection) - (Int)"
showPretty HalfElfDetectionCha = "Half-Elf (Dragonmark of Detection) - (Cha)"
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
showPretty HumanVariantStrDex = "Human (Variant - Str & Dex)"
showPretty HumanVariantStrCon = "Human (Variant - Str & Con)"
showPretty HumanVariantStrInt = "Human (Variant - Str & Int)"
showPretty HumanVariantStrWis = "Human (Variant - Str & Wis)"
showPretty HumanVariantStrCha = "Human (Variant - Str & Cha)"
showPretty HumanVariantDexCon = "Human (Variant - Dex & Con)"
showPretty HumanVariantDexInt = "Human (Variant - Dex & Int)"
showPretty HumanVariantDexWis = "Human (Variant - Dex & Wis)"
showPretty HumanVariantDexCha = "Human (Variant - Dex & Cha)"
showPretty HumanVariantConInt = "Human (Variant - Con & Int)"
showPretty HumanVariantConWis = "Human (Variant - Con & Wis)"
showPretty HumanVariantConCha = "Human (Variant - Con & Cha)"
showPretty HumanVariantIntWis = "Human (Variant - Int & Wis)"
showPretty HumanVariantIntCha = "Human (Variant - Int & Cha)"
showPretty HumanVariantWisCha = "Human (Variant - Wis & Cha)"
showPretty HumanFinding = "Human (Dragonmark of Finding)"
showPretty HumanHandlingStr = "Human (Dragonmarok of Handling - Str)"
showPretty HumanHandlingDex = "Human (Dragonmarok of Handling - Dex)"
showPretty HumanHandlingCon = "Human (Dragonmarok of Handling - Con)"
showPretty HumanHandlingInt = "Human (Dragonmarok of Handling - Int)"
showPretty HumanHandlingCha = "Human (Dragonmarok of Handling - Cha)"
showPretty HumanMakingStr = "Human (Dragonmark of Making - Str)"
showPretty HumanMakingDex = "Human (Dragonmark of Making - Dex)"
showPretty HumanMakingCon = "Human (Dragonmark of Making - Con)"
showPretty HumanMakingWis = "Human (Dragonmark of Making - Wis)"
showPretty HumanMakingCha = "Human (Dragonmark of Making - Cha)"
showPretty HumanPassageStr = "Human (Dragonmark of Passage - Str)"
showPretty HumanPassageCon = "Human (Dragonmark of Passage - Con)"
showPretty HumanPassageInt = "Human (Dragonmark of Passage - Int)"
showPretty HumanPassageWis = "Human (Dragonmark of Passage - Wis)"
showPretty HumanPassageCha = "Human (Dragonmark of Passage - Cha)"
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
showPretty SimicStr = "Simic (Str)"
showPretty SimicDex = "Simic (Dex)"
showPretty SimicInt = "Simic (Int)"
showPretty SimicWis = "Simic (Wis)"
showPretty SimicCha = "Simic (Cha)"
showPretty Tabaxi = "Tabaxi"
showPretty TieflingNormalDevilsTongue = "Tiefling (Normal - DevilsTongue)"
showPretty TieflingNormalHellfire = "Tiefling (Normal - Hellfire)"
showPretty TieflingNormalWinged = "Tiefling (Normal - Winged)"
showPretty TieflingNormalAsmodeus = "Tiefling (Normal - Asmodeus)"
showPretty TieflingNormalBaalzebul = "Tiefling (Normal - Baalzebul)"
showPretty TieflingNormalDispater = "Tiefling (Normal - Dispater)"
showPretty TieflingNormalFierna = "Tiefling (Normal - Fierna)"
showPretty TieflingNormalGlasya = "Tiefling (Normal - Glasya)"
showPretty TieflingNormalLevistus = "Tiefling (Normal - Levistus)"
showPretty TieflingNormalMammon = "Tiefling (Normal - Mammon)"
showPretty TieflingNormalMephistopheles = "Tiefling (Normal - Mephistopheles)"
showPretty TieflingNormalZariel = "Tiefling (Normal - Zariel)"
showPretty TieflingFeralDevilsTongue = "Tiefling (Feral - DevilsTongue)"
showPretty TieflingFeralHellfire = "Tiefling (Feral - Hellfire)"
showPretty TieflingFeralWinged = "Tiefling (Feral - Winged)"
showPretty Tortle = "Tortle"
showPretty Triton = "Triton"
showPretty Vedalken = "Vedalken"
showPretty WarforgedStr = "Warforged (Str)"
showPretty WarforgedDex = "Warforged (Dex)"
showPretty WarforgedInt = "Warforged (Int)"
showPretty WarforgedWis = "Warforged (Wis)"
showPretty WarforgedCha = "Warforged (Cha)"
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
  , ChangelingStr
  , ChangelingDex
  , ChangelingCon
  , ChangelingInt
  , ChangelingWis
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
  , HalfElfDMGStrDex
  , HalfElfDMGStrCon
  , HalfElfDMGStrInt
  , HalfElfDMGStrWis
  , HalfElfDMGDexCon
  , HalfElfDMGDexInt
  , HalfElfDMGDexWis
  , HalfElfDMGConInt
  , HalfElfDMGConWis
  , HalfElfDMGIntWis
  , HalfElfVariantStrDex
  , HalfElfVariantStrCon
  , HalfElfVariantStrInt
  , HalfElfVariantStrWis
  , HalfElfVariantDexCon
  , HalfElfVariantDexInt
  , HalfElfVariantDexWis
  , HalfElfVariantConInt
  , HalfElfVariantConWis
  , HalfElfVariantIntWis
  , HalfElfDetectionStr
  , HalfElfDetectionDex
  , HalfElfDetectionCon
  , HalfElfDetectionInt
  , HalfElfDetectionCha
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
  , HumanVariantStrDex
  , HumanVariantStrCon
  , HumanVariantStrInt
  , HumanVariantStrWis
  , HumanVariantStrCha
  , HumanVariantDexCon
  , HumanVariantDexInt
  , HumanVariantDexWis
  , HumanVariantDexCha
  , HumanVariantConInt
  , HumanVariantConWis
  , HumanVariantConCha
  , HumanVariantIntWis
  , HumanVariantIntCha
  , HumanVariantWisCha
  , HumanFinding
  , HumanHandlingStr
  , HumanHandlingDex
  , HumanHandlingCon
  , HumanHandlingInt
  , HumanHandlingCha
  , HumanMakingStr
  , HumanMakingDex
  , HumanMakingCon
  , HumanMakingWis
  , HumanMakingCha
  , HumanPassageStr
  , HumanPassageCon
  , HumanPassageInt
  , HumanPassageWis
  , HumanPassageCha
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
  , SimicStr
  , SimicDex
  , SimicInt
  , SimicWis
  , SimicCha
  , Tabaxi
  , TieflingNormalDevilsTongue
  , TieflingNormalHellfire
  , TieflingNormalWinged
  , TieflingNormalAsmodeus
  , TieflingNormalBaalzebul
  , TieflingNormalDispater
  , TieflingNormalFierna
  , TieflingNormalGlasya
  , TieflingNormalLevistus
  , TieflingNormalMammon
  , TieflingNormalMephistopheles
  , TieflingNormalZariel
  , TieflingFeralDevilsTongue
  , TieflingFeralHellfire
  , TieflingFeralWinged
  , Tortle
  , Triton
  , Vedalken
  , WarforgedStr
  , WarforgedDex
  , WarforgedInt
  , WarforgedWis
  , WarforgedCha
  , YuanTiPureblood
  ]

defaultRacialBonuses :: Race -> StatBlock
defaultRacialBonuses CustomRace = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses Aarakocra = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses AasimarDMG = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses AasimarProtector = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses AasimarScourge = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses AasimarFallen = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses Bugbear = StatBlock { _str = 2, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }
  
defaultRacialBonuses Centaur = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses ChangelingStr = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses ChangelingDex = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses ChangelingCon = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses ChangelingInt = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses ChangelingWis = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }

defaultRacialBonuses Dragonborn = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses DwarfHill = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses DwarfMountain = StatBlock { _str = 2, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses DwarfDuergar = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses DwarfWarding = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }

defaultRacialBonuses ElfHigh = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ElfWood = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses ElfEladrin = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses ElfEladrinMtof = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ElfDrow = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ElfSea = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ElfShadarKai = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ElfShadow = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses Firbolg = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 0 }

defaultRacialBonuses GenasiAir = StatBlock { _str = 0, _dex = 1, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses GenasiEarth = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses GenasiFire = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses GenasiWater = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
  
defaultRacialBonuses Githyanki = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses Githzerai = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 2, _cha = 0 }

defaultRacialBonuses GnomeForest = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses GnomeRock = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses GnomeDeep = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses GnomeScribing = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 1 }

defaultRacialBonuses Goblin = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses Goliath = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses Grung = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses HalfElfDMGStrDex = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfDMGStrCon = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfDMGStrInt = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfDMGStrWis = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfDMGDexCon = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfDMGDexInt = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfDMGDexWis = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfDMGConInt = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfDMGConWis = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfDMGIntWis = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfVariantStrDex = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfVariantStrCon = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfVariantStrInt = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfVariantStrWis = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfVariantDexCon = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfVariantDexInt = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfVariantDexWis = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfVariantConInt = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses HalfElfVariantConWis = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfVariantIntWis = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 2 }
defaultRacialBonuses HalfElfDetectionStr = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses HalfElfDetectionDex = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses HalfElfDetectionCon = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses HalfElfDetectionInt = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 2, _cha = 0 }
defaultRacialBonuses HalfElfDetectionCha = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 1 }
defaultRacialBonuses HalfElfStorm = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses HalfOrcStandard = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses HalfOrcFinding = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }

defaultRacialBonuses HalflingLightfoot = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses HalflingStout = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses HalflingGhostwise = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses HalflingHealing = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses HalflingHospitality = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses Hobgoblin = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }

defaultRacialBonuses HumanStandard = StatBlock { _str = 1, _dex = 1, _con = 1, _int = 1, _wis = 1, _cha = 1 }
defaultRacialBonuses HumanVariantStrDex = StatBlock { _str = 1, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanVariantStrCon = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanVariantStrInt = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanVariantStrWis = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses HumanVariantStrCha = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses HumanVariantDexCon = StatBlock { _str = 0, _dex = 1, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanVariantDexInt = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanVariantDexWis = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses HumanVariantDexCha = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses HumanVariantConInt = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanVariantConWis = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses HumanVariantConCha = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses HumanVariantIntWis = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 1, _cha = 0 }
defaultRacialBonuses HumanVariantIntCha = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 1 }
defaultRacialBonuses HumanVariantWisCha = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 1 }
defaultRacialBonuses HumanFinding = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses HumanHandlingStr = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses HumanHandlingDex = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses HumanHandlingCon = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 2, _cha = 0 }
defaultRacialBonuses HumanHandlingInt = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 2, _cha = 0 }
defaultRacialBonuses HumanHandlingCha = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 1 }
defaultRacialBonuses HumanMakingStr = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanMakingDex = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanMakingCon = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 2, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanMakingWis = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 1, _cha = 0 }
defaultRacialBonuses HumanMakingCha = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 0, _cha = 1 }
defaultRacialBonuses HumanPassageStr = StatBlock { _str = 1, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanPassageCon = StatBlock { _str = 0, _dex = 2, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanPassageInt = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses HumanPassageWis = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses HumanPassageCha = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses HumanSentinel = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses Kalashtar = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 2, _cha = 1 }

defaultRacialBonuses Kenku = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses Kobold = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses Leonin = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses Lizardfolk = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses Locathah = StatBlock { _str = 2, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses Loxodon = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses Minotaur = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses OrcStandard = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses OrcEberron = StatBlock { _str = 2, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 0 }

defaultRacialBonuses Satyr = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses ShifterBeasthide = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ShifterLongtooth = StatBlock { _str = 2, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses ShifterSwiftstride = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }
defaultRacialBonuses ShifterWildhunt = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 2, _cha = 0 }

defaultRacialBonuses SimicStr = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses SimicDex = StatBlock { _str = 0, _dex = 1, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses SimicInt = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses SimicWis = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses SimicCha = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses Tabaxi = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses TieflingNormalDevilsTongue = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalHellfire = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalWinged = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalAsmodeus = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalBaalzebul = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalDispater = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalFierna = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 2 }
defaultRacialBonuses TieflingNormalGlasya = StatBlock { _str = 0, _dex = 1, _con = 0, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalLevistus = StatBlock { _str = 0, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalMammon = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalMephistopheles = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
defaultRacialBonuses TieflingNormalZariel = StatBlock { _str = 1, _dex = 0, _con = 0, _int = 0, _wis = 0, _cha = 2 }

defaultRacialBonuses TieflingFeralDevilsTongue = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses TieflingFeralHellfire = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses TieflingFeralWinged = StatBlock { _str = 0, _dex = 2, _con = 0, _int = 1, _wis = 0, _cha = 0 }

defaultRacialBonuses Tortle = StatBlock { _str = 2, _dex = 0, _con = 0, _int = 0, _wis = 1, _cha = 0 }

defaultRacialBonuses Triton = StatBlock { _str = 1, _dex = 0, _con = 1, _int = 0, _wis = 0, _cha = 1 }
  
defaultRacialBonuses Vedalken = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 2, _wis = 1, _cha = 0 }
  
defaultRacialBonuses WarforgedStr = StatBlock { _str = 1, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses WarforgedDex = StatBlock { _str = 0, _dex = 1, _con = 2, _int = 0, _wis = 0, _cha = 0 }
defaultRacialBonuses WarforgedInt = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 1, _wis = 0, _cha = 0 }
defaultRacialBonuses WarforgedWis = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 1, _cha = 0 }
defaultRacialBonuses WarforgedCha = StatBlock { _str = 0, _dex = 0, _con = 2, _int = 0, _wis = 0, _cha = 1 }

defaultRacialBonuses YuanTiPureblood = StatBlock { _str = 0, _dex = 0, _con = 0, _int = 1, _wis = 0, _cha = 2 }
  

racialData :: Race -> [(String, [String])]
racialData CustomRace = []
racialData Aarakocra =
  [ ("Ability Score Increase", ["Your Dexterity score increases by 2, and your Wisdom score increases by 1." ])
  , ("Age", ["Aarakocra reach maturity by age 3. Compared to humans, aarakocra don't usually live longer than 30 years." ])
  , ("Alignment", ["Most aarakocra are good and rarely choose sides when it comes to law and chaos. Tribal leaders and warriors might be lawful, while explorers and adventurers might tend toward chaotic." ])
  , ("Size", ["Aarakocra are about 5 feet tall. They have thin, lightweight bodies that weigh between 80 and 100 pounds. Your size is Medium." ])
  , ("Speed", ["Your base walking speed is 25 feet." ])
  , ("Flight", ["You have a flying speed of 50 feet. To use this speed, you can't be wearing medium or heavy armor." ])
  , ("Talons", ["You are proficient with your unarmed strikes, which deal 1d4 slashing damage on a hit." ])
  , ("Languages", ["You can speak, read, and write Common, Aarakocra, and Auran." ])
  ]
    
racialData AasimarDMG =
  [ ("Ability Score Increase", ["Your Wisdom score increases by 1, and your Charisma score increases by 2." ])
  , ("Age", ["Aasimar mature at the same rate as humans but live a few years longer." ])
  , ("Alignment", ["Due to their celestial heritage, aasimar are often good. However, some aasimar fall into evil, rejecting their heritage." ])
  , ("Size", ["Aasimar have the same range of height and weight as humans." ])
  , ("Speed", ["Your base walking speed is 30 feet." ])
  , ("Darkvision", ["Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ])
  , ("Celestial Resistance", ["You have resistance to necrotic damage and radiant damage." ])
  , ("Languages", ["You can speak, read, and write Common, and Celestial." ])
  , ("Celestial Legacy", 
    [ "You know the Light cantrip."
    , "Once you reach 3rd level, you can cast the Lesser Restoration spell once with this trait, and you regain the ability to do so when you finish a long rest." 
    , "Once you reach 5th level, you can cast the Daylight spell once with this spell as a 3rd level spell, and you regain the ability to do so when you finish a long rest." 
    , "Charisma is your spellcasting ability for these spells." 
    ])
  ]
racialData AasimarProtector =
  [ ("Ability Score Increase", ["Your Charisma score increases by 2, and your Wisdom score increases by 1." ])
  , ("Age", ["Aasimar mature at the same rate as humans, but can live up to 160 years." ])
  , ("Alignment", ["Imbued with celestial power, most aasimar are good. Outcast aasimar are most often neutral or even evil." ])
  , ("Size", ["Aasimar have the same range of height and weight as humans." ])
  , ("Speed", ["Your base walking speed is 30 feet." ])
  , ("Darkvision", ["Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ])
  , ("Celestial Resistance", ["You have resistance to necrotic damage and radiant damage." ])
  , ("Healing Hands", ["As an action, you can touch a creature and cause it to regain a number of hit points equal to your level. Once you use this trait, you can't use it again until you finish a long rest." ])
  , ("Light Bearer", ["You know the Light cantrip. Charisma is your spellcasting ability for it." ])
  , ("Languages", ["You can speak, read, and write Common, and Celestial." ])
  , ("Radiant Soul", 
    [ "Starting at 3rd level, you can use your action to unleash the divine energy within yourself, causing your eyes to glimmer and two luminous, incorporeal wings to sprout from your back. "
    , "Your transformation lasts for 1 minute or until you end it as a bonus action. During it, you have a flying speed of 30 feet, and once on each of your turns, you can deal extra radiant damage to one target when you deal damage to it with an attack or a spell. The extra radiant damage equals your level."
    , "Once you use this trait, you can't use it again until you finish a long rest."
    ])
  ]
racialData AasimarScourge =
  [ ("Ability Score Increase", [ "Your Charisma score increases by 2, and your Constitution score increases by 1." ])
  , ("Age", [ "Aasimar mature at the same rate as humans, but can live up to 160 years." ])
  , ("Alignment", [ "Imbued with celestial power, most aasimar are good. Outcast aasimar are most often neutral or even evil." ])
  , ("Size", [ "Aasimar have the same range of height and weight as humans." ])
  , ("Speed", [ "Your base walking speed is 30 feet." ])
  , ("Darkvision", [ "Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ])
  , ("Celestial Resistance", [ "You have resistance to necrotic damage and radiant damage." ])
  , ("Healing Hands", [ "As an action, you can touch a creature and cause it to regain a number of hit points equal to your level. Once you use this trait, you can't use it again until you finish a long rest." ])
  , ("Light Bearer", [ "You know the Light cantrip. Charisma is your spellcasting ability for it." ])
  , ("Languages", [ "You can speak, read, and write Common, and Celestial." ])
  , ("Radiant Consumption", 
    [ "Starting at 3rd level, you can use your action to unleash the divine energy within yourself, causing a searing light to radiate from you, pour out of your eyes and mouth, and threaten to char you."
    , "Your transformation lasts for 1 minute or until you end it as a bonus action. During it, you shed bright light in a 10-foot radius and dim light for an additional 10 feet, and at the end of each of your turns, you and each creature within 10 feet of you take radiant damage equal to half your level (rounded up). In addition, once on each of your turns, you can deal extra radiant damage to one target when you deal damage to it with an attack or a spell. The extra radiant damage equals your level."
    , "Once you use this trait, you can't use it again until you finish a long rest."
    ])
  ]
racialData AasimarFallen =
  [ ( "Ability Score Increase", [ "Your Charisma score increases by 2, and your Strength score increases by 1." ])
  , ( "Age", [ "Aasimar mature at the same rate as humans, but can live up to 160 years." ])
  , ( "Alignment", [ "Imbued with celestial power, most aasimar are good. Outcast aasimar are most often neutral or even evil." ])
  , ( "Size", [ "Aasimar have the same range of height and weight as humans." ])
  , ( "Speed", [ "Your base walking speed is 30 feet." ])
  , ( "Darkvision", [ "Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ])
  , ( "Celestial Resistance", [ "You have resistance to necrotic damage and radiant damage." ])
  , ( "Healing Hands", [ "As an action, you can touch a creature and cause it to regain a number of hit points equal to your level. Once you use this trait, you can't use it again until you finish a long rest." ])
  , ( "Light Bearer", [ "You know the Light cantrip. Charisma is your spellcasting ability for it." ])
  , ( "Languages", [ "You can speak, read, and write Common, and Celestial." ])
  , ( "Radiant Consumption", 
    [ "Starting at 3rd level, you can use your action to unleash the divine energy within yourself, causing your eyes to turn into pools of darkness and two skeletal, ghostly, flightless wings to sprout from your back. The instant you transform, other creatures within 10 feet of you that you can see you must each succeed on a Charisma saving throw (DC 8 + your proficiency bonus + your Charisma modifier) or become frightened of you until the end of your next turn."
    , "Your transformation lasts for 1 minute or until you end it as a bonus action. During it, once on each of your turns, you can deal extra necrotic damage to one target when you deal damage to it with an attack or a spell. The extra necrotic damage equals your level."
    , "Once you use this trait, you can't use it again until you finish a long rest."
    ])
  ]

racialData Bugbear =
  [ ("Ability Score Increase", [ "Your Strength score increases by 2, and your Dexterity score increases by 1." ])
  , ("Age", [ "Bugbears reach adulthood at age 16 and live up to 80 years." ])
  , ("Alignment", [ "Bugbears endure a harsh existence that demands each of them to remain self-sufficient, even at the expense of their fellows. They tend to be chaotic evil." ])
  , ("Size", [ "Bugbears are between 6 and 8 feet tall and weigh between 250 and 350 pounds. Your size is Medium." ])
  , ("Speed", [ "Your base walking speed is 30 feet." ])
  , ("Darkvision", [ "You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ])
  , ("Long-Limbed", [ "When you make a melee attack on your turn, your reach for it is 5 feet greater than normal." ])
  , ("Powerful Build", [ "You count as one size larger when determining your carrying capacity and the weight you can push, drag, or lift." ])
  , ("Sneaky", [ "You are proficient in the Stealth skill." ])
  , ("Surprise Attack", [ "If you surprise a creature and hit it with an attack on your first turn in combat, the attack deals an extra 2d6 damage to it. You can use this trait only once per combat." ])
  , ("Languages", [ "You can speak, read, and write Common and Goblin." ])
  ]
  
racialData Centaur =
  [ ( "Ability Score Increase", [ "Your Strength score increases by 2, and your Wisdom score increases by 1." ])
  , ( "Age", [ "Centaurs mature and age at about the same rate as humans." ])
  , ( "Alignment", [ "Centaurs are inclined toward neutrality. Those who join the Selesnya are more often neutral good, while those who join the Gruul are typically chaotic neutral." ])
  , ( "Size", [ "Centaurs stand between 6 and 7 feet tall, with their equine bodies reaching about 4 feet at the withers. Your size is Medium." ])
  , ( "Speed", [ "Your base walking speed is 40 feet." ])
  , ( "Fey", [ "Your creature type is fey, rather than humanoid." ])
  , ( "Charge", [ "If you move at least 30 feet straight toward a target and then hit it with a melee weapon attack on the same turn, you can immediately follow that attack with a bonus action, making one attack against the target with your hooves." ])
  , ( "Hooves", [ "Your hooves are natural melee weapons, which you can use to make unarmed strikes. If you hit with them, you deal bludgeoning damage equal to 1d4 + your Strength modifier, instead of the bludgeoning damage normal for an unarmed strike." ])
  , ( "Equine Build", 
    [ "You count as one size larger when determining your carrying capacity and the weight you can push or drag."
    , "In addition, any climb that requires hands and feet is especially difficult for you because of your equine legs. When you make such a climb, each foot of movement costs you 4 extra feet, instead of the normal 1 extra foot."
    ])
  , ( "Survivor", [ "You have proficiency in one of the following skills of your choice: Animal Handling, Medicine, Nature, or Survival." ])
  , ( "Languages", [ "You can speak, read, and write Common and Sylvan. Sylvan is widely spoken in the Selesnya Conclave, for it is rich in vocabulary to describe natural phenomena and spiritual forces.]" ])
  ]

racialData ChangelingStr =
  [ ( "Ability Score Increase", [ "Your Charisma score increases by 2. In addition, one other ability score of your choice increases by 1. (Errata: No longer allows +3 Charisma)" ] )
  , ( "Age", [ "Changelings mature slightly faster than humans but share a similar lifespan - typically a century or less. While a changeling can transform to conceal their age, the effects of aging affect them similarly to humans." ] )
  , ( "Alignment", [ "Changelings tend toward pragramatic neutrality, and few changelings embrace evil." ] )
  , ( "Size", 
    [ "Your size is Medium. To set your height and weight randomly, start with rolling a size modifier."
    , "Size modifier = 2d4."
    , "Height = 5 feet + 1 inch + your size modifier in inches."
    , "Weight in pounds = 115 + (2d4 x your size modifier)"
    ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Shapechanger", 
    [ "As an action, you can change your appearance and your voice. You determine the specifics of the changes, including your coloration, hair length, and sex. You can also adjust your height and weight, but not so much that your size changes. You can make yourself appear as a member of another race, though none of your game statistics change. You can't duplicate the appearance of a creature you've never seen, and you must adopt a form that has the same basic arrangement of limbs that you have. Your clothing and equipment aren't changed by this trait."
    , "You stay in this new form until you use an action to revert to your true form or until you die."
    ] )
  , ( "Changeling Instincts", [ "You gain proficiency with two of the following skills of your choice: Deception, Insight, Intimidation, and Persuasion." ] )
  , ( "Languages", [ "You can speak, read, and write Common and two other languages of your choice." ] )
  ]
racialData ChangelingDex = racialData ChangelingStr
racialData ChangelingCon = racialData ChangelingStr
racialData ChangelingInt = racialData ChangelingStr
racialData ChangelingWis = racialData ChangelingStr

racialData Dragonborn =
  [ ( "Ability Score Increase", [ "Your Strength score increases by 2, and your Charisma score increases by 1." ] )
  , ( "Age", [ "Young dragonborn grow quickly. They walk hours after hatching, attain the size and development of a 10-year-old human child by the age of 3, and reach adulthood by 15. They live to be around 80." ] )
  , ( "Alignment", [ "Dragonborn tend to extremes, making a conscious choice for one side or the other in the cosmic war between good and evil (represented by Bahamut and Tiamat, respectively). Most dragonborn are good, but those who side with Tiamat can be terrible villains." ] )
  , ( "Size", [ "Dragonborn are taller and heavier than humans, standing well over 6 feet tall and averaging almost 250 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Breath Weapon", [ "You can use your action to exhale destructive energy. Your draconic ancestry determines the size, shape, and damage type of the exhalation. When you use your breath weapon, each creature in the area of the exhalation must make a saving throw, the type of which is determined by your draconic ancestry. The DC for this saving throw equals 8 + your Constitution modifier + your proficiency bonus. A creature takes 2d6 damage on a failed save, and half as much damage on a successful one. The damage increases to 3d6 at 6th level, 4d6 at 11th level, and 5d6 at 16th level. After you use your breath weapon, you can't use it again until you complete a short or long rest." ] )
  , ( "Damage Resistance", [ "You have resistance to the damage type associated with your draconic ancestry." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Draconic. Draconic is thought to be one of the oldest languages and is often used in the study of magic. The language sounds harsh to most other creatures and includes numerous hard consonants and sibilants." ] )
  , ( "Draconic Ancestry", 
    [ "You have draconic ancestry. Choose one type of dragon from the Draconic Ancestry table. Your breath weapon and damage resistance are determined by the dragon type, as shown in the table." 
    , "Black Dragon: Acid Breath, 5 by 30 ft. line (Dex. save)"
    , "Blue Dragon: Lightning Breath, 5 by 30 ft. line (Dex. save)"
    , "Brass Dragon: Fire Breath, 5 by 30 ft. line (Dex. save)"
    , "Bronze Dragon: Lightning Breath, 5 by 30 ft. line (Dex. save)"
    , "Copper Dragon: Acid Breath, 5 by 30 ft. line (Dex. save)"
    , "Gold Dragon: Fire Breath, 15 ft. cone (Dex. save)"
    , "Green Dragon: Poison Breath, 15 ft. cone (Con. save)"
    , "Red Dragon: Fire Breath, 15 ft. cone (Dex. save)"
    , "Silver Dragon: Cold Breath, 15 ft. cone (Con. save)"
    , "White Dragon: Cold Breath, 15 ft. cone (Con. save)"
    ] )
  ]

racialData DwarfHill =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Wisdom score increases by 1." ] )
  , ( "Age", [ "Dwarves mature at the same rate as humans, but they're considered young until they reach the age of 50. On average, they live about 350 years." ] )
  , ( "Alignment", [ "Most dwarves are lawful, believing firmly in the benefits of a well-ordered society. They tend toward good as well, with a strong sense of fair play and a belief that everyone deserves to share in the benefits of a just order." ] )
  , ( "Size", [ "Dwarves stand between 4 and 5 feet tall and average about 150 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet. Your speed is not reduced by wearing heavy armor." ] )
  , ( "Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Dwarven Resilience", [ "You have advantage on saving throws against poison, and you have resistance against poison damage." ] )
  , ( "Dwarven Combat Training", [ "You have proficiency with the battleaxe, handaxe, light hammer, and warhammer." ] )
  , ( "Tool Proficiency", [ "You gain proficiency with the artisan's tools of your choice: smith's tools, brewer's supplies, or mason's tools." ] )
  , ( "Stonecunning", [ "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check, instead of your normal proficiency bonus." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Dwarvish. Dwarvish is full of hard consonants and guttural sounds, and those characteristics spill over into whatever other language a dwarf might speak." ] )
  , ( "Dwarven Toughness", [ "Your hit point maximum increases by 1, and it increases by 1 every time you gain a level." ] )
  ]
racialData DwarfMountain =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Strength score increases by 2." ] )
  , ( "Age", [ "Dwarves mature at the same rate as humans, but they're considered young until they reach the age of 50. On average, they live about 350 years." ] )
  , ( "Alignment", [ "Most dwarves are lawful, believing firmly in the benefits of a well-ordered society. They tend toward good as well, with a strong sense of fair play and a belief that everyone deserves to share in the benefits of a just order." ] )
  , ( "Size", [ "Dwarves stand between 4 and 5 feet tall and average about 150 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet. Your speed is not reduced by wearing heavy armor." ] )
  , ( "Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Dwarven Resilience", [ "You have advantage on saving throws against poison, and you have resistance against poison damage." ] )
  , ( "Dwarven Combat Training", [ "You have proficiency with the battleaxe, handaxe, light hammer, and warhammer." ] )
  , ( "Tool Proficiency", [ "You gain proficiency with the artisan's tools of your choice: smith's tools, brewer's supplies, or mason's tools." ] )
  , ( "Stonecunning", [ "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check, instead of your normal proficiency bonus." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Dwarvish. Dwarvish is full of hard consonants and guttural sounds, and those characteristics spill over into whatever other language a dwarf might speak." ] )
  , ( "Dwarven Armor Training", [ "You have proficiency with light and medium armor." ] )
  ]
racialData DwarfDuergar =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Strength score increases by 1." ] )
  , ( "Age", [ "Dwarves mature at the same rate as humans, but they're considered young until they reach the age of 50. On average, they live about 350 years." ] )
  , ( "Alignment", [ "Most dwarves are lawful, believing firmly in the benefits of a well-ordered society. They tend toward good as well, with a strong sense of fair play and a belief that everyone deserves to share in the benefits of a just order." ] )
  , ( "Size", [ "Dwarves stand between 4 and 5 feet tall and average about 150 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet. Your speed is not reduced by wearing heavy armor." ] )
  , ( "Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 120 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Dwarven Resilience", [ "You have advantage on saving throws against poison, and you have resistance against poison damage." ] )
  , ( "Dwarven Combat Training", [ "You have proficiency with the battleaxe, handaxe, light hammer, and warhammer." ] )
  , ( "Tool Proficiency", [ "You gain proficiency with the artisan's tools of your choice: smith's tools, brewer's supplies, or mason's tools." ] )
  , ( "Stonecunning", [ "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check, instead of your normal proficiency bonus." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Undercommon and Dwarvish. Dwarvish is full of hard consonants and guttural sounds, and those characteristics spill over into whatever other language a dwarf might speak." ] )
  , ( "Duergar Resilience", [ "You have advantage on saving throws against illusions and against being charmed or paralyzed." ] )
  , ( "Duergar Magic", [ "When you reach 3rd level, you can cast the Enlarge/Reduce spell on yourself once with this trait, using only the spell's enlarge option. When you reach 5th level, you can cast the Invisibility spell on yourself once with this trait. You don't need material components for either spell, and you can't cast them while you're in direct sunlight, although sunlight has no effect on them once cast. You regain the ability to cast these spells with this trait when you finish a long rest. Intelligence is your spellcasting ability for these spells." ] )
  , ( "Sunlight Sensitivity", [ "You have disadvantage on attack rolls and on Wisdom (Perception) checks that rely on sight when you, the target of your attack, or whatever you are trying to perceive is in direct sunlight." ] )
  ]
racialData DwarfWarding =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Intelligence score increases by 2." ] )
  , ( "Age", [ "Dwarves mature at the same rate as humans, but they're considered young until they reach the age of 50. On average, they live about 350 years." ] )
  , ( "Alignment", [ "Most dwarves are lawful, believing firmly in the benefits of a well-ordered society. They tend toward good as well, with a strong sense of fair play and a belief that everyone deserves to share in the benefits of a just order." ] )
  , ( "Size", [ "Dwarves stand between 4 and 5 feet tall and average about 150 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet. Your speed is not reduced by wearing heavy armor." ] )
  , ( "Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Dwarven Resilience", [ "You have advantage on saving throws against poison, and you have resistance against poison damage." ] )
  , ( "Dwarven Combat Training", [ "You have proficiency with the battleaxe, handaxe, light hammer, and warhammer." ] )
  , ( "Tool Proficiency", [ "You gain proficiency with the artisan's tools of your choice: smith's tools, brewer's supplies, or mason's tools." ] )
  , ( "Stonecunning", [ "Whenever you make an Intelligence (History) check related to the origin of stonework, you are considered proficient in the History skill and add double your proficiency bonus to the check, instead of your normal proficiency bonus." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Dwarvish. Dwarvish is full of hard consonants and guttural sounds, and those characteristics spill over into whatever other language a dwarf might speak." ] )
  , ( "Warder's Intuition", [ "When you make an Intelligence (Investigation) check or an ability check using thieves' tools, you can roll a d4 and add the number rolled to the ability check." ] )
  , ( "Wards and Seals", [ "You can cast the Alarm and Mage Armor spells with this trait. Starting at 3rd level, you can also cast the Arcane Lock spell with it. Once you cast any of these spells with this trait, you can't cast that spell with it again until you finish a long rest. Intelligence is your spellcasting ability for these spells, and you don't need material components for them when you cast them with this trait." ] )
  , ( "Spells of the Mark", 
      [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Warding Spells table are added to the spell list of your spellcasting class."
      , "1st level: Alarm, Armor of Agathys"
      , "2nd level: Arcane Lock, Knock"
      , "3rd level: Glyph of Warding, Magic Circle"
      , "4th level: Leomund's Secret Chest, Mordenkainen's Faithful Hound"
      , "5th level: Antilife Shell"
      ]
    )
  ]

racialData ElfHigh =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your intelligence score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Elvish, plus one extra language of your choice. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Elf Weapon Training", [ "You have proficiency with the longsword, shortsword, shortbow, and longbow." ] )
  , ( "Cantrip", [ "You know one Cantrip of your choice from the wizard spell list. Intelligence is your spellcasting ability for it." ] )
  ]
racialData ElfWood =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your wisdom score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Elvish, plus one extra language of your choice. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Elf Weapon Training", [ "You have proficiency with the longsword, shortsword, shortbow, and longbow." ] )
  , ( "Fleet of Foot", [ "Your base walking speed increases to 35 feet." ] )
  , ( "Mask of the Wild", [ "You can attempt to hide even when you are only lightly obscured by foliage, heavy rain, falling snow, mist, and other natural phenomena." ] )
  ]
racialData ElfEladrin =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your intelligence score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Elvish, plus one extra language of your choice. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Elf Weapon Training", [ "You have proficiency with the longsword, shortsword, shortbow, and longbow." ] )
  , ( "Fey Step", [ "You can cast the Misty Step spell once using this trait. You regain the ability to do so when you finish a short or long rest." ] )
  ]
racialData ElfEladrinMtof =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your charisma score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Elvish, plus one extra language of your choice. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Changing with the Seasons", [ "Some eladrin remain associated with a particular season for their entire lives, whereas other eladrin transform, adopting characteristics of a new season. When finishing a long rest, any eladrin can change their season." ] )
  , ( "Fey Step", 
      [ "As a bonus action, you can magically teleport up to 30 feet to an unoccupied space you can see. Once you use this trait, you can't do so again until you finish a short or long rest."
      , "When you reach 3rd level, your Fey Step gains an additional effect based on your season; if the effect requires a saving throw, the DC equals 8 + your proficiency bonus + your Charisma modifier:"
      , "Autumn: Immediately after you use your Fey Step, up to two creatures of your choice that you can see within 10 feet of you must succeed on a Wisdom saving throw or be charmed by you for 1 minute, or until you or your companions deal any damage to it."
      , "Winter: When you use your Fey Step, one creature of your choice that you can see within 5 feet of you before you teleport must succeed on a Wisdom saving throw or be frightened of you until the end of your next turn."
      , "Spring: When you use your Fey Step, you can touch one willing creature within 5 feet of you. That creature then teleports instead of you, appearing in an unoccupied space of your choice that you can see within 30 feet of you."
      , "Summer: Immediately after you use your Fey Step, each creature of your choice that you can see within 5 feet of you take fire damage equal to your Charisma modifier (minimum of 1 damage)."
      ] 
    )
  ]
racialData ElfDrow =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your charisma score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Superior Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 120 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Elvish. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Sunlight Sensitivity", [ "You have disadvantage on attack rolls and on Wisdom (Perception) checks that rely on sight when you, the target of your attack, or whatever you are trying to perceive is in direct sunlight." ] )
  , ( "Drow Magic", [ "You know the Dancing Lights cantrip. When you reach 3rd level, you can cast the Faerie Fire spell once with this trait and regain the ability to do so when you finish a long rest. When you reach 5th level, you can also cast the Darkness spell once with this trait and regain the ability to do so when you finish a long rest. Charisma is your spellcasting ability for these spells." ] )
  , ( "Drow Weapon Training", [ "You have proficiency with rapiers, shortswords, and hand crossbows." ])
  ]
racialData ElfSea =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your constitution score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Superior Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 120 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Elvish and Aquan. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Sea Elf Training", [ "You have proficiency with the spear, trident, light crossbow, and net." ])
  , ( "Friend of the Sea", [ "Using gestures and sounds, you can communicate simple ideas with any beast that has an innate swimming speed." ] )
  ]
racialData ElfShadarKai =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your constitution score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Superior Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 120 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Elvish. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Necrotic Resistance", [ "You have resistance to necrotic damage." ])
  , ( "Blessing of the Raven Queen", 
      [ "As a bonus action, you can magically teleport up to 30 feet to an unoccupied space you can see. Once you use this trait, you can't do so until you finish a long rest."
      , "Starting at 3rd level, you also gain resistance to all damage when you teleport using this trait. The resistance lasts until the start of your next turn. During that time, you appear ghostly and translucent."
      ] 
    )
  ]
racialData ElfShadow =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your charisma score increases by 1." ] )
  , ( "Age", [ "Although elves reach physical maturity at about the same age as humans, the elven understanding of adulthood goes beyond physical growth to encompass worldly experience. An elf typically claims adulthood and an adult name around the age of 100 and can live to be 750 years old." ] )
  , ( "Alignment", [ "Elves love freedom, variety, and self-expression, so they lean strongly toward the gentler aspects of chaos. They value and protect others' freedom as well as their own, and they are more often good than not. The drow are an exception; their exile into the Underdark has made them vicious and dangerous. Drow are more often evil than not." ] )
  , ( "Size", [ "Elves range from under 5 to over 6 feet tall and have slender builds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Superior Darkvision", [ "Accustomed to twilit forests and the night sky, you have superior vision in dark and dim conditions. You can see in dim light within 120 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Keen Senses", [ "You have proficiency in the Perception skill." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Trance", [ "Elves don't need to sleep. Instead, they meditate deeply, remaining semiconscious, for 4 hours a day. (The Common word for such meditation is 'trance.') While meditating, you can dream after a fashion; such dreams are actually mental exercises that have become reflexive through years of practice. After resting in this way, you gain the same benefit that a human does from 8 hours of sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Elvish. Elvish is fluid, with subtle intonations and intricate grammar. Elven literature is rich and varied, and their songs and poems are famous among other races. Many bards learn their language so they can add Elvish ballads to their repertoires." ] )
  , ( "Cunning Intuition", [ "When you make an Charisma (Performance) or Dexterity (Stealth) check, you can roll a d4 and add the number rolled to the ability check." ])
  , ( "Shape Shadows", [ "You can cast the Minor Illusion cantrip. Starting at 3rd level, you can cast the Invisibility spell once with this trait, and you regain the ability to cast it when you finish a long rest. Charisma is your spellcasting ability for these spells." ])
  , ( "Spells of the Mark", 
      [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Shadow Spells table are added to the spell list of your spellcasting class."
      , "1st level: Disguise Self, Silent Image"
      , "2nd level: Darkness, Pass Without Trace"
      , "3rd level: Clairvoyance, Major Image"
      , "4th level: Greater Invisibility, Hallucinatory Terrain"
      , "5th level: Mislead"
      ] 
    )
  ]

racialData Firbolg =
  [ ( "Ability Score Increase", [ "Your Wisdom score increases by 2, and your Strength score increases by 1." ] )
  , ( "Age", [ "As humanoids related to the fey, firbolg have long lifespans. A firbolg reaches adulthood around 30, and the oldest of them can live for 500 years." ] )
  , ( "Alignment", [ "As people who follow the rhythm of nature and see themselves as its caretakers, firbolg are typically neutral good. Evil firbolg are rare and are usually the sworn enemies of the rest of their kind." ] )
  , ( "Size", [ "Firbolg are between 7 and 8 feet tall and weigh between 240 and 300 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Firbolg Magic", [ "You can cast Detect Magic and Disguise Self with this trait, using Wisdom as your spellcasting ability for them. Once you cast either spell, you can't cast it again with this trait until you finish a short or long rest. When you use this version of Disguise Self, you can seem up to 3 feet shorter than normal, allowing you to more easily blend in with humans and elves." ] )
  , ( "Hidden Step", [ "As a bonus action, you can magically turn invisible until the start of your next turn or until you attack, make a damage roll, or force someone to make a saving throw. Once you use this trait, you can't use it again until you finish a short or long rest." ] )
  , ( "Powerful Build", [ "You count as one size larger when determining your carrying capacity and the weight you can push, drag, or lift." ] )
  , ( "Speech of Beast and Leaf", [ "You have the ability to communicate in a limited manner with beasts and plants. They can understand the meaning of your words, though you have no special ability to understand them in return. You have advantage on all Charisma checks you make to influence them." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Elvish, and Giant." ] )
  ]

racialData GenasiAir =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Dexterity score increases by 1." ] )
  , ( "Age", [ "Genasi mature at about the same rate as humans and reach adulthood in their late teens. They live somewhat longer than humans do, up to 120 years." ] )
  , ( "Alignment", [ "Independent and self-reliant, genasi tend toward a neutral alignment." ] )
  , ( "Size", [ "Genasi are as varied as their mortal parents but are generally built like humans, standing anywhere from 5 feet to over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Primordial. Primordial is a guttural language, filled with harsh syllables and hard consonants." ] )
  , ( "Unending Breath", [ "You can hold your breath indefinitely while you're not incapacitated." ] )
  , ( "Mingle with the Wind", [ "You can cast the levitate spell once with this trait, requiring no material components, and you regain the ability to cast it this way when you finish a long rest. Constitution is your spellcasting ability for this spell." ] )
  ]
racialData GenasiEarth =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Strength score increases by 1." ] )
  , ( "Age", [ "Genasi mature at about the same rate as humans and reach adulthood in their late teens. They live somewhat longer than humans do, up to 120 years." ] )
  , ( "Alignment", [ "Independent and self-reliant, genasi tend toward a neutral alignment." ] )
  , ( "Size", [ "Genasi are as varied as their mortal parents but are generally built like humans, standing anywhere from 5 feet to over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Primordial. Primordial is a guttural language, filled with harsh syllables and hard consonants." ] )
  , ( "Earth Walk", [ "You can move across difficult terrain made of earth or stone without expending extra movement." ] )
  , ( "Merge with Stone", [ "You can cast the pass without trace spell once with this trait, requiring no material components, and you regain the ability to cast it this way when you finish a long rest. Constitution is your spellcasting ability for this spell." ] )
  ]
racialData GenasiFire =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Intelligence score increases by 1." ] )
  , ( "Age", [ "Genasi mature at about the same rate as humans and reach adulthood in their late teens. They live somewhat longer than humans do, up to 120 years." ] )
  , ( "Alignment", [ "Independent and self-reliant, genasi tend toward a neutral alignment." ] )
  , ( "Size", [ "Genasi are as varied as their mortal parents but are generally built like humans, standing anywhere from 5 feet to over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Primordial. Primordial is a guttural language, filled with harsh syllables and hard consonants." ] )
  , ( "Darkvision", [ "You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. Your ties to the Elemental Plane of Fire make your darkvision unusual: everything you see in darkness is in a shade of red." ] )
  , ( "Fire Resistance", [ "You have resistance to fire damage." ] )
  , ( "Reach to the Blaze", [ "You know the Produce Flame cantrip. Once you reach 3rd level, you can cast the burning hands spell once with this trait as a 1st-level spell, and you regain the ability to cast it this way when you finish a long rest. Constitution is your spellcasting ability for these spells." ] )
  ]
racialData GenasiWater =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Wisdom score increases by 1." ] )
  , ( "Age", [ "Genasi mature at about the same rate as humans and reach adulthood in their late teens. They live somewhat longer than humans do, up to 120 years." ] )
  , ( "Alignment", [ "Independent and self-reliant, genasi tend toward a neutral alignment." ] )
  , ( "Size", [ "Genasi are as varied as their mortal parents but are generally built like humans, standing anywhere from 5 feet to over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Primordial. Primordial is a guttural language, filled with harsh syllables and hard consonants." ] )
  , ( "Acid Resistance", [ "You have resistance to acid damage." ] )
  , ( "Amphibious", [ "You can breathe air and water." ] )
  , ( "Swim", [ "You have a swimming speed of 30 feet." ] )
  , ( "Call to the Wave", [ "You know the shape water cantrip (see chapter 2 EEPC). When you reach 3rd level, you can cast the create or destroy water spell as a 2nd-level spell once with this trait, and you regain the ability to cast it this way when you finish a long rest. Constitution is your spellcasting ability for these spells." ] )
  ]
  
racialData Githyanki =
  [ ( "Ability Score Increase", [ "Your Intelligence score increases by 1, and your Strength score increases by 2." ] )
  , ( "Age", [ "Gith reach adulthood in their late teens and live for about a century." ] )
  , ( "Size", [ "Gith are taller and leaner than humans, with most a slender 6 feet in height." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Gith." ] )
  , ( "Alignment", [ "Githyanki tend toward lawful evil. They are aggressive and arrogant, and they remain the faithful servants of their lich-queen, Vlaakith. Renegade githyanki tend toward chaos." ] )
  , ( "Decadent Mastery", [ "You learn one language of your choice, and you are proficient with one skill or tool of your choice. In the timeless city of Tu'narath, githyanki have bountiful time to master odd bits of knowledge." ] )
  , ( "Martial Prodigy", [ "You are proficient with light and medium armor and with shortswords, longswords, and greatswords." ] )
  , ( "Githyanki Psionics", 
      [ "You know the Mage Hand cantrip and the hand is invisible when you cast the cantrip with this trait."
      , "When you reach 3rd level, you can cast the Jump spell once with this trait, and you regain the ability to do so when you finish a long rest. When you reach 5th level, you can cast the Misty Step spell once with this trait and you regain the ability to do so when you finish a long rest."
      , "Intelligence is your spellcasting ability for these spells. When you cast them with this trait, they don't require components."
      ]
    )
  ]
racialData Githzerai =
  [ ( "Ability Score Increase", [ "Your Intelligence score increases by 1, and your Wisdom score increases by 2." ] )
  , ( "Age", [ "Gith reach adulthood in their late teens and live for about a century." ] )
  , ( "Size", [ "Gith are taller and leaner than humans, with most a slender 6 feet in height." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Gith." ] )
  , ( "Alignment", [ "Githzerai tend toward lawful neutral. Their rigorous training in psychic abilities requires an implacable mental discipline." ] )
  , ( "Mental Discipline", [ "You have advantage on saving throws against the charmed and frightened conditions. Under the tutelage of monastic masters, githzerai learn to govern their own minds." ] )
  , ( "Githzerai Psionics", 
      [ "You know the Mage Hand cantrip and the hand is invisible when you cast the cantrip with this trait."
      , "When you reach 3rd level, you can cast the Shield spell once with this trait, and you regain the ability to do so after you finish a long rest. When you reach 5th level, you can cast the Detect Thoughts spell once with this trait, and you regain the ability to do so when you finish a long rest."
      , "Wisdom is your spellcasting ability for these spells. When you cast them with this trait, they don't require components."
      ]
    )
  ]

racialData GnomeForest =
  [ ( "Ability Score Increase", [ "Your Intelligence score increases by 2, and your Dexterity score increases by 1." ] )
  , ( "Age", [ "Gnomes mature at the same rate humans do, and most are expected to settle down into an adult life by around age 40. They can live 350 to almost 500 years." ] )
  , ( "Alignment", [ "Gnomes are most often good. Those who lend toward law are sages, engineers, researchers, scholars, investigators, or inventors. Those who lend toward chaos are minstrels, tricksters, wanderers, or fanciful jewelers. Gnomes are good-hearted, and even the tricksters among them are more playful than vicious." ] )
  , ( "Size", [ "Gnomes are between 3 and 4 feet tall and average about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Gnome Cunning", [ "You have advantage on all Intelligence, Wisdom, and Charisma saving throws against magic." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Gnomish. The Gnomish language, which uses the Dwarvish script, is renowned for its technical treatises and its catalogs of knowledge about the natural world." ] )
  , ( "Natural Illusionist", [ "You know the Minor Illusion cantrip. Intelligence is your spellcasting ability for it." ] )
  , ( "Speak with Small Beasts", [ "Through sounds and gestures, you can communicate simple ideas with Small or smaller beasts. Forest gnomes love animals and often keep squirrels, badgers, rabbits, moles, woodpeckers, and other creatures as beloved pets." ] )
  ]
racialData GnomeRock =
  [ ( "Ability Score Increase", [ "Your Intelligence score increases by 2, and your Constitution score increases by 1." ] )
  , ( "Age", [ "Gnomes mature at the same rate humans do, and most are expected to settle down into an adult life by around age 40. They can live 350 to almost 500 years." ] )
  , ( "Alignment", [ "Gnomes are most often good. Those who lend toward law are sages, engineers, researchers, scholars, investigators, or inventors. Those who lend toward chaos are minstrels, tricksters, wanderers, or fanciful jewelers. Gnomes are good-hearted, and even the tricksters among them are more playful than vicious." ] )
  , ( "Size", [ "Gnomes are between 3 and 4 feet tall and average about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Gnome Cunning", [ "You have advantage on all Intelligence, Wisdom, and Charisma saving throws against magic." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Gnomish. The Gnomish language, which uses the Dwarvish script, is renowned for its technical treatises and its catalogs of knowledge about the natural world." ] )
  , ( "Artificer's Lore", [ "Whenever you make an Intelligence (History) check related to magic items, alchemical objects, or technological devices, you can add twice your proficiency bonus, instead of any proficiency bonus you normally apply." ] )
  , ( "Tinker", 
    [ "You have proficiency with artisan's tools (tinker's tools). Using those tools, you can spend 1 hour and 10 gp worth of materials to construct a Tiny clockwork device (AC 5, 1 hp). The device ceases to function after 24 hours (unless you spend 1 hour repairing it to keep the device functioning), or when you use your action to dismantle it; at that time, you can reclaim the materials used to create it. You can have up to three such devices active at a time. When you create a device, choose one of the following options:"
    , "Clockwork Toy - This toy is a clockwork animal, monster, or person, such as a frog, mouse, bird, dragon, or soldier. When placed on the ground, the toy moves 5 feet across the ground on each of your turns in a random direction. It makes noises as appropriate to the creature it represents."
    , "Fire Starter - The device produces a miniature flame, which you can use to light a candle, torch, or campfire. Using the device requires your action."
    , "Music Box - When opened, this music box plays a single song at a moderate volume. The box stops playing when it reaches the song's end or when it is closed."
    ] )
  ]
racialData GnomeDeep =
  [ ( "Ability Score Increase", [ "Your Intelligence score increases by 2, and your Dexterity score increases by 1." ] )
  , ( "Age", [ "Deep gnomes are short-lived for gnomes. They mature at the same rate humans do and are considered full-grown adults by 25. They live 200 to 250 years, although hard toil and the dangers of the Underdark often claim them before their time." ] )
  , ( "Alignment", [ "Svirfneblin believe that survival depends on avoiding entanglements with other creatures and not making enemies, so they favor neutral alignments. They rarely wish others ill, and they are unlikely to take risks on behalf of others." ] )
  , ( "Size", [ "A typical svirfneblin stands about 3 to 3½ feet tall and weighs 80 to 120 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Superior Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 120 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Gnome Cunning", [ "You have advantage on all Intelligence, Wisdom, and Charisma saving throws against magic." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Undercommon, and Gnomish. The Gnomish language, which uses the Dwarvish script, is renowned for its technical treatises and its catalogs of knowledge about the natural world." ] )
  , ( "Stone Camouflage", [ "You have advantage on Dexterity (Stealth) checks to hide in rocky terrain." ] )
  , ( "Optional Feat: Svirfneblin Magic", [ "You have inherited the innate spellcasting ability of your ancestors. This ability allows you to cast Nondetection on yourself at will, without needing a material component. You can also cast each of the following spells once with this ability: Blindness/Deafness, Blur, and Disguise Self. You regain the ability to cast these spells when you finish a long rest." ] )
  ]
racialData GnomeScribing =
  [ ( "Ability Score Increase", [ "Your Intelligence score increases by 2, and your Charisma score increases by 1." ] )
  , ( "Age", [ "Gnomes mature at the same rate humans do, and most are expected to settle down into an adult life by around age 40. They can live 350 to almost 500 years." ] )
  , ( "Alignment", [ "Gnomes are most often good. Those who lend toward law are sages, engineers, researchers, scholars, investigators, or inventors. Those who lend toward chaos are minstrels, tricksters, wanderers, or fanciful jewelers. Gnomes are good-hearted, and even the tricksters among them are more playful than vicious." ] )
  , ( "Size", [ "Gnomes are between 3 and 4 feet tall and average about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Darkvision", [ "Accustomed to life underground, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Gnome Cunning", [ "You have advantage on all Intelligence, Wisdom, and Charisma saving throws against magic." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Gnomish. The Gnomish language, which uses the Dwarvish script, is renowned for its technical treatises and its catalogs of knowledge about the natural world." ] )
  , ( "Gifted Scribe", [ "When you make an Intelligence (History) check or an ability check using calligrapher's supplies, you can roll a d4 and add the number rolled to the ability check." ] )
  , ( "Scribe's Insight", [ "You can cast the Message cantrip. You can also cast the Comprehend Languages once with this trait, and you regain the ability to cast it when you finish a short or long rest. Starting at 3rd level, you can cast the Magic Mouth spell with this trait, and you regain the ability to cast it when you finish a long rest. Intelligence is your spellcasting ability for these spells." ] )
  , ( "Spells of the Mark", 
    [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Scribing Spells table are added to the spell list of your spellcasting class."
    , "1st level: Comprehend Languages, Illusory Script"
    , "2nd level: Animal Messenger, Silence"
    , "3rd level: Sending, Tongues"
    , "4th level: Arcane Eye, Confusion"
    , "5th level: Dream"
    ] )
  ]

racialData Goblin =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2, and your Constitution score increases by 1." ] )
  , ( "Age", [ "Goblins reach adulthood at age 8 and live up to 60 years." ] )
  , ( "Alignment", [ "Goblins are typically neutral evil, as they care only for their own needs. A few goblins might tend toward good or neutrality, but only rarely." ] )
  , ( "Size", [ "Goblins are between 3 and 4 feet tall and weigh between 40 and 80 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Fury of the Small", [ "When you damage a creature with an attack or a spell and the creature's size is larger than yours, you can cause the attack or spell to deal extra damage to the creature. The extra damage equals your level. Once you use this trait, you can't use it again until you finish a short or long rest." ] )
  , ( "Nimble Escape", [ "You can take the Disengage or Hide action as a bonus action of each of your turns." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Goblin. In Ravnica, Goblin is a simplistic language with a limited vocabulary and fluid rules of grammar, unsuited to any sophisticated conversation." ] )
  ]

racialData Goliath =
  [ ( "Ability Score Increase", [ "Your Strength score increases by 2, and your Constitution score increases by 1." ] )
  , ( "Age", [ "Goliaths have lifespans comparable to humans. They enter adulthood in their late teens and usually live less than a century." ] )
  , ( "Alignment", [ "Goliath society, with its clear roles and tasks, has a strong lawful bent. The goliath sense of fairness, balanced with an emphasis on self-sufficiency and personal accountability, pushes them toward neutrality." ] )
  , ( "Size", [ "Goliaths are between 7 and 8 feet tall and weigh between 280 and 340 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Natural Athlete", [ "You have proficiency in the Athletics skill." ] )
  , ( "Stone's Endurance", [ "You can focus yourself to occasionally shrug off injury. When you take damage, you can use your reaction to roll a d12. Add your Constitution modifier to the number rolled, and reduce the damage by that total. After you use this trait, you can't use it again until you finish a short or long rest." ] )
  , ( "Powerful Build", [ "You count as one size larger when determining your carrying capacity and the weight you can push, drag, or lift." ] )
  , ( "Mountain Born", [ "You have resistance to cold damage. You're acclimated to high altitude, including elevations above 20,000 feet. You're also naturally adapted to cold climates, as described in chapter 5 of the Dungeon Master's Guide. (Errata: Added cold resistance)" ] )
  , ( "Languages", [ "You can speak, read, and write Common and Giant." ] )
  ]

racialData Grung = 
  [ ( "Ability Score Increase.", [ "Your Dexterity score increases by 2 and your Constitution score increases by 1." ] )
  , ( "Age.", [ "Grungs mature to adulthood in a single year, but have been known to live up to 50 years." ] )
  , ( "Alignment.", [ "Most grungs are lawful, having been raised in a strict caste system. They tend toward evil as well, coming from a culture where social advancement occurs rarely, and most often because another member of your army has died and there is no one else of that caste to fill the vacancy." ] )
  , ( "Arboreal Alertness.", [ "You have proficiency in the Perception skill." ] )
  , ( "Size.", [ "Grungs stand between 2 ½ and 3 ½ feet tall and average about 30 pounds. Your size is Small." ] )
  , ( "Speed.", [ "You have a walking speed of 25 feet. Your sticky finger and toe pads give you a climb speed of 25 feet." ] )
  , ( "Amphibious.", [ "You can breathe air and water." ] )
  , ( "Poison Immunity.", [ "You are immune to poison damage and the poisoned condition." ] )
  , ( "Poisonous Skin.",
      [ "Any creature that grapples you or otherwise comes into direct contact with your skin must succeed on a DC 12 Constitution saving throw or become poisoned for 1 minute. A poisoned creature no longer in direct contact with you can repeat the saving throw at the end of each of its turns, ending the effect on itself on a success."
      , "You can also apply this poison to any piercing weapon as part of an attack with that weapon, though when you hit the poison reacts differently. The target must succeed on a DC 12 Constitution saving throw or take 2d4 poison damage."
      ]
    )
  , ( "Standing Leap.", [ "Your long jump is up to 25 feet and your high jump is up to 15 feet, with or without a running start." ] )
  , ( "Water Dependency.", [ "If you fail to immerse yourself in water for at least 1 hour during a day, you suffer 1 level of exhaustion at the end of that day. You can recover from this exhaustion only through magic or by immersing yourself in water for at least 1 hour." ] )
  , ( "Languages.", [ "You can speak, read, and write Grung." ] )
  ]

racialData HalfElfDMGStrDex = 
  [ ( "Ability Score Increase", [ "Your Charisma score increases by 2, and two other ability scores of your choice increase by 1." ] )
  , ( "Age", [ "Half-elves mature at the same rate humans do and reach adulthood around the age of 20. They live much longer than humans, however, often exceeding 180 years." ] )
  , ( "Alignment", [ "Half-elves share the chaotic bent of their elven heritage. They value both personal freedom and creative expression, demonstrating neither love of leaders nor desire for followers. They chafe at rules, resent others' demands, and sometimes prove unreliable, or at least unpredictable." ] )
  , ( "Size", [ "HaIf-elves are about the same size as humans, ranging from 5 to 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Thanks to your elf blood, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Skill Versatility", [ "You gain proficiency in two skills of your choice." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Elvish, and one extra language of your choice." ] )
  ]
racialData HalfElfDMGStrCon = racialData HalfElfDMGStrDex
racialData HalfElfDMGStrInt = racialData HalfElfDMGStrDex
racialData HalfElfDMGStrWis = racialData HalfElfDMGStrDex
racialData HalfElfDMGDexCon = racialData HalfElfDMGStrDex
racialData HalfElfDMGDexInt = racialData HalfElfDMGStrDex
racialData HalfElfDMGDexWis = racialData HalfElfDMGStrDex
racialData HalfElfDMGConInt = racialData HalfElfDMGStrDex
racialData HalfElfDMGConWis = racialData HalfElfDMGStrDex
racialData HalfElfDMGIntWis = racialData HalfElfDMGStrDex
racialData HalfElfVariantStrDex = 
  [ ( "Ability Score Increase", [ "Your Charisma score increases by 2, and two other ability scores of your choice increase by 1." ] )
  , ( "Age", [ "Half-elves mature at the same rate humans do and reach adulthood around the age of 20. They live much longer than humans, however, often exceeding 180 years." ] )
  , ( "Alignment", [ "Half-elves share the chaotic bent of their elven heritage. They value both personal freedom and creative expression, demonstrating neither love of leaders nor desire for followers. They chafe at rules, resent others' demands, and sometimes prove unreliable, or at least unpredictable." ] )
  , ( "Size", [ "HaIf-elves are about the same size as humans, ranging from 5 to 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Thanks to your elf blood, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Elvish, and one extra language of your choice." ] )
  , ( "Half-Elf Variants", 
      [ "Some half-elves in Faerûn have a racial trait in place of the Skill Versatility trait. If your DM allows it, your half-elf character can forgo Skill Versatility and instead take the trait Keen Senses or a trait based on your elf parentage. (Note: There is no reason to take Keen Senses over Skill Versatility. Until errata is published, Wizards of the Coast currently recommends picking one of the choices below instead.)"
      , "Wood Elf: A half-elf of wood elf descent can choose the wood elf's Elf Weapon Training, Fleet of Foot, or Mask of the Wild."
      , "Moon or Sun Elf: A half-elf of moon elf or sun elf descent can choose the high elf's Elf Weapon Training or Cantrip."
      , "Drow: A half-elf of drow descent can choose the drow's Drow Magic."
      , "Aquatic Elf: A half-elf of aquatic heritage can choose a swimming speed of 30 feet." 
      ] 
    )
  ]
racialData HalfElfVariantStrCon = racialData HalfElfVariantStrDex
racialData HalfElfVariantStrInt = racialData HalfElfVariantStrDex
racialData HalfElfVariantStrWis = racialData HalfElfVariantStrDex
racialData HalfElfVariantDexCon = racialData HalfElfVariantStrDex
racialData HalfElfVariantDexInt = racialData HalfElfVariantStrDex
racialData HalfElfVariantDexWis = racialData HalfElfVariantStrDex
racialData HalfElfVariantConInt = racialData HalfElfVariantStrDex
racialData HalfElfVariantConWis = racialData HalfElfVariantStrDex
racialData HalfElfVariantIntWis = racialData HalfElfVariantStrDex
racialData HalfElfDetectionStr = 
  [ ( "Ability Score Increase", [ "Your Wisdom score increases by 2, and one other ability score of your choice increases by 1." ] )
  , ( "Age", [ "Half-elves mature at the same rate humans do and reach adulthood around the age of 20. They live much longer than humans, however, often exceeding 180 years." ] )
  , ( "Alignment", [ "Half-elves share the chaotic bent of their elven heritage. They value both personal freedom and creative expression, demonstrating neither love of leaders nor desire for followers. They chafe at rules, resent others' demands, and sometimes prove unreliable, or at least unpredictable." ] )
  , ( "Size", [ "HaIf-elves are about the same size as humans, ranging from 5 to 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Thanks to your elf blood, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Elvish, and one extra language of your choice." ] )
  , ( "Deductive Intuition", [ "When you make an Intelligence (Investigation) or Wisdom (Insight) check, you can roll a d4 and add the number rolled to the ability check." ] )
  , ( "Magical Detection", [ "You can cast the Detect Magic and Detect Poison and Disease spells with this trait. Starting at 3rd level, you can also cast the See Invisibility spell with it. Once you cast any of these spells with this trait, you can't cast that spell with it again until you finish a long rest. Wisdom is your spellcasting ability for these spells, and you don't require material components for them. (Errata: Casting ability changed to Wisdom from Intelligence)" ] )
  , ( "Spells of the Mark", 
      [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Detection Spells table are added to the spell list of your spellcasting class."
      , "1st level: Detect Evil and Good, Detect Poison and Disease"
      , "2nd level: Detect Thoughts, Find Traps"
      , "3rd level: Clairevoyance, Nondetection"
      , "4th level: Arcane Eye, Divination"
      , "5th level: Legend Lore"
      ]
    )
  ]
racialData HalfElfDetectionDex = racialData HalfElfDetectionStr
racialData HalfElfDetectionCon = racialData HalfElfDetectionStr
racialData HalfElfDetectionInt = racialData HalfElfDetectionStr
racialData HalfElfDetectionCha = racialData HalfElfDetectionStr
racialData HalfElfStorm = 
  [ ( "Ability Score Increase", [ "Your Charisma score increases by 2, and your Dexterity score increases by 1." ] )
  , ( "Age", [ "Half-elves mature at the same rate humans do and reach adulthood around the age of 20. They live much longer than humans, however, often exceeding 180 years." ] )
  , ( "Alignment", [ "Half-elves share the chaotic bent of their elven heritage. They value both personal freedom and creative expression, demonstrating neither love of leaders nor desire for followers. They chafe at rules, resent others' demands, and sometimes prove unreliable, or at least unpredictable." ] )
  , ( "Size", [ "HaIf-elves are about the same size as humans, ranging from 5 to 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Thanks to your elf blood, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Fey Ancestry", [ "You have advantage on saving throws against being charmed, and magic can't put you to sleep." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Elvish, and one extra language of your choice." ] )
  , ( "Windwright's Intuition", [ "When you make an Dexterity (Acrobatics) or any ability check involving navigator's tools, you can roll a d4 and add the number rolled to the ability check." ] )
  , ( "Storm's Boon", [ "You have resistance to lightning damage." ] )
  , ( "Headwinds", [ "You know the Gust cantrip. Starting at 3rd level, you can cast the Gust of Wind spell once with this trait, and you regain the ability to cast it when you finish a long rest. Charisma is your spellcasting ability for these spells." ] )
  , ( "Spells of the Mark", 
      [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Storm Spells table are added to the spell list of your spellcasting class."
      , "1st level: Feather Fall, Fog Cloud"
      , "2nd level: Gust of Wind, Levitate"
      , "3rd level: Sleet Storm, Wind Wall"
      , "4th level: Conjure Minor Elemental, Control Water"
      , "5th level: Conjure Elemental"
      ] 
    )
  ]

racialData HalfOrcStandard = 
  [ ( "Ability Score Increase", [ "Your Strength score increases by 2, and your Constitution score increases by 1." ] )
  , ( "Age", [ "Half-orcs mature a little faster than humans, reaching adulthood around age 14. They age noticeably faster and rarely live longer than 75 years." ] )
  , ( "Alignment", [ "Half-orcs inherit a tendency toward chaos from their orc parents and are not strongly inclined toward good. Half-orcs raised among orcs and willing to live out their lives among them are usually evil." ] )
  , ( "Size", [ "Half-orcs are somewhat larger and bulkier than humans, and they range from 5 to well over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Thanks to your orc blood, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Menacing", [ "You gain proficiency in the Intimidation skill." ] )
  , ( "Relentless Endurance", [ "When you are reduced to 0 hit points but not killed outright, you can drop to 1 hit point instead. You can't use this feature again until you finish a long rest." ] )
  , ( "Savage Attacks", [ "When you score a critical hit with a melee weapon attack, you can roll one of the weapon's damage dice one additional time and add it to the extra damage of the critical hit." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Orc. Orc is a harsh, grating language with hard consonants. It has no script of its own but is written in the Dwarvish script." ] )
  ]
racialData HalfOrcFinding = 
  [ ( "Ability Score Increase", [ "Your Wisdom score increases by 2, and your Constitution score increases by 1." ] )
  , ( "Age", [ "Half-orcs mature a little faster than humans, reaching adulthood around age 14. They age noticeably faster and rarely live longer than 75 years." ] )
  , ( "Alignment", [ "Half-orcs inherit a tendency toward chaos from their orc parents and are not strongly inclined toward good. Half-orcs raised among orcs and willing to live out their lives among them are usually evil." ] )
  , ( "Size", [ "Half-orcs are somewhat larger and bulkier than humans, and they range from 5 to well over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "Thanks to your orc blood, you have superior vision in dark and dim conditions. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Menacing", [ "You gain proficiency in the Intimidation skill." ] )
  , ( "Relentless Endurance", [ "When you are reduced to 0 hit points but not killed outright, you can drop to 1 hit point instead. You can't use this feature again until you finish a long rest." ] )
  , ( "Savage Attacks", [ "When you score a critical hit with a melee weapon attack, you can roll one of the weapon's damage dice one additional time and add it to the extra damage of the critical hit." ] )
  , ( "Darkvision", [ "You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Hunter's Intuition", [ "When you make a Wisdom (Perception) or Wisdom (Survival) check, you can roll a d4 and add the number rolled to the ability check." ] )
  , ( "Finder's Magic", [ "You can cast the Hunter's Mark spell with this trait. Starting at 3rd level, you can also cast the Locate Object spell with it. Once you cast either spell with this trait, you can't cast that spell with it again until you finish a long rest. Wisdom is your spellcasting ability for these spells." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Goblin." ] )
  , ( "Spells of the Mark", 
      [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Finding Spells table are added to the spell list of your spellcasting class."
      , "1st level: Faerie Fire, Longstrider"
      , "2nd level: Locate Animals or Plants, Locate Object"
      , "3rd level: Clairvoyance, Speak with Plants"
      , "4th level: Divination, Locate Creature"
      , "5th level: Commune with Nature"
      ]
    )
  ]

racialData HalflingLightfoot = 
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your Charisma score increases by 1." ] )
  , ( "Age", [ "A halfling reaches adulthood at the age of 20 and generally lives into the middle of his or her second century." ] )
  , ( "Alignment", [ "Most halflings are lawful good. As a rule, they are good-hearted and kind, hate to see others in pain, and have no tolerance for oppression. They are also very orderly and traditional, leaning heavily on the support of their community and the comfort of their old ways." ] )
  , ( "Size", [ "Halflings average about 3 feet tall and weigh about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Lucky", [ "When you roll a 1 on an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll." ] )
  , ( "Brave", [ "You have advantage on saving throws against being frightened." ] )
  , ( "Halfling Nimbleness", [ "You can move through the space of any creature that is of a size larger than yours." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Halfling. The Halfling language isn't secret, but halflings are loath to share it with others. They write very little, so they don't have a rich body of literature. Their oral tradition, however, is very strong. Almost all halflings speak Common to converse with the people in whose lands they dwell or through which they are traveling." ] )
  , ( "Naturally Stealthy", [ "You can attempt to hide even when you are obscured by only a creature that is at least one size larger than you." ] )
  ]
racialData HalflingStout = 
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your Constitution score increases by 1." ] )
  , ( "Age", [ "A halfling reaches adulthood at the age of 20 and generally lives into the middle of his or her second century." ] )
  , ( "Alignment", [ "Most halflings are lawful good. As a rule, they are good-hearted and kind, hate to see others in pain, and have no tolerance for oppression. They are also very orderly and traditional, leaning heavily on the support of their community and the comfort of their old ways." ] )
  , ( "Size", [ "Halflings average about 3 feet tall and weigh about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Lucky", [ "When you roll a 1 on an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll." ] )
  , ( "Brave", [ "You have advantage on saving throws against being frightened." ] )
  , ( "Halfling Nimbleness", [ "You can move through the space of any creature that is of a size larger than yours." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Halfling. The Halfling language isn't secret, but halflings are loath to share it with others. They write very little, so they don't have a rich body of literature. Their oral tradition, however, is very strong. Almost all halflings speak Common to converse with the people in whose lands they dwell or through which they are traveling." ] )
  , ( "Stout Resilience", [ "You have advantage on saving throws against poison, and you have resistance against poison damage." ] )
  ]
racialData HalflingGhostwise =
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your Wisdom score increases by 1." ] )
  , ( "Age", [ "A halfling reaches adulthood at the age of 20 and generally lives into the middle of his or her second century." ] )
  , ( "Alignment", [ "Most halflings are lawful good. As a rule, they are good-hearted and kind, hate to see others in pain, and have no tolerance for oppression. They are also very orderly and traditional, leaning heavily on the support of their community and the comfort of their old ways." ] )
  , ( "Size", [ "Halflings average about 3 feet tall and weigh about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Lucky", [ "When you roll a 1 on an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll." ] )
  , ( "Brave", [ "You have advantage on saving throws against being frightened." ] )
  , ( "Halfling Nimbleness", [ "You can move through the space of any creature that is of a size larger than yours." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Halfling. The Halfling language isn't secret, but halflings are loath to share it with others. They write very little, so they don't have a rich body of literature. Their oral tradition, however, is very strong. Almost all halflings speak Common to converse with the people in whose lands they dwell or through which they are traveling." ] )
  , ( "Silent Speech", [ "You can speak telepathically to any creature within 30 feet of you. The creature understands you only if the two of you share a language. You can speak telepathically in this way to one creature at a time." ] )
  ]
racialData HalflingHealing = 
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your Wisdom score increases by 1." ] )
  , ( "Age", [ "A halfling reaches adulthood at the age of 20 and generally lives into the middle of his or her second century." ] )
  , ( "Alignment", [ "Most halflings are lawful good. As a rule, they are good-hearted and kind, hate to see others in pain, and have no tolerance for oppression. They are also very orderly and traditional, leaning heavily on the support of their community and the comfort of their old ways." ] )
  , ( "Size", [ "Halflings average about 3 feet tall and weigh about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Lucky", [ "When you roll a 1 on an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll." ] )
  , ( "Brave", [ "You have advantage on saving throws against being frightened." ] )
  , ( "Halfling Nimbleness", [ "You can move through the space of any creature that is of a size larger than yours." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Halfling. The Halfling language isn't secret, but halflings are loath to share it with others. They write very little, so they don't have a rich body of literature. Their oral tradition, however, is very strong. Almost all halflings speak Common to converse with the people in whose lands they dwell or through which they are traveling." ] )
  , ( "Medical Intuition", [ "When you make a Wisdom (Medicine) check or an ability check using an herbalism kit, you can roll a d4 and add the number rolled to the ability check." ] )
  , ( "Healing Touch", [ "You can cast the Cure Wounds spell with this trait. Starting at 3rd level, you can also cast Lesser Restoration with it. Once you cast either spell with this trait, you can't cast that spell with it again until you finish a long rest. Wisdom is your spellcasting ability for these spells." ] )
  , ( "Spells of the Mark", 
      [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Healing Spells table are added to the spell list of your spellcasting class."
      , "1st level: Cure Wounds, Healing Word"
      , "2nd level: Lesser Restoration, Prayer of Healing"
      , "3rd level: Aura of Vitality, Mass Healing Word"
      , "4th level: Aura of Purity, Aura of Life"
      , "5th level: Greater Restoration"
      ]
    )
  ]
racialData HalflingHospitality = 
  [ ( "Ability Score Increase", [ "Your Dexterity score increases by 2 and your Charisma score increases by 1." ] )
  , ( "Age", [ "A halfling reaches adulthood at the age of 20 and generally lives into the middle of his or her second century." ] )
  , ( "Alignment", [ "Most halflings are lawful good. As a rule, they are good-hearted and kind, hate to see others in pain, and have no tolerance for oppression. They are also very orderly and traditional, leaning heavily on the support of their community and the comfort of their old ways." ] )
  , ( "Size", [ "Halflings average about 3 feet tall and weigh about 40 pounds. Your size is Small." ] )
  , ( "Speed", [ "Your base walking speed is 25 feet." ] )
  , ( "Lucky", [ "When you roll a 1 on an attack roll, ability check, or saving throw, you can reroll the die and must use the new roll." ] )
  , ( "Brave", [ "You have advantage on saving throws against being frightened." ] )
  , ( "Halfling Nimbleness", [ "You can move through the space of any creature that is of a size larger than yours." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Halfling. The Halfling language isn't secret, but halflings are loath to share it with others. They write very little, so they don't have a rich body of literature. Their oral tradition, however, is very strong. Almost all halflings speak Common to converse with the people in whose lands they dwell or through which they are traveling." ] )
  , ( "Ever Hospitable", [ "When you make a Charisma (Persuasion) check or an ability check involving brewer's supplies or cook's utensils, you can roll a d4 and add the number rolled to the ability check." ] )
  , ( "Innkeeper's Magic", [ "You can cast the Prestidigitation cantrip. You can also cast the Purify Food and Drink and Unseen Servant spells with this trait. Once you cast either spell with this trait, you can't cast that spell with it again until you finish a long rest. Charisma is your spellcasting ability for these spells." ] )
  , ( "Spells of the Mark", 
      [ "If you have the Spellcasting or the Pact Magic class feature, the spells on the Mark of Hospitality Spells table are added to the spell list of your spellcasting class."
      , "1st level: Goodberry, Sleep"
      , "2nd level: Aid, Calm Emotions"
      , "3rd level: Create Food and Water, Leomund's Tiny Hut"
      , "4th level: Aura of Purity, Mordenkainen's Private Sanctum"
      , "5th level: Hallow"
      ]
    )
  ]

racialData Hobgoblin = 
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Intelligence score increases by 1." ] )
  , ( "Age", [ "Hobgoblins mature at the same rate as humans and have lifespans similar in length to theirs." ] )
  , ( "Alignment", [ "Hobgoblin society is built on fidelity to a rigid, unforgiving code of conduct. As such, they tend toward lawful evil." ] )
  , ( "Size", [ "Hobgoblins are between 5 and 6 feet tall and weigh between 150 and 200 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Martial Training", [ "You are proficient with two martial weapons of your choice and with light armor." ] )
  , ( "Saving Face", [ "Hobgoblins are careful not to show weakness in front of their allies, for fear of losing status. If you miss with an attack roll or fail an ability check or a saving throw, you can gain a bonus to the roll equal to the number of allies you can see within 30 feet of you (maximum bonus of +5). Once you use this trait, you can't use it again until you finish a short or long rest." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Goblin." ] )
  ]

racialData HumanStandard = []
racialData HumanVariantStrDex = []
racialData HumanVariantStrCon = []
racialData HumanVariantStrInt = []
racialData HumanVariantStrWis = []
racialData HumanVariantStrCha = []
racialData HumanVariantDexCon = []
racialData HumanVariantDexInt = []
racialData HumanVariantDexWis = []
racialData HumanVariantDexCha = []
racialData HumanVariantConInt = []
racialData HumanVariantConWis = []
racialData HumanVariantConCha = []
racialData HumanVariantIntWis = []
racialData HumanVariantIntCha = []
racialData HumanVariantWisCha = []
racialData HumanFinding = []
racialData HumanHandlingStr = []
racialData HumanHandlingDex = []
racialData HumanHandlingCon = []
racialData HumanHandlingInt = []
racialData HumanHandlingCha = []
racialData HumanMakingStr = []
racialData HumanMakingDex = []
racialData HumanMakingCon = []
racialData HumanMakingWis = []
racialData HumanMakingCha = []
racialData HumanPassageStr = []
racialData HumanPassageCon = []
racialData HumanPassageInt = []
racialData HumanPassageWis = []
racialData HumanPassageCha = []
racialData HumanSentinel = []

racialData Kalashtar = []

racialData Kenku = []

racialData Kobold = []

racialData Leonin = []

racialData Lizardfolk = []

racialData Locathah = []

racialData Loxodon = []

racialData Minotaur = []

racialData OrcStandard = []
racialData OrcEberron = []

racialData Satyr = []

racialData ShifterBeasthide = []
racialData ShifterLongtooth = []
racialData ShifterSwiftstride = []
racialData ShifterWildhunt = []

racialData SimicStr = []
racialData SimicDex = []
racialData SimicInt = []
racialData SimicWis = []
racialData SimicCha = []

racialData Tabaxi = []

racialData TieflingNormalDevilsTongue = []
racialData TieflingNormalHellfire = []
racialData TieflingNormalWinged = []
racialData TieflingNormalAsmodeus = []
racialData TieflingNormalBaalzebul = []
racialData TieflingNormalDispater = []
racialData TieflingNormalFierna = []
racialData TieflingNormalGlasya = []
racialData TieflingNormalLevistus = []
racialData TieflingNormalMammon = []
racialData TieflingNormalMephistopheles = []
racialData TieflingNormalZariel = []

racialData TieflingFeralDevilsTongue = []
racialData TieflingFeralHellfire = []
racialData TieflingFeralWinged = []

racialData Tortle = []

racialData Triton = 
  [ ( "Ability Score Increase", [ "Your Strength, Constitution, and Charisma scores each increase by 1." ] )
  , ( "Age", [ "Tritons reach maturity around age 15 and can live up to 200 years." ] )
  , ( "Alignment", [ "Tritons tend toward lawful good. As guardians of the darkest reaches of the sea, their culture pushes them toward order and benevolence." ] )
  , ( "Size", [ "Tritons are slightly shorter than humans, averaging 5 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet, and you have a swimming speed of 30 feet." ] )
  , ( "Amphibious", [ "You can breathe air and water." ] )
  , ( "Control Air and Water", [ "A child of the sea, you can call on the magic of elemental air and water. You can cast Fog Cloud with this trait. Starting at 3rd level, you can cast Gust of Wind with it, and starting at 5th level, you can also cast Wall of Water with it. Once you cast a spell with this trait, you can’t cast that spell with it again until you finish a long rest. Charisma is your spellcasting ability for these spells." ] )
  , ( "Darkvision", [ "You can see in dim light within 60 feet of you as if it were bright light and in darkness as if it were dim light. You can’t discern color in darkness, only shades of gray. (Errata: Added Darkvision)" ] )
  , ( "Emissary of the Sea", [ "Aquatic beasts have an extraordinary affinity with your people. You can communicate simple ideas with beasts that can breathe water. They can understand the meaning of your words, though you have no special ability to understand them in return." ] )
  , ( "Guardians of the Depths", [ "Adapted to even the most extreme ocean depths, you have resistance to cold damage. (Errata: Removed additional text)" ] )
  , ( "Languages", [ "You can speak, read, and write Common and Primordial." ] )
  ]
  
racialData Vedalken = 
  [ ( "Ability Score Increase", [ "Your Intelligence score increases by 2, and your Wisdom score increases by 1." ] )
  , ( "Age", [ "Vedalken mature slower than humans do, reaching maturity around age 40. Their life span is typically 350 years, with some living to the age of 500." ] )
  , ( "Alignment", [ "Vedalken are usually lawful and non-evil." ] )
  , ( "Size", [ "Tall and slender, Vedalken stand 6 to 6 ½ feet tall on average and usually weigh less than 200 pounds. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Vedalken Dispassion", [ "You have advantage on all Intelligence, Wisdom, and Charisma saving throws." ] )
  , ( "Tireless Precision", 
      [ "You are proficient in one of the following skills of your choice: Arcana, History, Investigation, Medicine, Performance, or Sleight of Hand. You are also proficient with one tool of your choice."
      , "Whenever you make an ability check with the chosen skill or tool, roll a d4 and add the number rolled to the check's total."
      ]
    )
  , ( "Partially Amphibious", [ "By absorbing oxygen through your skin, you can breathe underwater for up to 1 hour. Once you've reached that limit, you can't use this trait again until you finish a long rest." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Vedalken, and one other language of your choice." ] )
  ]
  
racialData WarforgedStr = 
  [ ( "Ability Score Increase" , [ "Your Constitution score increases by 2, and one other ability score of your choice increases by 1." ] )
  , ( "Age" , [ "A typical warforged is between two and thirty years old. The maximum warforged lifespan remains a mystery; so far, warforged have shown no signs of deterioration due to age. You are immune to magical aging effects." ] )
  , ( "Alignment" , [ "Most warforged take comfort in order and discipline, tending toward law and neutrality. But some have absorbed the morality, or lack thereof, of the beings with which they served." ] )
  , ( "Size" , 
      [ "Your size is Medium. To set your height and weight randomly, start with a rolling size modifier:"
      , "Size modifier = 2d6"
      , "Height = 5 feet + 10 inches + your size modifier"
      , "Weight in pounds = 270 + (4 x your size modifier)" 
      ] 
    )
  , ( "Speed" , [ "Your base walking speed is 30 feet." ] )
  , ( "Constructed Resilience" , 
      [ "You were constructed to have remarkable fortitude, represented by the following benefits:"
      , "You have advantage on savings throws against being poisoned, and you have resistance to poison damage."
      , "You don't need to eat, drink, or breathe."
      , "You are immune to disease."
      , "You don't need to sleep, and magic can't put you to sleep."
      ]
    )
  , ( "Sentry's Rest" , [ "When you take a long rest, you must spend at least six hours in an inactive, motionless state, rather than sleeping. In this state, you appear inert, but it doesn't render you unconcious, and you can see and hear as normal." ] )
  , ( "Integrated Protection" , 
      [ "Your body has a built in defensive layers, which can be enhanced with armor:"
      , "You gain a +1 bonus to Armor Class."
      , "You can don only armor with which you have proficency. To don armor other than a shield, you must incorporate it into your body over the course of 1 hour, during which, you remain in contact with the armor. To doff armor, you must spend 1 hour removing it. You can rest while donning or doffing armor in this way."
      , "While you live, the armor incorporated into your body can’t be removed against your will"
      ]
    )
  , ( "Specialized Design" , [ "You gain one skill proficency and one tool proficency of your choice." ] )
  , ( "Languages" , [ "You can speak, read, and write Common and one other language of your choice." ] )
  ]
racialData WarforgedDex = racialData WarforgedStr
racialData WarforgedInt = racialData WarforgedStr
racialData WarforgedWis = racialData WarforgedStr
racialData WarforgedCha = racialData WarforgedStr

racialData YuanTiPureblood = 
  [ ( "Ability Score Increase", [ "Your Charisma score increases by 2, and your Intelligence score increases by 1." ] )
  , ( "Age", [ "Purebloods mature at the same rate as humans and have lifespans similar in length to theirs." ] )
  , ( "Alignment", [ "Purebloods are devoid of emotion and see others as tools to manipulate. They care little for law or chaos and are typically neutral evil." ] )
  , ( "Size", [ "Purebloods match humans in average size and weight. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Darkvision", [ "You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] )
  , ( "Innate Spellcasting", [ "You know the Poison Spray cantrip. You can cast Animal Friendship an unlimited number of times with this trait, but you can only target snakes with it. Starting at 3rd level, you can also cast Suggestion with this trait. Once you cast it, you can't do so again until you finish a long rest. Charisma is your spellcasting ability for these spells." ] )
  , ( "Magic Resistance", [ "You have advantage on all saving throws against spells and other magical effects." ] )
  , ( "Poison Immunity", [ "You are immune to poison damage and the poisoned condition." ] )
  , ( "Languages", [ "You can speak, read, and write Common, Abyssal, and Draconic." ] )

  ]
