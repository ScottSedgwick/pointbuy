{-# LANGUAGE OverloadedStrings #-}
module View.Calculator.Race where

import qualified Data.Map as M
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import           Miso.Lens

import Types
import Types.Races
import Types.Stats

raceSelector :: Model -> View Model Action
raceSelector x =
  case x ^. race of
    _ -> standardSelector x

standardSelector :: Model -> View Model Action
standardSelector x =
  H.div_ [ P.className "grid" ]
  [ raceSelect x
  ]

raceSelect :: Model -> View Model Action
raceSelect x =
  H.div_ [ P.className "field label suffix border s12"] 
  [ H.select_ [ E.onInput ChangeRace ] ( map (mkRaceOption (x ^. race)) allRaces )
  , H.label_ [] [ text"Select Race" ]
  , H.i_ [] [ text "arrow_drop_down" ]
  ]

mkRaceOption :: Race -> Race -> View Model Action
mkRaceOption c r = H.option_ [ P.selected_ (c == r), P.value_ (ms $ show r) ] [ text (ms $ Types.Races.showPretty r) ]

aasimarSelector :: Model -> View Model Action
aasimarSelector x =
  H.div_ [ P.className "grid" ]
  [ raceSelect x
  , H.div_ [ P.className "s6" ] [ text "Aasimar" ]
  ]


racialTraits :: Race -> [View Model Action]
racialTraits CustomRace = []
racialTraits Aarakocra =
  [ H.h6_ [] [ text "Ability Score Increase" ]
  , H.ul_ [] [ H.li_ [] [ text "Your Dexterity score increases by 2, and your Wisdom score increases by 1." ] ]
  , H.h6_ [] [ text "Age" ]
  , H.ul_ [] [ H.li_ [] [ text "Aarakocra reach maturity by age 3. Compared to humans, aarakocra don't usually live longer than 30 years." ] ]
  , H.h6_ [] [ text "Alignment" ]
  , H.ul_ [] [ H.li_ [] [ text "Most aarakocra are good and rarely choose sides when it comes to law and chaos. Tribal leaders and warriors might be lawful, while explorers and adventurers might tend toward chaotic." ] ]
  , H.h6_ [] [ text "Size" ]
  , H.ul_ [] [ H.li_ [] [ text "Aarakocra are about 5 feet tall. They have thin, lightweight bodies that weigh between 80 and 100 pounds. Your size is Medium." ] ]
  , H.h6_ [] [ text "Speed" ]
  , H.ul_ [] [ H.li_ [] [ text "Your base walking speed is 25 feet." ] ]
  , H.h6_ [] [ text "Flight" ]
  , H.ul_ [] [ H.li_ [] [ text "You have a flying speed of 50 feet. To use this speed, you can't be wearing medium or heavy armor." ] ]
  , H.h6_ [] [ text "Talons" ]
  , H.ul_ [] [ H.li_ [] [ text "You are proficient with your unarmed strikes, which deal 1d4 slashing damage on a hit." ] ]
  , H.h6_ [] [ text "Languages" ]
  , H.ul_ [] [ H.li_ [] [ text "You can speak, read, and write Common, Aarakocra, and Auran." ] ]
  ]
    
racialTraits (AasimarDMG) =
  [ H.h6_ [] [ text "Ability Score Increase" ]
  , H.ul_ [] [ H.li_ [] [ text "Your Wisdom score increases by 1, and your Charisma score increases by 2." ] ]
  , H.h6_ [] [ text "Age" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar mature at the same rate as humans but live a few years longer." ] ]
  , H.h6_ [] [ text "Alignment" ]
  , H.ul_ [] [ H.li_ [] [ text "Due to their celestial heritage, aasimar are often good. However, some aasimar fall into evil, rejecting their heritage." ] ]
  , H.h6_ [] [ text "Size" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar have the same range of height and weight as humans." ] ]
  , H.h6_ [] [ text "Speed" ]
  , H.ul_ [] [ H.li_ [] [ text "Your base walking speed is 30 feet." ] ]
  , H.h6_ [] [ text "Darkvision" ]
  , H.ul_ [] [ H.li_ [] [ text "Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] ]
  , H.h6_ [] [ text "Celestial Resistance" ]
  , H.ul_ [] [ H.li_ [] [ text "You have resistance to necrotic damage and radiant damage." ] ]
  , H.h6_ [] [ text "Languages" ]
  , H.ul_ [] [ H.li_ [] [ text "You can speak, read, and write Common, and Celestial." ] ]
  , H.h6_ [] [ text "Celestial Legacy" ]
  , H.ul_ [] 
    [ H.li_ [] [ text "You know the Light cantrip." ]
    , H.li_ [] [ text "Once you reach 3rd level, you can cast the Lesser Restoration spell once with this trait, and you regain the ability to do so when you finish a long rest." ]
    , H.li_ [] [ text "Once you reach 5th level, you can cast the Daylight spell once with this spell as a 3rd level spell, and you regain the ability to do so when you finish a long rest." ]
    , H.li_ [] [ text "Charisma is your spellcasting ability for these spells." ] ]
  ]
racialTraits (AasimarProtector) =
  [ H.h6_ [] [ text "Ability Score Increase" ]
  , H.ul_ [] [ H.li_ [] [ text "Your Charisma score increases by 2, and your Wisdom score increases by 1." ] ]
  , H.h6_ [] [ text "Age" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar mature at the same rate as humans, but can live up to 160 years." ] ]
  , H.h6_ [] [ text "Alignment" ]
  , H.ul_ [] [ H.li_ [] [ text "Imbued with celestial power, most aasimar are good. Outcast aasimar are most often neutral or even evil." ] ]
  , H.h6_ [] [ text "Size" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar have the same range of height and weight as humans." ] ]
  , H.h6_ [] [ text "Speed" ]
  , H.ul_ [] [ H.li_ [] [ text "Your base walking speed is 30 feet." ] ]
  , H.h6_ [] [ text "Darkvision" ]
  , H.ul_ [] [ H.li_ [] [ text "Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] ]
  , H.h6_ [] [ text "Celestial Resistance" ]
  , H.ul_ [] [ H.li_ [] [ text "You have resistance to necrotic damage and radiant damage." ] ]
  , H.h6_ [] [ text "Healing Hands" ]
  , H.ul_ [] [ H.li_ [] [ text "As an action, you can touch a creature and cause it to regain a number of hit points equal to your level. Once you use this trait, you can't use it again until you finish a long rest." ] ]
  , H.h6_ [] [ text "Light Bearer" ]
  , H.ul_ [] [ H.li_ [] [ text "You know the Light cantrip. Charisma is your spellcasting ability for it." ] ]
  , H.h6_ [] [ text "Languages" ]
  , H.ul_ [] [ H.li_ [] [ text "You can speak, read, and write Common, and Celestial." ] ]
  , H.h6_ [] [ text "Radiant Soul" ]
  , H.ul_ [] 
    [ H.li_ [] [ text "Starting at 3rd level, you can use your action to unleash the divine energy within yourself, causing your eyes to glimmer and two luminous, incorporeal wings to sprout from your back. " ]
    , H.li_ [] [ text "Your transformation lasts for 1 minute or until you end it as a bonus action. During it, you have a flying speed of 30 feet, and once on each of your turns, you can deal extra radiant damage to one target when you deal damage to it with an attack or a spell. The extra radiant damage equals your level." ]
    , H.li_ [] [ text "Once you use this trait, you can't use it again until you finish a long rest." ]
    ]
  ]
racialTraits (AasimarScourge) =
  [ H.h6_ [] [ text "Ability Score Increase" ]
  , H.ul_ [] [ H.li_ [] [ text "Your Charisma score increases by 2, and your Constitution score increases by 1." ] ]
  , H.h6_ [] [ text "Age" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar mature at the same rate as humans, but can live up to 160 years." ] ]
  , H.h6_ [] [ text "Alignment" ]
  , H.ul_ [] [ H.li_ [] [ text "Imbued with celestial power, most aasimar are good. Outcast aasimar are most often neutral or even evil." ] ]
  , H.h6_ [] [ text "Size" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar have the same range of height and weight as humans." ] ]
  , H.h6_ [] [ text "Speed" ]
  , H.ul_ [] [ H.li_ [] [ text "Your base walking speed is 30 feet." ] ]
  , H.h6_ [] [ text "Darkvision" ]
  , H.ul_ [] [ H.li_ [] [ text "Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] ]
  , H.h6_ [] [ text "Celestial Resistance" ]
  , H.ul_ [] [ H.li_ [] [ text "You have resistance to necrotic damage and radiant damage." ] ]
  , H.h6_ [] [ text "Healing Hands" ]
  , H.ul_ [] [ H.li_ [] [ text "As an action, you can touch a creature and cause it to regain a number of hit points equal to your level. Once you use this trait, you can't use it again until you finish a long rest." ] ]
  , H.h6_ [] [ text "Light Bearer" ]
  , H.ul_ [] [ H.li_ [] [ text "You know the Light cantrip. Charisma is your spellcasting ability for it." ] ]
  , H.h6_ [] [ text "Languages" ]
  , H.ul_ [] [ H.li_ [] [ text "You can speak, read, and write Common, and Celestial." ] ]
  , H.h6_ [] [ text "Radiant Consumption" ]
  , H.ul_ [] 
    [ H.li_ [] [ text "Starting at 3rd level, you can use your action to unleash the divine energy within yourself, causing a searing light to radiate from you, pour out of your eyes and mouth, and threaten to char you." ]
    , H.li_ [] [ text "Your transformation lasts for 1 minute or until you end it as a bonus action. During it, you shed bright light in a 10-foot radius and dim light for an additional 10 feet, and at the end of each of your turns, you and each creature within 10 feet of you take radiant damage equal to half your level (rounded up). In addition, once on each of your turns, you can deal extra radiant damage to one target when you deal damage to it with an attack or a spell. The extra radiant damage equals your level." ]
    , H.li_ [] [ text "Once you use this trait, you can't use it again until you finish a long rest." ]
    ]
  ]
racialTraits (AasimarFallen) =
  [ H.h6_ [] [ text "Ability Score Increase" ]
  , H.ul_ [] [ H.li_ [] [ text "Your Charisma score increases by 2, and your Strength score increases by 1." ] ]
  , H.h6_ [] [ text "Age" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar mature at the same rate as humans, but can live up to 160 years." ] ]
  , H.h6_ [] [ text "Alignment" ]
  , H.ul_ [] [ H.li_ [] [ text "Imbued with celestial power, most aasimar are good. Outcast aasimar are most often neutral or even evil." ] ]
  , H.h6_ [] [ text "Size" ]
  , H.ul_ [] [ H.li_ [] [ text "Aasimar have the same range of height and weight as humans." ] ]
  , H.h6_ [] [ text "Speed" ]
  , H.ul_ [] [ H.li_ [] [ text "Your base walking speed is 30 feet." ] ]
  , H.h6_ [] [ text "Darkvision" ]
  , H.ul_ [] [ H.li_ [] [ text "Blessed with a radiant soul, your vision can easily cut through darkness. You can see in dim light within 60 feet of you as if it were bright light, and in darkness as if it were dim light. You can't discern color in darkness, only shades of gray." ] ]
  , H.h6_ [] [ text "Celestial Resistance" ]
  , H.ul_ [] [ H.li_ [] [ text "You have resistance to necrotic damage and radiant damage." ] ]
  , H.h6_ [] [ text "Healing Hands" ]
  , H.ul_ [] [ H.li_ [] [ text "As an action, you can touch a creature and cause it to regain a number of hit points equal to your level. Once you use this trait, you can't use it again until you finish a long rest." ] ]
  , H.h6_ [] [ text "Light Bearer" ]
  , H.ul_ [] [ H.li_ [] [ text "You know the Light cantrip. Charisma is your spellcasting ability for it." ] ]
  , H.h6_ [] [ text "Languages" ]
  , H.ul_ [] [ H.li_ [] [ text "You can speak, read, and write Common, and Celestial." ] ]
  , H.h6_ [] [ text "Radiant Consumption" ]
  , H.ul_ [] 
    [ H.li_ [] [ text "Starting at 3rd level, you can use your action to unleash the divine energy within yourself, causing your eyes to turn into pools of darkness and two skeletal, ghostly, flightless wings to sprout from your back. The instant you transform, other creatures within 10 feet of you that you can see you must each succeed on a Charisma saving throw (DC 8 + your proficiency bonus + your Charisma modifier) or become frightened of you until the end of your next turn." ]
    , H.li_ [] [ text "Your transformation lasts for 1 minute or until you end it as a bonus action. During it, once on each of your turns, you can deal extra necrotic damage to one target when you deal damage to it with an attack or a spell. The extra necrotic damage equals your level." ]
    , H.li_ [] [ text "Once you use this trait, you can't use it again until you finish a long rest." ]
    ]
  ]

racialTraits (Bugbear) =
    []
  
racialTraits (Centaur) =
    []

racialTraits (ChangelingStrength) =
    []
racialTraits (ChangelingDexterity) =
    []
racialTraits (ChangelingConstitution) =
    []
racialTraits (ChangelingIntelligence) =
    []
racialTraits (ChangelingWisdom) =
    []

racialTraits (Dragonborn) =
    []

racialTraits (DwarfHill) =
    []
racialTraits (DwarfMountain) =
    []
racialTraits (DwarfDuergar) =
    []
racialTraits (DwarfWarding) =
    []

racialTraits (ElfHigh) =
    []
racialTraits (ElfWood) =
    []
racialTraits (ElfEladrin) =
    []
racialTraits (ElfEladrinMtof) =
    []
racialTraits (ElfDrow) =
    []
racialTraits (ElfSea) =
    []
racialTraits (ElfShadarKai) =
    []
racialTraits (ElfShadow) =
    []

racialTraits (Firbolg) =
    []

racialTraits (GenasiAir) =
    []
racialTraits (GenasiEarth) =
    []
racialTraits (GenasiFire) =
    []
racialTraits (GenasiWater) =
    []
  
racialTraits (Githyanki) =
    []
racialTraits (Githzerai) =
    []

racialTraits (GnomeForest) =
    []
racialTraits (GnomeRock) =
    []
racialTraits (GnomeDeep) =
    []
racialTraits (GnomeScribing) =
    []

racialTraits (Goblin) =
    []

racialTraits (Goliath) =
    []

racialTraits (Grung) =
    []

racialTraits ((HalfElfDMG HalfElfDMGStrength HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGStrength HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGStrength HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGStrength HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGStrength HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGStrength HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGStrength HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGStrength HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGStrength HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGStrength HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGStrength)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGDexterity)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGConstitution)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGIntelligence)) =
    []
racialTraits ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGWisdom)) =
    []
racialTraits ((HalfElfDetection HalfElfDetectionStrength)) =
    []
racialTraits ((HalfElfDetection HalfElfDetectionDexterity)) =
    []
racialTraits ((HalfElfDetection HalfElfDetectionConstitution)) =
    []
racialTraits ((HalfElfDetection HalfElfDetectionIntelligence)) =
    []
racialTraits ((HalfElfDetection HalfElfDetectionCharisma)) =
    []
racialTraits (HalfElfStorm) =
    []

racialTraits (HalfOrcStandard) =
    []
racialTraits (HalfOrcFinding) =
    []

racialTraits (HalflingLightfoot) =
    []
racialTraits (HalflingStout) =
    []
racialTraits (HalflingGhostwise) =
    []
racialTraits (HalflingHealing) =
    []
racialTraits (HalflingHospitality) =
    []

racialTraits (Hobgoblin) =
    []

racialTraits (HumanStandard) =
    []

racialTraits HumanVariantStrDex = []
racialTraits HumanVariantStrCon = []
racialTraits HumanVariantStrInt = []
racialTraits HumanVariantStrWis = []
racialTraits HumanVariantStrCha = []
racialTraits HumanVariantDexCon = []
racialTraits HumanVariantDexInt = []
racialTraits HumanVariantDexWis = []
racialTraits HumanVariantDexCha = []
racialTraits HumanVariantConInt = []
racialTraits HumanVariantConWis = []
racialTraits HumanVariantConCha = []
racialTraits HumanVariantIntWis = []
racialTraits HumanVariantIntCha = []
racialTraits HumanVariantWisCha = []

racialTraits (HumanFinding) =
    []
racialTraits ((HumanHandling HumanHandlingStrength)) =
    []
racialTraits ((HumanHandling HumanHandlingDexterity)) =
    []
racialTraits ((HumanHandling HumanHandlingConstitution)) =
    []
racialTraits ((HumanHandling HumanHandlingIntelligence)) =
    []
racialTraits ((HumanHandling HumanHandlingCharisma)) =
    []
racialTraits ((HumanMaking HumanMakingStrength)) =
    []
racialTraits ((HumanMaking HumanMakingDexterity)) =
    []
racialTraits ((HumanMaking HumanMakingConstitution)) =
    []
racialTraits ((HumanMaking HumanMakingWisdom)) =
    []
racialTraits ((HumanMaking HumanMakingCharisma)) =
    []
racialTraits ((HumanPassage HumanPassageStrength)) =
    []
racialTraits ((HumanPassage HumanPassageConstitution)) =
    []
racialTraits ((HumanPassage HumanPassageIntelligence)) =
    []
racialTraits ((HumanPassage HumanPassageWisdom)) =
    []
racialTraits ((HumanPassage HumanPassageCharisma)) =
    []
racialTraits (HumanSentinel) =
    []

racialTraits (Kalashtar) =
    []

racialTraits (Kenku) =
    []

racialTraits (Kobold) =
    []

racialTraits (Leonin) =
    []

racialTraits (Lizardfolk) =
    []

racialTraits (Locathah) =
    []

racialTraits (Loxodon) =
    []

racialTraits (Minotaur) =
    []

racialTraits (OrcStandard) =
    []
racialTraits (OrcEberron) =
    []

racialTraits (Satyr) =
    []

racialTraits (ShifterBeasthide) =
    []
racialTraits (ShifterLongtooth) =
    []
racialTraits (ShifterSwiftstride) =
    []
racialTraits (ShifterWildhunt) =
    []

racialTraits (SimicStrength) =
    []
racialTraits (SimicDexterity) =
    []
racialTraits (SimicIntelligence) =
    []
racialTraits (SimicWisdom) =
    []
racialTraits (SimicCharisma) =
    []

racialTraits (Tabaxi) =
    []

racialTraits ((TieflingNormal TieflingNormalDevilsTongue)) =
    []
racialTraits ((TieflingNormal TieflingNormalHellfire)) =
    []
racialTraits ((TieflingNormal TieflingNormalWinged)) =
    []
racialTraits ((TieflingNormal TieflingNormalAsmodeus)) =
    []
racialTraits ((TieflingNormal TieflingNormalBaalzebul)) =
    []
racialTraits ((TieflingNormal TieflingNormalDispater)) =
    []
racialTraits ((TieflingNormal TieflingNormalFierna)) =
    []
racialTraits ((TieflingNormal TieflingNormalGlasya)) =
    []
racialTraits ((TieflingNormal TieflingNormalLevistus)) =
    []
racialTraits ((TieflingNormal TieflingNormalMammon)) =
    []
racialTraits ((TieflingNormal TieflingNormalMephistopheles)) =
    []
racialTraits ((TieflingNormal TieflingNormalZariel)) =
    []

racialTraits ((TieflingFeral TieflingFeralDevilsTongue)) =
    []
racialTraits ((TieflingFeral TieflingFeralHellfire)) =
    []
racialTraits ((TieflingFeral TieflingFeralWinged)) =
    []

racialTraits (Tortle) =
    []

racialTraits (Triton) =
    []
  
racialTraits (Vedalken) =
    []
  
racialTraits (WarforgedStrength) =
    []
racialTraits (WarforgedDexterity) =
    []
racialTraits (WarforgedIntelligence) =
    []
racialTraits (WarforgedWisdom) =
    []
racialTraits (WarforgedCharisma) =
    []

racialTraits (YuanTiPureblood) =
    []
