{-# LANGUAGE OverloadedStrings #-}
module View.Calculator.Race where

import           Control.Lens       ( (^.) )
import qualified Data.Map           as M
import           Miso               ( View, ms, text )
import qualified Miso.Html          as H
import qualified Miso.Html.Event    as E
import qualified Miso.Html.Property as P

import           Types              ( Action(..), Model, race )
import           Types.Races        ( Race(..), HalfElfDMGStat(..), HalfElfDetectionStat(..), HumanHandlingStat(..), HumanMakingStat(..), HumanPassageStat(..), TieflingFeralType(..), TieflingNormalType(..), allRaces, showPretty )

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
mkRaceOption c r = H.option_ [ P.selected_ (c == r), P.value_ (ms $ show r) ] [ text (ms $ showPretty r) ]

aasimarSelector :: Model -> View Model Action
aasimarSelector x =
  H.div_ [ P.className "grid" ]
  [ raceSelect x
  , H.div_ [ P.className "s6" ] [ text "Aasimar" ]
  ]


racialTraits :: Race -> [View Model Action]
racialTraits = formatTraits . racialData

formatTraits :: [(String, [String])] -> [View Model Action]
formatTraits = concatMap (\(x,xs) -> [ H.h6_ [] [ text (ms x) ], H.ul_ [] (map (\y -> H.li_ [] [ text (ms y)]) xs)])

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
    
racialData (AasimarDMG) =
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
racialData (AasimarProtector) =
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
racialData (AasimarScourge) =
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
racialData (AasimarFallen) =
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

racialData (Bugbear) =
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
  
racialData (Centaur) =
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

racialData (ChangelingStrength) =
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
racialData (ChangelingDexterity) = racialData (ChangelingStrength)
racialData (ChangelingConstitution) = racialData (ChangelingStrength)
racialData (ChangelingIntelligence) = racialData (ChangelingStrength)
racialData (ChangelingWisdom) = racialData (ChangelingStrength)

racialData (Dragonborn) =
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

racialData (DwarfHill) =
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
racialData (DwarfMountain) =
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
racialData (DwarfDuergar) =
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
racialData (DwarfWarding) =
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

racialData (ElfHigh) =
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
racialData (ElfWood) =
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
racialData (ElfEladrin) =
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
racialData (ElfEladrinMtof) =
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
racialData (ElfDrow) =
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
racialData (ElfSea) =
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
racialData (ElfShadarKai) =
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
racialData (ElfShadow) =
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

racialData (Firbolg) =
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

racialData (GenasiAir) =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Dexterity score increases by 1." ] )
  , ( "Age", [ "Genasi mature at about the same rate as humans and reach adulthood in their late teens. They live somewhat longer than humans do, up to 120 years." ] )
  , ( "Alignment", [ "Independent and self-reliant, genasi tend toward a neutral alignment." ] )
  , ( "Size", [ "Genasi are as varied as their mortal parents but are generally built like humans, standing anywhere from 5 feet to over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Primordial. Primordial is a guttural language, filled with harsh syllables and hard consonants." ] )
  , ( "Unending Breath", [ "You can hold your breath indefinitely while you're not incapacitated." ] )
  , ( "Mingle with the Wind", [ "You can cast the levitate spell once with this trait, requiring no material components, and you regain the ability to cast it this way when you finish a long rest. Constitution is your spellcasting ability for this spell." ] )
  ]
racialData (GenasiEarth) =
  [ ( "Ability Score Increase", [ "Your Constitution score increases by 2, and your Strength score increases by 1." ] )
  , ( "Age", [ "Genasi mature at about the same rate as humans and reach adulthood in their late teens. They live somewhat longer than humans do, up to 120 years." ] )
  , ( "Alignment", [ "Independent and self-reliant, genasi tend toward a neutral alignment." ] )
  , ( "Size", [ "Genasi are as varied as their mortal parents but are generally built like humans, standing anywhere from 5 feet to over 6 feet tall. Your size is Medium." ] )
  , ( "Speed", [ "Your base walking speed is 30 feet." ] )
  , ( "Languages", [ "You can speak, read, and write Common and Primordial. Primordial is a guttural language, filled with harsh syllables and hard consonants." ] )
  , ( "Earth Walk", [ "You can move across difficult terrain made of earth or stone without expending extra movement." ] )
  , ( "Merge with Stone", [ "You can cast the pass without trace spell once with this trait, requiring no material components, and you regain the ability to cast it this way when you finish a long rest. Constitution is your spellcasting ability for this spell." ] )
  ]
racialData (GenasiFire) =
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
racialData (GenasiWater) =
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
  
racialData (Githyanki) =
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
racialData (Githzerai) =
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

racialData (GnomeForest) =
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
racialData (GnomeRock) =
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
racialData (GnomeDeep) =
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
racialData (GnomeScribing) =
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

racialData (Goblin) =
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

racialData (Goliath) =
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

racialData (Grung) =
    []

racialData ((HalfElfDMG HalfElfDMGStrength HalfElfDMGStrength)) =
    []
racialData ((HalfElfDMG HalfElfDMGStrength HalfElfDMGDexterity)) =
    []
racialData ((HalfElfDMG HalfElfDMGStrength HalfElfDMGConstitution)) =
    []
racialData ((HalfElfDMG HalfElfDMGStrength HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfDMG HalfElfDMGStrength HalfElfDMGWisdom)) =
    []
racialData ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGStrength)) =
    []
racialData ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGDexterity)) =
    []
racialData ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGConstitution)) =
    []
racialData ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfDMG HalfElfDMGDexterity HalfElfDMGWisdom)) =
    []
racialData ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGStrength)) =
    []
racialData ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGDexterity)) =
    []
racialData ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGConstitution)) =
    []
racialData ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfDMG HalfElfDMGConstitution HalfElfDMGWisdom)) =
    []
racialData ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGStrength)) =
    []
racialData ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGDexterity)) =
    []
racialData ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGConstitution)) =
    []
racialData ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfDMG HalfElfDMGIntelligence HalfElfDMGWisdom)) =
    []
racialData ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGStrength)) =
    []
racialData ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGDexterity)) =
    []
racialData ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGConstitution)) =
    []
racialData ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfDMG HalfElfDMGWisdom HalfElfDMGWisdom)) =
    []
racialData ((HalfElfVariant HalfElfDMGStrength HalfElfDMGStrength)) =
    []
racialData ((HalfElfVariant HalfElfDMGStrength HalfElfDMGDexterity)) =
    []
racialData ((HalfElfVariant HalfElfDMGStrength HalfElfDMGConstitution)) =
    []
racialData ((HalfElfVariant HalfElfDMGStrength HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfVariant HalfElfDMGStrength HalfElfDMGWisdom)) =
    []
racialData ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGStrength)) =
    []
racialData ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGDexterity)) =
    []
racialData ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGConstitution)) =
    []
racialData ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfVariant HalfElfDMGDexterity HalfElfDMGWisdom)) =
    []
racialData ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGStrength)) =
    []
racialData ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGDexterity)) =
    []
racialData ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGConstitution)) =
    []
racialData ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfVariant HalfElfDMGConstitution HalfElfDMGWisdom)) =
    []
racialData ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGStrength)) =
    []
racialData ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGDexterity)) =
    []
racialData ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGConstitution)) =
    []
racialData ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfVariant HalfElfDMGIntelligence HalfElfDMGWisdom)) =
    []
racialData ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGStrength)) =
    []
racialData ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGDexterity)) =
    []
racialData ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGConstitution)) =
    []
racialData ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGIntelligence)) =
    []
racialData ((HalfElfVariant HalfElfDMGWisdom HalfElfDMGWisdom)) =
    []
racialData ((HalfElfDetection HalfElfDetectionStrength)) =
    []
racialData ((HalfElfDetection HalfElfDetectionDexterity)) =
    []
racialData ((HalfElfDetection HalfElfDetectionConstitution)) =
    []
racialData ((HalfElfDetection HalfElfDetectionIntelligence)) =
    []
racialData ((HalfElfDetection HalfElfDetectionCharisma)) =
    []
racialData (HalfElfStorm) =
    []

racialData (HalfOrcStandard) =
    []
racialData (HalfOrcFinding) =
    []

racialData (HalflingLightfoot) =
    []
racialData (HalflingStout) =
    []
racialData (HalflingGhostwise) =
    []
racialData (HalflingHealing) =
    []
racialData (HalflingHospitality) =
    []

racialData (Hobgoblin) =
    []

racialData (HumanStandard) =
    []

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

racialData (HumanFinding) =
    []
racialData ((HumanHandling HumanHandlingStrength)) =
    []
racialData ((HumanHandling HumanHandlingDexterity)) =
    []
racialData ((HumanHandling HumanHandlingConstitution)) =
    []
racialData ((HumanHandling HumanHandlingIntelligence)) =
    []
racialData ((HumanHandling HumanHandlingCharisma)) =
    []
racialData ((HumanMaking HumanMakingStrength)) =
    []
racialData ((HumanMaking HumanMakingDexterity)) =
    []
racialData ((HumanMaking HumanMakingConstitution)) =
    []
racialData ((HumanMaking HumanMakingWisdom)) =
    []
racialData ((HumanMaking HumanMakingCharisma)) =
    []
racialData ((HumanPassage HumanPassageStrength)) =
    []
racialData ((HumanPassage HumanPassageConstitution)) =
    []
racialData ((HumanPassage HumanPassageIntelligence)) =
    []
racialData ((HumanPassage HumanPassageWisdom)) =
    []
racialData ((HumanPassage HumanPassageCharisma)) =
    []
racialData (HumanSentinel) =
    []

racialData (Kalashtar) =
    []

racialData (Kenku) =
    []

racialData (Kobold) =
    []

racialData (Leonin) =
    []

racialData (Lizardfolk) =
    []

racialData (Locathah) =
    []

racialData (Loxodon) =
    []

racialData (Minotaur) =
    []

racialData (OrcStandard) =
    []
racialData (OrcEberron) =
    []

racialData (Satyr) =
    []

racialData (ShifterBeasthide) =
    []
racialData (ShifterLongtooth) =
    []
racialData (ShifterSwiftstride) =
    []
racialData (ShifterWildhunt) =
    []

racialData (SimicStrength) =
    []
racialData (SimicDexterity) =
    []
racialData (SimicIntelligence) =
    []
racialData (SimicWisdom) =
    []
racialData (SimicCharisma) =
    []

racialData (Tabaxi) =
    []

racialData ((TieflingNormal TieflingNormalDevilsTongue)) =
    []
racialData ((TieflingNormal TieflingNormalHellfire)) =
    []
racialData ((TieflingNormal TieflingNormalWinged)) =
    []
racialData ((TieflingNormal TieflingNormalAsmodeus)) =
    []
racialData ((TieflingNormal TieflingNormalBaalzebul)) =
    []
racialData ((TieflingNormal TieflingNormalDispater)) =
    []
racialData ((TieflingNormal TieflingNormalFierna)) =
    []
racialData ((TieflingNormal TieflingNormalGlasya)) =
    []
racialData ((TieflingNormal TieflingNormalLevistus)) =
    []
racialData ((TieflingNormal TieflingNormalMammon)) =
    []
racialData ((TieflingNormal TieflingNormalMephistopheles)) =
    []
racialData ((TieflingNormal TieflingNormalZariel)) =
    []

racialData ((TieflingFeral TieflingFeralDevilsTongue)) =
    []
racialData ((TieflingFeral TieflingFeralHellfire)) =
    []
racialData ((TieflingFeral TieflingFeralWinged)) =
    []

racialData (Tortle) =
    []

racialData (Triton) =
    []
  
racialData (Vedalken) =
    []
  
racialData (WarforgedStrength) =
    []
racialData (WarforgedDexterity) =
    []
racialData (WarforgedIntelligence) =
    []
racialData (WarforgedWisdom) =
    []
racialData (WarforgedCharisma) =
    []

racialData (YuanTiPureblood) =
    []
