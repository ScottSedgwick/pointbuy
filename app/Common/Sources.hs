module Common.Sources
  ( Source(..)
  , allSources
  ) where

import Data.Maybe    ( Maybe(..) )
import Common.Unshow ( Unshow, unshow )

data Source
  = SourceAcquisitionsIncorporated
  | SourceBaldursGateDescentIntoAvernus
  | SourceBookOfManyThings
  | SourceBigbyPresentsGloryOfTheGiants
  | SourceCandlekeepMysteries
  | SourceCurseOfStrahd
  | SourceCriticalRoleCallOfNetherdeep
  | SourceCriticalRoleTwitter
  | SourceDivineContention
  | SourceDMG
  | SourceDragonlanceShadowOfTheDragonQueen
  | SourceEberronRisingFromTheLastWar
  | SourceExplorersGuideToWildemount
  | SourceFizbansTreasuryOfDragons
  | SourceGuildmastersGuideToRavnica
  | SourceGhostsOfSaltmarsh
  | SourceDungeonsAndDragonsHonorAmongThieves
  | SourceIcewindDaleRimeOfTheFrostmaiden
  | SourceInfernalMachineRebuild
  | SourceJourneysThroughTheRadiantCitadel
  | SourceKeysFromTheGoldenVault
  | SourceLostLaboratoryOfKwalish
  | SourceLostMineOfPhandelver
  | SourceMonstrousCompendium2
  | SourceMordenkainenPresentsMonstersOfTheMultiverse
  | SourceMulmasterBondsAndBackgrounds
  | SourceMythicOdysseysOfTheros
  | SourceOutOfTheAbyss
  | SourcePhandelverAndBelowTheShatteredObelisk
  | SourcePlanescapeAdventuresInTheMultiverse
  | SourcePlaneShiftAmonkhet
  | SourcePlaneShiftInnistrad
  | SourceQuestsFromTheInfiniteStaircase
  | SourcePrincesOfTheApocalypse
  | SourceTheRiseOfTiamat
  | SourceSpelljammerAdventuresInSpace
  | SourceStateOfHillsfar
  | SourceStrixhavenCurriculumOfChaos
  | SourceSleepingDragonsWake
  | SourceStormKingsThunder
  | SourceSwordCoastAdventurersGuide
  | SourceTashasCauldronOfEverything
  | SourceTombOfAnnihilation
  | SourceTyrannyOfDragons
  | SourceTalesFromTheYawningPortal
  | SourceVanRichtensGuideToRavenloft
  | SourceVecnaEyeOfRuin
  | SourceVolosGuideToMonsters
  | SourceWaterdeepDragonHeist
  | SourceWaterdeepDungeonOfTheMadMage
  | SourceWayfarersGuideToEberron
  | SourceTheWildBeyondTheWitchlight
  | SourceXanatharsGuideToEverything
  | SourcePlayersHandbook
  | SourceHitPointPressHumblewoodCampaignSetting
  | SourceTalDoreiCampaignSettingReborn
  | SourceSigilAndTheOutlands
  | SourceElementalEvilPlayersCompanion
  | SourceUnknown String

instance Show Source where
  show SourceAcquisitionsIncorporated = "Acquisitions Incorporated"
  show SourceBaldursGateDescentIntoAvernus = "Baldur's Gate: Descent into Avernus"
  show SourceBookOfManyThings = "The Book of Many Things"
  show SourceBigbyPresentsGloryOfTheGiants = "Bigby Presents: Glory of the Giants"
  show SourceCandlekeepMysteries = "Candlekeep Mysteries"
  show SourceCurseOfStrahd = "Curse of Strahd"
  show SourceCriticalRoleCallOfNetherdeep = "Critical Role: Call of the Netherdeep"
  show SourceCriticalRoleTwitter = "Critical Role: Twitter"
  show SourceDivineContention = "Divine Contention"
  show SourceDMG = "Dungeon Master's Guide"
  show SourceDragonlanceShadowOfTheDragonQueen = "Dragonlance: Shadow of the Dragon Queen"
  show SourceEberronRisingFromTheLastWar = "Eberron: Rising from the Last War"
  show SourceExplorersGuideToWildemount = "Explorer's Guide to Wildemount"
  show SourceFizbansTreasuryOfDragons = "Fizban's Treasury of Dragons"
  show SourceGuildmastersGuideToRavnica = "Guildmaster's Guide to Ravnica"
  show SourceGhostsOfSaltmarsh = "Ghosts of Saltmarsh"
  show SourceDungeonsAndDragonsHonorAmongThieves = "Dungeons and Dragons: Honor Among Thieves"
  show SourceIcewindDaleRimeOfTheFrostmaiden = "Icewind Dale: Rime of the Frostmaiden"
  show SourceInfernalMachineRebuild = "Infernal Machine Rebuild"
  show SourceJourneysThroughTheRadiantCitadel = "Journeys through the Radiant Citadel"
  show SourceKeysFromTheGoldenVault = "Keys from the Golden Vault"
  show SourceLostLaboratoryOfKwalish = "Lost Laboratory of Kwalish"
  show SourceLostMineOfPhandelver = "Lost Mine of Phandelver"
  show SourceMonstrousCompendium2 = "Monstrous Compendium Volume 2 - Dragonlance Creatures"
  show SourceMordenkainenPresentsMonstersOfTheMultiverse = "Mordenkainen Presents: Monsters of the Multiverse"
  show SourceMulmasterBondsAndBackgrounds = "Mulmaster Bonds and Backgrounds"
  show SourceMythicOdysseysOfTheros = "Mythic Odysseys of Theros"
  show SourceOutOfTheAbyss = "Out of the Abyss"
  show SourcePhandelverAndBelowTheShatteredObelisk = "Phandelver and Below: The Shattered Obelisk"
  show SourcePlanescapeAdventuresInTheMultiverse = "Planescape: Adventures in the Multiverse"
  show SourcePlaneShiftAmonkhet = "Plane Shift: Amonkhet"
  show SourcePlaneShiftInnistrad = "Plane Shift: Innistrad"
  show SourceQuestsFromTheInfiniteStaircase = "Quests from the Infinite Staircase"
  show SourcePrincesOfTheApocalypse = "Princes of the Apocalypse"
  show SourceTheRiseOfTiamat = "The Rise of Tiamat"
  show SourceSpelljammerAdventuresInSpace = "Spelljammer: Adventures in Space"
  show SourceStrixhavenCurriculumOfChaos = "Strixhaven: A Curriculum of Chaos"
  show SourceSleepingDragonsWake = "Sleeping Dragon's Wake"
  show SourceStateOfHillsfar = "State of Hillsfar"
  show SourceStormKingsThunder = "Storm King's Thunder"
  show SourceSwordCoastAdventurersGuide = "Sword Coast Adventurers Guide"
  show SourceTashasCauldronOfEverything = "Tasha's Cauldron of Everything"
  show SourceTombOfAnnihilation = "Tomb of Annihilation"
  show SourceTyrannyOfDragons = "Tyranny of Dragons"
  show SourceTalesFromTheYawningPortal = "Tales from the Yawning Portal"
  show SourceVanRichtensGuideToRavenloft = "Van Richten's Guide to Ravenloft"
  show SourceVecnaEyeOfRuin = "Vecna: Eve of Ruin"
  show SourceVolosGuideToMonsters = "Volo's Guide to Monsters"
  show SourceWaterdeepDragonHeist = "Waterdeep: Dragon Heist"
  show SourceWaterdeepDungeonOfTheMadMage = "Waterdeep: Dungeon of the Mad Mage"
  show SourceWayfarersGuideToEberron = "Wayfarer's Guide to Eberron"
  show SourceTheWildBeyondTheWitchlight = "The Wild Beyond the Witchlight"
  show SourceXanatharsGuideToEverything = "Xanathar's Guide to Everything"
  show SourcePlayersHandbook = "Players Handbook"
  show SourceHitPointPressHumblewoodCampaignSetting = "Hit Point Press: Humblewood Campaign Setting"
  show SourceTalDoreiCampaignSettingReborn = "Tal'Dorei Campaign Setting Reborn"
  show SourceSigilAndTheOutlands = "Sigil and the Outlands"
  show SourceElementalEvilPlayersCompanion = "Elemental Evil Player's Companion"
  show (SourceUnknown s) = s

instance Eq Source where
  (==) SourceAcquisitionsIncorporated SourceAcquisitionsIncorporated = True
  (==) SourceBaldursGateDescentIntoAvernus SourceBaldursGateDescentIntoAvernus = True
  (==) SourceBookOfManyThings SourceBookOfManyThings = True
  (==) SourceBigbyPresentsGloryOfTheGiants SourceBigbyPresentsGloryOfTheGiants = True
  (==) SourceCandlekeepMysteries SourceCandlekeepMysteries = True
  (==) SourceCurseOfStrahd SourceCurseOfStrahd = True
  (==) SourceCriticalRoleCallOfNetherdeep SourceCriticalRoleCallOfNetherdeep = True
  (==) SourceCriticalRoleTwitter SourceCriticalRoleTwitter = True
  (==) SourceDivineContention SourceDivineContention = True
  (==) SourceDMG SourceDMG = True
  (==) SourceDragonlanceShadowOfTheDragonQueen SourceDragonlanceShadowOfTheDragonQueen = True
  (==) SourceEberronRisingFromTheLastWar SourceEberronRisingFromTheLastWar = True
  (==) SourceExplorersGuideToWildemount SourceExplorersGuideToWildemount = True
  (==) SourceFizbansTreasuryOfDragons SourceFizbansTreasuryOfDragons = True
  (==) SourceGuildmastersGuideToRavnica SourceGuildmastersGuideToRavnica = True
  (==) SourceGhostsOfSaltmarsh SourceGhostsOfSaltmarsh = True
  (==) SourceDungeonsAndDragonsHonorAmongThieves SourceDungeonsAndDragonsHonorAmongThieves = True
  (==) SourceIcewindDaleRimeOfTheFrostmaiden SourceIcewindDaleRimeOfTheFrostmaiden = True
  (==) SourceInfernalMachineRebuild SourceInfernalMachineRebuild = True
  (==) SourceJourneysThroughTheRadiantCitadel SourceJourneysThroughTheRadiantCitadel = True
  (==) SourceKeysFromTheGoldenVault SourceKeysFromTheGoldenVault = True
  (==) SourceLostLaboratoryOfKwalish SourceLostLaboratoryOfKwalish = True
  (==) SourceLostMineOfPhandelver SourceLostMineOfPhandelver = True
  (==) SourceMonstrousCompendium2 SourceMonstrousCompendium2 = True
  (==) SourceMordenkainenPresentsMonstersOfTheMultiverse SourceMordenkainenPresentsMonstersOfTheMultiverse = True
  (==) SourceMulmasterBondsAndBackgrounds SourceMulmasterBondsAndBackgrounds = True
  (==) SourceMythicOdysseysOfTheros SourceMythicOdysseysOfTheros = True
  (==) SourceOutOfTheAbyss SourceOutOfTheAbyss = True
  (==) SourcePhandelverAndBelowTheShatteredObelisk SourcePhandelverAndBelowTheShatteredObelisk = True
  (==) SourcePlanescapeAdventuresInTheMultiverse SourcePlanescapeAdventuresInTheMultiverse = True
  (==) SourcePlaneShiftAmonkhet SourcePlaneShiftAmonkhet = True
  (==) SourcePlaneShiftInnistrad SourcePlaneShiftInnistrad = True
  (==) SourceQuestsFromTheInfiniteStaircase SourceQuestsFromTheInfiniteStaircase = True
  (==) SourcePrincesOfTheApocalypse SourcePrincesOfTheApocalypse = True
  (==) SourceTheRiseOfTiamat SourceTheRiseOfTiamat = True
  (==) SourceSpelljammerAdventuresInSpace SourceSpelljammerAdventuresInSpace = True
  (==) SourceStateOfHillsfar SourceStateOfHillsfar = True
  (==) SourceStrixhavenCurriculumOfChaos SourceStrixhavenCurriculumOfChaos = True
  (==) SourceSleepingDragonsWake SourceSleepingDragonsWake = True
  (==) SourceStormKingsThunder SourceStormKingsThunder = True
  (==) SourceSwordCoastAdventurersGuide SourceSwordCoastAdventurersGuide = True
  (==) SourceTashasCauldronOfEverything SourceTashasCauldronOfEverything = True
  (==) SourceTombOfAnnihilation SourceTombOfAnnihilation = True
  (==) SourceTyrannyOfDragons SourceTyrannyOfDragons = True
  (==) SourceTalesFromTheYawningPortal SourceTalesFromTheYawningPortal = True
  (==) SourceVanRichtensGuideToRavenloft SourceVanRichtensGuideToRavenloft = True
  (==) SourceVecnaEyeOfRuin SourceVecnaEyeOfRuin = True
  (==) SourceVolosGuideToMonsters SourceVolosGuideToMonsters = True
  (==) SourceWaterdeepDragonHeist SourceWaterdeepDragonHeist = True
  (==) SourceWaterdeepDungeonOfTheMadMage SourceWaterdeepDungeonOfTheMadMage = True
  (==) SourceWayfarersGuideToEberron SourceWayfarersGuideToEberron = True
  (==) SourceTheWildBeyondTheWitchlight SourceTheWildBeyondTheWitchlight = True
  (==) SourceXanatharsGuideToEverything SourceXanatharsGuideToEverything = True
  (==) SourcePlayersHandbook SourcePlayersHandbook = True
  (==) SourceHitPointPressHumblewoodCampaignSetting SourceHitPointPressHumblewoodCampaignSetting = True
  (==) SourceTalDoreiCampaignSettingReborn SourceTalDoreiCampaignSettingReborn = True
  (==) SourceSigilAndTheOutlands SourceSigilAndTheOutlands = True
  (==) SourceElementalEvilPlayersCompanion SourceElementalEvilPlayersCompanion = True
  (==) (SourceUnknown a) (SourceUnknown b) = a == b
  (==) _ _ = False

instance Ord Source where
  compare a b = compare (show a) (show b)

allSources :: [ Source ]
allSources =
  [ SourceAcquisitionsIncorporated
  , SourceBaldursGateDescentIntoAvernus
  , SourceBookOfManyThings
  , SourceBigbyPresentsGloryOfTheGiants
  , SourceCandlekeepMysteries
  , SourceCurseOfStrahd
  , SourceCriticalRoleCallOfNetherdeep
  , SourceCriticalRoleTwitter
  , SourceDivineContention
  , SourceDMG
  , SourceDragonlanceShadowOfTheDragonQueen
  , SourceEberronRisingFromTheLastWar
  , SourceExplorersGuideToWildemount
  , SourceFizbansTreasuryOfDragons
  , SourceGuildmastersGuideToRavnica
  , SourceGhostsOfSaltmarsh
  , SourceDungeonsAndDragonsHonorAmongThieves
  , SourceIcewindDaleRimeOfTheFrostmaiden
  , SourceInfernalMachineRebuild
  , SourceJourneysThroughTheRadiantCitadel
  , SourceKeysFromTheGoldenVault
  , SourceLostLaboratoryOfKwalish
  , SourceLostMineOfPhandelver
  , SourceMonstrousCompendium2
  , SourceMordenkainenPresentsMonstersOfTheMultiverse
  , SourceMulmasterBondsAndBackgrounds
  , SourceMythicOdysseysOfTheros
  , SourceOutOfTheAbyss
  , SourcePhandelverAndBelowTheShatteredObelisk
  , SourcePlanescapeAdventuresInTheMultiverse
  , SourcePlaneShiftAmonkhet
  , SourcePlaneShiftInnistrad
  , SourceQuestsFromTheInfiniteStaircase
  , SourcePrincesOfTheApocalypse
  , SourceTheRiseOfTiamat
  , SourceSpelljammerAdventuresInSpace
  , SourceStateOfHillsfar
  , SourceStrixhavenCurriculumOfChaos
  , SourceSleepingDragonsWake
  , SourceStateOfHillsfar
  , SourceStormKingsThunder
  , SourceSwordCoastAdventurersGuide
  , SourceTashasCauldronOfEverything
  , SourceTombOfAnnihilation
  , SourceTyrannyOfDragons
  , SourceTalesFromTheYawningPortal
  , SourceVanRichtensGuideToRavenloft
  , SourceVecnaEyeOfRuin
  , SourceVolosGuideToMonsters
  , SourceWaterdeepDragonHeist
  , SourceWaterdeepDungeonOfTheMadMage
  , SourceWayfarersGuideToEberron
  , SourceTheWildBeyondTheWitchlight
  , SourceXanatharsGuideToEverything
  , SourcePlayersHandbook
  , SourceHitPointPressHumblewoodCampaignSetting
  , SourceTalDoreiCampaignSettingReborn
  , SourceSigilAndTheOutlands
  , SourceElementalEvilPlayersCompanion
  ]

instance Unshow Source where
  unshow "Acquisitions Incorporated" = Just SourceAcquisitionsIncorporated
  unshow "Baldur's Gate: Descent into Avernus" = Just SourceBaldursGateDescentIntoAvernus
  unshow "The Book of Many Things" = Just SourceBookOfManyThings
  unshow "Bigby Presents: Glory of the Giants" = Just SourceBigbyPresentsGloryOfTheGiants
  unshow "Candlekeep Mysteries" = Just SourceCandlekeepMysteries
  unshow "Curse of Strahd" = Just SourceCurseOfStrahd
  unshow "Critical Role: Call of the Netherdeep" = Just SourceCriticalRoleCallOfNetherdeep
  unshow "Critical Role: Twitter" = Just SourceCriticalRoleTwitter
  unshow "Divine Contention" = Just SourceDivineContention
  unshow "Dungeon Master's Guide" = Just SourceDMG
  unshow "Dragonlance: Shadow of the Dragon Queen" = Just SourceDragonlanceShadowOfTheDragonQueen
  unshow "Eberron: Rising from the Last War" = Just SourceEberronRisingFromTheLastWar
  unshow "Explorer's Guide to Wildemount" = Just SourceExplorersGuideToWildemount
  unshow "Fizban's Treasury of Dragons" = Just SourceFizbansTreasuryOfDragons
  unshow "Guildmaster's Guide to Ravnica" = Just SourceGuildmastersGuideToRavnica
  unshow "Ghosts of Saltmarsh" = Just SourceGhostsOfSaltmarsh
  unshow "Dungeons and Dragons: Honor Among Thieves" = Just SourceDungeonsAndDragonsHonorAmongThieves
  unshow "Icewind Dale: Rime of the Frostmaiden" = Just SourceIcewindDaleRimeOfTheFrostmaiden
  unshow "Infernal Machine Rebuild" = Just SourceInfernalMachineRebuild
  unshow "Journeys through the Radiant Citadel" = Just SourceJourneysThroughTheRadiantCitadel
  unshow "Keys from the Golden Vault" = Just SourceKeysFromTheGoldenVault
  unshow "Lost Laboratory of Kwalish" = Just SourceLostLaboratoryOfKwalish
  unshow "Lost Mine of Phandelver" = Just SourceLostMineOfPhandelver
  unshow "Monstrous Compendium Volume 2 - Dragonlance Creatures" = Just SourceMonstrousCompendium2
  unshow "Mordenkainen Presents: Monsters of the Multiverse" = Just SourceMordenkainenPresentsMonstersOfTheMultiverse
  unshow "Mulmaster Bonds and Backgrounds" = Just SourceMulmasterBondsAndBackgrounds
  unshow "Mythic Odysseys of Theros" = Just SourceMythicOdysseysOfTheros
  unshow "Out of the Abyss" = Just SourceOutOfTheAbyss
  unshow "Phandelver and Below: The Shattered Obelisk" = Just SourcePhandelverAndBelowTheShatteredObelisk
  unshow "Planescape: Adventures in the Multiverse" = Just SourcePlanescapeAdventuresInTheMultiverse
  unshow "Plane Shift: Amonkhet" = Just SourcePlaneShiftAmonkhet
  unshow "Plane Shift: Innistrad" = Just SourcePlaneShiftInnistrad
  unshow "Quests from the Infinite Staircase" = Just SourceQuestsFromTheInfiniteStaircase
  unshow "Princes of the Apocalypse" = Just SourcePrincesOfTheApocalypse
  unshow "The Rise of Tiamat" = Just SourceTheRiseOfTiamat
  unshow "Spelljammer: Adventures in Space" = Just SourceSpelljammerAdventuresInSpace
  unshow "State of Hillsfar" = Just SourceStateOfHillsfar
  unshow "Strixhaven: A Curriculum of Chaos" = Just SourceStrixhavenCurriculumOfChaos
  unshow "Sleeping Dragon's Wake" = Just SourceSleepingDragonsWake
  unshow "Storm King's Thunder" = Just SourceStormKingsThunder
  unshow "Sword Coast Adventurers Guide" = Just SourceSwordCoastAdventurersGuide
  unshow "Tasha's Cauldron of Everything" = Just SourceTashasCauldronOfEverything
  unshow "Tomb of Annihilation" = Just SourceTombOfAnnihilation
  unshow "Tyranny of Dragons" = Just SourceTyrannyOfDragons
  unshow "Tales from the Yawning Portal" = Just SourceTalesFromTheYawningPortal
  unshow "Van Richten's Guide to Ravenloft" = Just SourceVanRichtensGuideToRavenloft
  unshow "Vecna: Eve of Ruin" = Just SourceVecnaEyeOfRuin
  unshow "Volo's Guide to Monsters" = Just SourceVolosGuideToMonsters
  unshow "Waterdeep: Dragon Heist" = Just SourceWaterdeepDragonHeist
  unshow "Waterdeep: Dungeon of the Mad Mage" = Just SourceWaterdeepDungeonOfTheMadMage
  unshow "Wayfarer's Guide to Eberron" = Just SourceWayfarersGuideToEberron
  unshow "The Wild Beyond the Witchlight" = Just SourceTheWildBeyondTheWitchlight
  unshow "Xanathar's Guide to Everything" = Just SourceXanatharsGuideToEverything
  unshow "Players Handbook" = Just SourcePlayersHandbook
  unshow "Hit Point Press: Humblewood Campaign Setting" = Just SourceHitPointPressHumblewoodCampaignSetting
  unshow "Tal'Dorei Campaign Setting Reborn" = Just SourceTalDoreiCampaignSettingReborn
  unshow "Sigil and the Outlands" = Just SourceSigilAndTheOutlands
  unshow "Elemental Evil Player's Companion" = Just SourceElementalEvilPlayersCompanion
  unshow "All" = Nothing
  unshow s = Just (SourceUnknown s)