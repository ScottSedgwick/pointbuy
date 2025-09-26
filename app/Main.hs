{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Main where

import Control.Lens                ( (&), (.~), (.=) )
import Data.Default                ( def )
import Data.Either                 ( either )
import Data.Maybe                  ( maybe )
import Language.Javascript.JSaddle ( JSString, eval )
import Miso                        ( App, MisoString, Transition, (<#), component, fromMisoString, io_, run, startApp )
import Miso.State                  ( modify )
import Miso.String                 ( fromMisoStringEither )
import Text.Read                   ( readMaybe )

import Types                       ( Action(..), Model, race, racialBonuses )
import Types.Races                 ( Race, defaultRacialBonuses )
import View.Main                   ( viewModel )

----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp app)

----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component def updateModel viewModel

----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel Reset           = def <# pure (ChangeTitle "D&D 5e Point Buy Calculator")    -- example of chaining events
updateModel (ChangeTitle s) = runJS $ "document.title = '" <> s <> "';"
updateModel (ChangeTab l a) = l .= a
updateModel (ChangeInt l a) = l .= (getIntDef 0 a)
updateModel (ChangeRace s)  = setRace (readMaybe (fromMisoString s))

runJS :: JSString -> Transition Model Action
runJS js = io_ (eval js >> return ())

getIntDef :: Int -> MisoString -> Int
getIntDef d a = either (const d) id (fromMisoStringEither a)

setRace :: Maybe Race -> Transition Model Action
setRace Nothing  = pure ()
setRace (Just r) = modify $ \m -> m & race .~ r & racialBonuses .~ (defaultRacialBonuses r)