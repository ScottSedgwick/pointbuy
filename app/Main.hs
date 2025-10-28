{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Main where

import Control.Lens                ( (&), (.~), (.=), (?=) )
import Data.Default                ( def )
import Data.Either                 ( either )
import Data.Maybe                  ( maybe )
import Data.List                   ( drop, find )
import Language.Javascript.JSaddle ( JSString, eval )
import Miso                        ( App, JSM, MisoString, Transition, (<#), component, fromMisoString, io, io_, issue, run, startApp )
import Miso.Fetch                  ( Response(..), getJSON )
import Miso.FFI                    ( consoleError, consoleLog )
import Miso.State                  ( get, modify, put )
import Miso.String                 ( fromMisoStringEither, ms )
import Miso.Subscription.History   ( URI(..), getURI, pushURI )
import Miso.Types                  ( Component(..) )
import Text.Read                   ( readMaybe )

import qualified Data.Map.Strict   as M

import Types                       ( Action(..), Model(..), StateData, race, racialBonuses, stateData )
import Types.Encoding              ( decode, encode )
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
app = (component def updateModel viewModel) { initialAction = Just LoadModel }

----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel (ChangeInt l a)  = l .= (getIntDef 0 a)                   >> issue SaveModel
updateModel (ChangeRace s)   = setRace (readMaybe (fromMisoString s)) >> issue SaveModel
updateModel (ChangeTab l a)  = l .= a                                 >> issue SaveModel
updateModel (ChangeTitle s)  = runJS $ "document.title = '" <> s <> "';"
updateModel LoadModel        = loadModel
updateModel (Log s)          = io_ $ consoleLog (ms s)
updateModel Reset            = def <# (pure $ ChangeTitle "D&D 5e Point Buy Calculator")
updateModel SaveModel        = saveModel
updateModel (SetModel m)     = put m
updateModel FetchData        = getJSON "data/data.json" [] SetData ErrorHandler
updateModel (SetData r)      = put (body r)
updateModel (ErrorHandler r) = io_ $ consoleError (maybe "" id (errorMessage r))

getIntDef :: Int -> MisoString -> Int
getIntDef d a = either (const d) id (fromMisoStringEither a)

-- This next function's hack job is because the Miso.Subscription.History module does not correctly decode parameters.
-- It stores the key *and* the data in the key, and Nothing in the data.
loadModel :: Transition Model Action
loadModel = do
    m <- get
    io $ do
        uri <- getURI 
        pure $ case (find (startsWith "data=") (map fromMisoString $ M.keys (uriQueryString uri))) of
            Nothing -> Log "Could not locate data parameter"
            Just s  -> 
                case (decode (drop 5 s) :: Maybe StateData) of
                    Nothing -> Log "Failed to decode data"
                    Just sd -> SetModel (m { _stateData = sd } )

saveModel :: Transition Model Action
saveModel = do
    m <- get  
    io_ $ do
        uri <- getURI 
        pushURI $ uri { uriQueryString = M.fromList [ ("data", (Just $ encode (_stateData m))) ] }

setRace :: Maybe Race -> Transition Model Action
setRace = maybe 
            (pure ()) 
            (\r -> modify $ \m -> m & (stateData . race) .~ r & (stateData . racialBonuses) .~ (defaultRacialBonuses r))

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith []     _      = True
startsWith xs     []     = False
startsWith (x:xs) (y:ys) = (x == y) && startsWith xs ys

runJS :: JSString -> Transition Model Action
runJS js = io_ (eval js >> return ())