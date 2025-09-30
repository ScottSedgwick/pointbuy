{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Main where

import Control.Lens                ( (&), (.~), (.=) )
import Data.Default                ( def )
import Data.Either                 ( either )
import Data.Maybe                  ( maybe )
import Data.List                   ( drop, find )
import Language.Javascript.JSaddle ( JSString, eval )
import Miso                        ( App, JSM, MisoString, Transition, (<#), component, fromMisoString, io, io_, run, startApp )
import Miso.FFI                    ( consoleLog )
import Miso.State                  ( get, modify, put )
import Miso.String                 ( fromMisoStringEither, ms )
import Miso.Subscription.History   ( URI(..), getURI, pushURI )
import Miso.Types                  ( Component(..) )
import Text.Read                   ( readMaybe )

import qualified Data.Map.Strict   as M

import Types                       ( Action(..), Model, race, racialBonuses )
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
updateModel Reset           = def <# pure (ChangeTitle "D&D 5e Point Buy Calculator")    -- example of chaining events
updateModel (ChangeTitle s) = runJS $ "document.title = '" <> s <> "';"
updateModel (ChangeTab l a) = l .= a
updateModel (ChangeInt l a) = l .= (getIntDef 0 a)
updateModel (ChangeRace s)  = setRace (readMaybe (fromMisoString s))
updateModel LoadModel       = loadModel
updateModel (Log s)         = io_ $ consoleLog (ms s)
updateModel SaveModel       = saveModel
updateModel (SetModel m)    = put m
updateModel Test            = do
    m <- get
    io_ $ do
        consoleLog (ms $ show m)
        let mstr = encode m
        consoleLog (mstr)
        case (decode (fromMisoString mstr) :: Maybe Model) of
            Nothing -> consoleLog "Failed to decode"
            Just m2 -> consoleLog (ms $ show m2)
        uri <- getURI
        pushURI $ uri { uriQueryString = M.insert "data" (Just mstr) (uriQueryString uri) }

getIntDef :: Int -> MisoString -> Int
getIntDef d a = either (const d) id (fromMisoStringEither a)

loadModel :: Transition Model Action
loadModel = io $ do
    uri <- getURI
    -- This next line hack job is because the Miso.Subscription.History module does not correctly decode parameters.
    -- It stores the key *and* the data in the key, and Nothing in the data.
    case (find (startsWith "data=") (map fromMisoString $ M.keys (uriQueryString uri))) of
        Nothing -> pure $ Log "Could not locate data parameter"
        Just s  -> 
            case (decode (drop 5 s) :: Maybe Model) of
                Nothing -> pure $ Log "Failed to decode data"
                Just m  -> pure $ SetModel m

saveModel :: Transition Model Action
saveModel = get >>= \m -> io_ $ getURI >>= \uri -> pushURI $ uri { uriQueryString = M.fromList [ ("data", (Just $ encode m)) ] }

setRace :: Maybe Race -> Transition Model Action
setRace Nothing  = pure ()
setRace (Just r) = modify $ \m -> m & race .~ r & racialBonuses .~ (defaultRacialBonuses r)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith []     _      = True
startsWith xs     []     = False
startsWith (x:xs) (y:ys) = (x == y) && startsWith xs ys

runJS :: JSString -> Transition Model Action
runJS js = io_ (eval js >> return ())