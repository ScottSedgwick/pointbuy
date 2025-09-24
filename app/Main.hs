----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Data.Default
import           Data.Either (either)
import           Data.Maybe (maybe)
import           Language.Javascript.JSaddle (eval)
import qualified Language.Javascript.JSaddle.Wasm as JSW -- (run)
import           Miso
import           Miso.Lens
import           Miso.State
import           Miso.String
import           Text.Read ( readMaybe )

import           Types
import           Types.Races
import           View.Main (viewModel)

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
updateModel (ChangeTitle s)   = runJS $ "document.title = '" <> s <> "';"
updateModel (ChangeTab l a)   = l .= a
updateModel (ChangeInt l a)   = l .= (getIntDef 0 a)
updateModel ResetCustomValues = modify (const def)
updateModel (ChangeRace s)    = do
  case (readMaybe (fromMisoString s)) of
    Nothing -> io_ $ consoleLog "Unable to parse race string"
    Just r -> do
      modify $ \m -> m & race .~ r & racialBonuses .~ (defaultRacialBonuses r)
      -- pure $ ChangeTitle "Well?"

runJS :: JSString -> Transition Model Action
runJS js = io_ (eval js >> return ())

getIntDef :: Int -> MisoString -> Int
getIntDef d a = either (const d) id (fromMisoStringEither a)