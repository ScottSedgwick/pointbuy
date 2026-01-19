{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Main where

import Data.Default                ( def )
import qualified Data.Map.Strict as M
import Miso                        ( App, Component(..), component, run, startApp )

import App.Main (Action, Model, updateModel, viewModel)

----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp M.empty app)

----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = (component def updateModel viewModel) { initialAction = Nothing }



-- runJS :: JSString -> Transition Model Action
-- runJS js = io_ (eval js >> return ())