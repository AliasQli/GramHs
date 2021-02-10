{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad
import Data.Either
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative.App.Simple
import Gui.Update
import Gui.View
import Net.Http
import Net.Ws
import Type

main :: IO ()
main = void $ do
  st <- runHttp getInitState
  when (isLeft st) $ do
    putTextLn $ fromLeft "" st
    mzero
  let Right state = st
  runWs
    ( \conn -> do
        Gtk.init Nothing

        Just screen <- Gdk.screenGetDefault
        provider <- Gtk.cssProviderNew
        Gtk.cssProviderLoadFromData provider css
        Gtk.styleContextAddProviderForScreen screen provider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER

        forkIO $ do
          runLoop $
            App
              { view = view'
              , update = update'
              , inputs = [eventPipe conn]
              , initialState = state
              }
          Gtk.mainQuit
        Gtk.main
    )
