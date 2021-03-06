{-# LANGUAGE OverloadedLists #-}

module Main where

import           Control.Concurrent            (forkIO)
import           Control.Monad
import           Data.Either
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.App.Simple
import           Gui.Update
import           Gui.View
import           Net.Http
import           Net.Ws
import           Type

main :: IO ()
main = void $ do
  st <- runHttp getInitState
  when (isLeft st) $ do
    putTextLn $ fromLeft "" st
    mzero
  let state = fromRight (error "Init failed.") st
  runWs (\conn -> do
      _ <- Gtk.init Nothing

      Just screen <- Gdk.screenGetDefault
      provider <- Gtk.cssProviderNew
      Gtk.cssProviderLoadFromData provider css
      Gtk.styleContextAddProviderForScreen screen provider $
        fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER

      _ <- forkIO $ do
        _ <- runLoop $ App
          { view = view'
          , update = update'
          , inputs = [eventPipe conn]
          , initialState = state
          }
        Gtk.mainQuit
      Gtk.main
    )
