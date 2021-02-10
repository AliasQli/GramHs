{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gui.CustomInput where

import Control.Monad
import Data.GI.Base.Attributes (clear)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified GI.GObject as GI
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource (fromCancellation)

data Command
  = Sended
  | SendedAndClear
  | AtMember Int
  deriving (Show, Eq)

data InputBoxProperties = InputBoxProperties
  { version :: Integer
  , command :: Command
  }
  deriving (Show, Eq)

newtype InputBoxEvent = InputBoxSend Text

data InputBoxReferences = InputBoxReferences
  { ver :: Integer
  , tv :: Gtk.TextView
  , btn :: Gtk.Button
  , img :: Gtk.FileChooserButton
  }

setSend :: InputBoxReferences -> IO ()
setSend InputBoxReferences{..} = do
  Gtk.set tv [#editable Gtk.:= True]
  Gtk.set btn [#sensitive Gtk.:= True]
  sc <- Gtk.widgetGetStyleContext btn
  Gtk.styleContextAddClass sc "send-button"
  Gtk.styleContextRemoveClass sc "sending-button"
  Gtk.set img [#sensitive Gtk.:= True]

setSending :: InputBoxReferences -> IO ()
setSending InputBoxReferences{..} = do
  Gtk.set tv [#editable Gtk.:= False]
  Gtk.set btn [#sensitive Gtk.:= False]
  sc <- Gtk.widgetGetStyleContext btn
  Gtk.styleContextAddClass sc "sending-button"
  Gtk.styleContextRemoveClass sc "send-button"
  Gtk.set img [#sensitive Gtk.:= False]

isAvailable :: InputBoxReferences -> IO Bool
isAvailable InputBoxReferences{..} = Gtk.get tv #editable

appendTV :: Gtk.TextView -> Text -> IO ()
appendTV tv t = do
  buf <- Gtk.get tv #buffer
  text <- fromMaybe "" <$> Gtk.get buf #text
  Gtk.set buf [#text Gtk.:= text <> t]

inputBox ::
  Vector (Attribute Gtk.Box InputBoxEvent) ->
  InputBoxProperties ->
  Widget InputBoxEvent
inputBox customAttributes customParams =
  Widget
    ( CustomWidget
        { customWidget
        , customCreate
        , customPatch
        , customSubscribe
        , customAttributes
        , customParams
        }
    )
 where
  customWidget = Gtk.Box
  -- Bug: Won't appear if not created at the beginning?
  customCreate InputBoxProperties{..} = do
    box <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]

    img <- Gtk.fileChooserButtonNew "Choose an Image to Send" Gtk.FileChooserActionOpen
    Gtk.set img [#widthChars Gtk.:= 5]
    filter <- Gtk.fileFilterNew
    forM_
      (["image/gif", "image/png", "image/jpeg", "image/bmp", "image/webp"] :: [Text])
      $ Gtk.fileFilterAddMimeType filter
    Gtk.fileChooserAddFilter img filter

    tv <- Gtk.new Gtk.TextView [#wrapMode Gtk.:= Gtk.WrapModeChar, #monospace Gtk.:= True]
    vp <- Gtk.new Gtk.Viewport [#child Gtk.:= tv]
    sw <-
      Gtk.new
        Gtk.ScrolledWindow
        [#hscrollbarPolicy Gtk.:= Gtk.PolicyTypeNever, #vscrollbarPolicy Gtk.:= Gtk.PolicyTypeAutomatic, #child Gtk.:= vp]

    btn <- Gtk.buttonNewWithMnemonic "_>_>_= Send"
    Gtk.set btn [#halign Gtk.:= Gtk.AlignEnd]

    let refs = InputBoxReferences version tv btn img
    setSend refs
    #packStart box img False False 0
    #packStart box sw True True 0
    #packStart box btn False False 0
    return (box, refs)

  customPatch _old new@InputBoxProperties{..} refs@InputBoxReferences{..} =
    if version > ver
      then CustomModify $ \_box -> do
        case command of
          Sended -> setSend refs
          SendedAndClear -> do
            setSend refs
            buf <- Gtk.get tv #buffer
            Gtk.set buf [#text Gtk.:= ""]
          AtMember target -> do
            available <- isAvailable refs
            when available $ appendTV tv ("[at|" <> T.pack (show target) <> "|]")
        return refs{ver = version}
      else CustomKeep

  customSubscribe _params refs@InputBoxReferences{..} _box cb = do
    h <- Gtk.on btn #clicked $ do
      buf <- Gtk.get tv #buffer
      text <- fromMaybe "" <$> Gtk.get buf #text
      when (text /= "") $ do
        setSending refs
        cb $ InputBoxSend text
    hi <-
      Gtk.on img #fileSet $
        Gtk.fileChooserGetFilename img
          >>= \case
            Just path -> do
              available <- isAvailable refs
              when available $ appendTV tv ("[img|file://" <> T.pack path <> "|]")
            Nothing -> return ()
    return $
      fromCancellation $
        GI.signalHandlerDisconnect img hi
          >> GI.signalHandlerDisconnect btn h