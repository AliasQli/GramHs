{-# LANGUAGE ApplicativeDo #-}
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
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource (fromCancellation)

-- | Commands the custom widget receive.
data Command
  = -- | Message has been sended (and failed).
    Sended
  | -- | Message has been sended (and succeeded),
    -- and the input box should be cleared.
    SendedAndClear
  | -- | At (\@) a member.
    --
    -- [@Int@] qq of the member.
    AtMember Int
  | -- | Quote (reply to) a message.
    --
    -- [@Int@] 'messageId' of that message.
    QuoteMessage Int
  deriving (Show, Eq)

{- |
  The props of the custom widget. Used to send commads to the widget.

  To send a command, first set the 'command' field to a desired value,
  then set 'version' to @succ version@.
-}
data InputBoxProperties = InputBoxProperties
  { -- | The version of the command.
    -- A 'command' is received when and only when 'version' cahnges to a greater value.
    version :: Integer
  , -- | The 'command' sent to the custom widget.
    command :: Command
  }
  deriving (Show, Eq)

-- Events the custom widget may emit.
data InputBoxEvent
  = -- | Send a message.
    --
    -- [@Maybe Int@] The target to at(\@).
    -- [@Text@] The text to send.
    InputBoxSend (Maybe Int) Text

-- | The internal state of the custom widget.
data InputBoxReferences = InputBoxReferences
  { -- | Version of the last command received.
    ver :: Integer
  , -- | 'The TextView'.
    tv :: Gtk.TextView
  , -- | The 'Button'.
    btn :: Gtk.Button
  , -- | The 'FileChooserButton' used for choosing images.
    img :: Gtk.FileChooserButton
  , -- | The 'Entry' used for displaying the 'messageId' of the message to reply to.
    entry :: Gtk.Entry
  }

-- | Set the whole widget to "send" status.
setSend :: InputBoxReferences -> IO ()
setSend InputBoxReferences{..} = do
  Gtk.set tv [#editable Gtk.:= True]
  Gtk.set btn [#sensitive Gtk.:= True]
  sc <- Gtk.widgetGetStyleContext btn
  Gtk.styleContextAddClass sc "send-button"
  Gtk.styleContextRemoveClass sc "sending-button"
  Gtk.set img [#sensitive Gtk.:= True]

-- | Set the whole widget to "sending" status.
setSending :: InputBoxReferences -> IO ()
setSending InputBoxReferences{..} = do
  Gtk.set tv [#editable Gtk.:= False]
  Gtk.set btn [#sensitive Gtk.:= False]
  sc <- Gtk.widgetGetStyleContext btn
  Gtk.styleContextAddClass sc "sending-button"
  Gtk.styleContextRemoveClass sc "send-button"
  Gtk.set img [#sensitive Gtk.:= False]

-- | Test if the widget is available (not sending).
isAvailable :: InputBoxReferences -> IO Bool
isAvailable InputBoxReferences{..} = Gtk.get tv #editable

-- | Append some text to the 'TextView'.
appendTV :: Gtk.TextView -> Text -> IO ()
appendTV tv t = do
  buf <- Gtk.get tv #buffer
  text <- fromMaybe "" <$> Gtk.get buf #text
  Gtk.set buf [#text Gtk.:= text <> t]

-- | The custom input box.
inputBox ::
  -- | Attributes of the outer 'Box'.
  Vector (Attribute Gtk.Box InputBoxEvent) ->
  -- | The initial props.
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
  -- What the outer widget is.
  customWidget = Gtk.Box
  -- Create the widget using its initial props.
  -- Bug: Won't appear if not created at the beginning?
  customCreate InputBoxProperties{..} = do
    box <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationVertical]

    bar <- Gtk.new Gtk.Box []

    img <- Gtk.fileChooserButtonNew "Choose an Image to Send" Gtk.FileChooserActionOpen
    Gtk.set img [#widthChars Gtk.:= 5]
    filter <- Gtk.fileFilterNew
    forM_
      (["image/gif", "image/png", "image/jpeg", "image/bmp", "image/webp"] :: [Text])
      $ Gtk.fileFilterAddMimeType filter
    Gtk.fileChooserAddFilter img filter
    #packStart bar img True True 0

    entry <- Gtk.new Gtk.Entry [#editable Gtk.:= False]
    #packStart bar entry False False 0

    #packStart box bar False False 0

    tv <- Gtk.new Gtk.TextView [#wrapMode Gtk.:= Gtk.WrapModeChar, #monospace Gtk.:= True]
    Gtk.widgetSetTooltipText tv $ Just "{-# LANGUAGE QuasiQuotes #-}"
    vp <- Gtk.new Gtk.Viewport [#child Gtk.:= tv]
    sw <-
      Gtk.new
        Gtk.ScrolledWindow
        [#hscrollbarPolicy Gtk.:= Gtk.PolicyTypeNever, #vscrollbarPolicy Gtk.:= Gtk.PolicyTypeAutomatic, #child Gtk.:= vp]
    #packStart box sw True True 0

    btn <- Gtk.buttonNewWithMnemonic "_>_>_= Send"
    Gtk.set btn [#halign Gtk.:= Gtk.AlignEnd]
    #packStart box btn False False 0

    let refs = InputBoxReferences version tv btn img entry
    setSend refs
    return (box, refs)
  -- Adjust the widget when the props change.
  customPatch _old new@InputBoxProperties{..} refs@InputBoxReferences{..} =
    if version > ver
      then CustomModify $ \_box -> do
        case command of
          Sended -> setSend refs
          SendedAndClear -> do
            setSend refs
            buf <- Gtk.get tv #buffer
            Gtk.set buf [#text Gtk.:= ""]
            entryBuf <- Gtk.get entry #buffer
            Gtk.set entryBuf [#text Gtk.:= ""]
            clear entry #secondaryIconGicon
          AtMember target ->
            isAvailable refs
              >>= guard
              >> appendTV tv ("[at|" <> T.pack (show target) <> "|]")
          QuoteMessage messageId -> do
            available <- isAvailable refs
            guard available
            entryBuf <- Gtk.get entry #buffer
            Gtk.set entryBuf [#text Gtk.:= T.pack (show messageId)]
            icon <- Gio.iconNewForString "gtk-clear"
            Gtk.set entry [#secondaryIconGicon Gtk.:= icon]
        return refs{ver = version}
      else CustomKeep
  -- Listen on some signals and emit corresponding events.
  customSubscribe _params refs@InputBoxReferences{..} _box cb = do
    h <- Gtk.on btn #clicked $ do
      buf <- Gtk.get tv #buffer
      text <- fromMaybe "" <$> Gtk.get buf #text
      entryBuf <- Gtk.get entry #buffer
      entryText <- Gtk.get entryBuf #text
      when (text /= "") $ do
        setSending refs
        cb $ InputBoxSend (if entryText == "" then Nothing else Just $ read $ T.unpack entryText) text
    hi <-
      Gtk.on img #fileSet $
        Gtk.fileChooserGetFilename img
          >>= \case
            Just path ->
              isAvailable refs
                >>= guard
                >> appendTV tv ("[img|file://" <> T.pack path <> "|]")
            Nothing -> return ()
    he <-
      Gtk.on entry #iconPress $
        const . const $ do
          available <- isAvailable refs
          guard available
          entryBuf <- Gtk.get entry #buffer
          Gtk.set entryBuf [#text Gtk.:= ""]
          clear entry #secondaryIconGicon
    return $
      fromCancellation $
        GI.signalHandlerDisconnect img hi
          >> GI.signalHandlerDisconnect entry he
          >> GI.signalHandlerDisconnect btn h