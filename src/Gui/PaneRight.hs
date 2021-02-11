{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Gui.PaneRight where

import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GI.Gtk hiding (Bin, Widget, on, set, (:=))
import GI.Gtk.Declarative
import GI.Pango.Enums
import Gui.CustomInput
import Gui.Update
import Model.Contact
import Model.Message
import Type

paneRight :: State -> Widget Event
paneRight state =
  paned
    [#orientation := OrientationVertical]
    (pane defaultPaneProperties{resize = True, shrink = False} $ paneUp state)
    (pane defaultPaneProperties{resize = False, shrink = False} $ paneDown state)

paneUp :: State -> Widget Event
paneUp state@State{..} =
  if currentContact == Nihil
    then widget Label []
    else
      container
        Box
        [#orientation := OrientationVertical]
        [ BoxChild defaultBoxChildProperties $
            widget
              Label
              [ #label := getCurrentDisplay state currentContact
              , #heightRequest := 30
              , #halign := AlignStart
              , #maxWidthChars := 25
              , #ellipsize := EllipsizeModeEnd
              ]
        , BoxChild defaultBoxChildProperties{padding = 5} $
            widget
              Separator
              [ #orientation := OrientationHorizontal
              ]
        , BoxChild
            defaultBoxChildProperties{fill = True, expand = True}
            if
                | CurrentGroup _ <- currentContact ->
                  paned
                    []
                    (pane defaultPaneProperties{resize = True, shrink = False} $ paneUpLeft state)
                    (pane defaultPaneProperties{resize = False, shrink = False} $ paneUpRight state)
                | otherwise ->
                  paneUpLeft state
        ]

-- TODO: Make it scroll automatically to the bottom.
paneUpLeft :: State -> Widget Event
paneUpLeft state@State{..} =
  bin
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAlways
    ]
    $ bin Viewport [] $
      container
        Box
        [#valign := AlignEnd, #orientation := OrientationVertical]
        $ BoxChild defaultBoxChildProperties . makeMessage <$> messageObjects
 where
  messageObjects = case currentContact of
    CurrentFriend friend -> friendMessages $ friends ?! friend
    CurrentGroup group -> groupMessages $ groups ?! group
    _ -> []

makeMessage :: MessageObject -> Widget Event
makeMessage messageObject =
  widget
    Label
    [ #halign := AlignStart
    , #useMarkup := True
    , #wrap := True
    , #wrapMode := GI.Pango.Enums.WrapModeChar
    , #label := (showText messageObject <> "\n")
    ]

paneUpRight :: State -> Widget Event
paneUpRight state@State{..} =
  case memberList of
    Pending ->
      makeLabel "Loading..."
    Failed ->
      makeLabel "Can't load."
    Success list ->
      bin
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeNever
        , #vscrollbarPolicy := PolicyTypeAutomatic
        ]
        $ bin Viewport [] $
          container
            ListBox
            [ #widthRequest := 160
            , #selectionMode := SelectionModeNone
            , #activateOnSingleClick := False
            , onM #rowActivated (rowHandler list)
            ]
            $ makeRow <$> list
 where
  CurrentGroup group = currentContact
  GroupRecord memberList _messages = groups ?! group
  makeLabel label = widget Label [#widthRequest := 160, #label := label] :: Widget Event

rowHandler :: MemberList -> ListBoxRow -> ListBox -> IO Event
rowHandler list row _box = do
  ix <- fromIntegral <$> listBoxRowGetIndex row
  return . MemberClicked . getId $ list V.! ix

makeRow :: Member -> Bin ListBoxRow Event
makeRow a =
  bin ListBoxRow [#heightRequest := 40] $
    widget
      Label
      [ #label := getDisplay a
      , #wrap := True
      , #wrapMode := GI.Pango.Enums.WrapModeChar
      ]

paneDown :: State -> Widget Event
paneDown state@State{..} =
  handle
    <$> inputBox
      [ #orientation := OrientationVertical
      , #heightRequest := 200
      ]
      inputBoxProperties
 where
  handle (InputBoxSend t) = Send t