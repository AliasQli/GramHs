{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Gui.PaneRight where

import           Control.Monad.ST
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as Algo
import           GI.Gtk                       hiding (Bin, Widget, on, set,
                                               (:=))
import           GI.Gtk.Declarative
import           GI.Pango.Enums
import           Gui.CustomInput
import           Gui.Update
import           Model.Contact
import           Model.Message
import           Prelude                      hiding (id)
import           Type

-- | The right of the first pane. In which is a second pane.
paneRight :: State -> Widget Event
paneRight state =
  paned
    [#orientation := OrientationVertical]
    (pane
      defaultPaneProperties
        {resize = True
        , shrink = False
        } $
      paneUp state)
    (pane
      defaultPaneProperties
        {resize = False
        , shrink = False
        } $
      paneDown state)

{- |
  The upper part of the second pane.
  If 'currentContact' is a 'group', inside is a third pane;
  if not, only the left part of the third pane is shown.
-}
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
              [ #label          := getCurrentDisplay state currentContact
              , #heightRequest  := 30
              , #halign         := AlignStart
              , #maxWidthChars  := 25
              , #ellipsize      := EllipsizeModeEnd
              ]
        , BoxChild defaultBoxChildProperties{padding = 5} $
            widget
              Separator
              [ #orientation := OrientationHorizontal
              ]
        , BoxChild
            defaultBoxChildProperties{fill = True, expand = True} $
            case currentContact of
              CurrentGroup _group ->
                paned
                  []
                  (pane
                    defaultPaneProperties
                      { resize = True
                      , shrink = False
                      } $
                    paneUpLeft state)
                  (pane
                    defaultPaneProperties
                      {resize = False
                      , shrink = False
                      } $
                    paneUpRight state)
              _ ->
                paneUpLeft state
        ]

-- | The left part of the second pane. Contains a 'ListBox', in which one row is a message.
paneUpLeft :: State -> Widget Event
paneUpLeft State{..} =
  bin
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAlways
    ]
    $ bin Viewport [] $
      container
        ListBox
        [ #valign                 := AlignEnd
        , #selectionMode          := SelectionModeNone
        , #activateOnSingleClick  := False
        , onM #rowActivated       (messageRowHandler messageObjects)
        ] $
        bin ListBoxRow [] . makeMessage <$> messageObjects
 where
  messageObjects = case currentContact of
    CurrentFriend friend -> friendMessages $ friends ?! friend
    CurrentGroup group   -> groupMessages  $ groups  ?! group
    _                    -> []

-- | Receives the 'MessageObject's and returns a signal handler for the 'ListBox' for 'MessageObject's.
messageRowHandler :: Vector MessageObject -> ListBoxRow -> ListBox -> IO Event
messageRowHandler messageObjects row _box = do
  ix <- fromIntegral <$> listBoxRowGetIndex row
  let messageChain =
        case messageObjects V.! ix of
          FriendMessage{..} -> fmessageChain
          GroupMessage{..}  -> gmessageChain
      Source{..} = case messageChain V.! 0 of
        source@Source{} -> source
        _               -> error "Invalid message"
  return $ MessageClicked id

-- | Makes a label containing the message.
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

-- | The right part of the third pane. A 'ListBox' for 'MemberList'.
paneUpRight :: State -> Widget Event
paneUpRight State{..} =
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
            [ #widthRequest           := 160
            , #selectionMode          := SelectionModeNone
            , #activateOnSingleClick  := False
            , onM #rowActivated        $ memberRowHandler list
            ] $
            bin ListBoxRow [#heightRequest := 40] . makeMember <$> sortMemberList list
 where
  CurrentGroup group = currentContact
  GroupRecord memberList _messages = groups ?! group
  makeLabel label = widget Label [#widthRequest := 160, #label := label] :: Widget Event

sortMemberList :: Vector Member -> Vector Member
sortMemberList list = runST $ do
  mvector <- V.unsafeThaw list
  Algo.sort mvector
  V.unsafeFreeze mvector

-- | Receives a 'MemberList' and returns a signal handler for the 'ListBox' for 'MemberList'.
memberRowHandler :: MemberList -> ListBoxRow -> ListBox -> IO Event
memberRowHandler list row _box = do
  ix <- fromIntegral <$> listBoxRowGetIndex row
  return . MemberClicked . getId $ list V.! ix

-- | Makes a label containing the member.
makeMember :: Member -> Widget Event
makeMember a =
  widget
    Label
    [ #label    := getDisplay a
    , #wrap     := True
    , #wrapMode := GI.Pango.Enums.WrapModeChar
    ]

-- | The lower part of the second pane. Contains the custom input box.
paneDown :: State -> Widget Event
paneDown State{..} =
  handle <$>
    inputBox
      [ #orientation    := OrientationVertical
      , #heightRequest  := 200
      ]
      inputBoxProperties
 where
  handle (InputBoxSend i t) = Send i t
