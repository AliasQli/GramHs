{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Gui.View where

import Data.ByteString
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GI.Gtk hiding (Bin, Widget, on, set, (:=))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Gui.PaneLeft
import Gui.PaneRight
import Gui.Update

{-

+-------------+------------------------------+----------+  -
|             |------------------------------+----------|  |
|             |                              |          |  |
|             |                              |          |
|             |                              |   pane   | Box
|             |          paneUpLeft          |    Up    |
|             |                              |   Right  |  |
|   paneLeft  |                              |          |  |
|             |                              |          |  |
|             |------------------------------+----------|  -
|             |                                         |
|             |                 paneDown                |
|             |                                         |
|             |                                         |
+-------------+-----------------------------------------+

              | <-------------- paneRight ------------->|
-}
css :: ByteString
css =
  fold
    ( [ ".send-button{"
      , "  background-color: #3465a4;"
      , "  color: white;"
      , "}"
      , ".send-button:hover{"
      , " background-color: #729fcf;"
      , "}"
      , ".sending-button{"
      , "  background-color: #87add6;"
      , "  color: white;"
      , "}"
      ] ::
        [ByteString]
    )

view' :: State -> AppView Window Event
view' state =
  bin
    Window
    [ #title := "HsTim"
    , #widthRequest := 800
    , #heightRequest := 600
    , on #deleteEvent $ const (True, Closed)
    ]
    $ paned
      [#margin := 5]
      (pane defaultPaneProperties{resize = False, shrink = False} $ paneLeft state)
      (pane defaultPaneProperties{resize = True, shrink = False} $ paneRight state)
