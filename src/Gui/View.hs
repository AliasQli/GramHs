{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

module Gui.View where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import           GI.Gtk                        (Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Gui.PaneLeft
import           Gui.PaneRight
import           Gui.Update

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

-- | The CSS data.
css :: ByteString
css =
  B.concat
    [ ".send-button{"
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
    ]

-- | The core function 'view''. It renders a state to a window.
view' :: State -> AppView Window Event
view' state =
  bin
    Window
    [ #title          := "GramHs"
    , #widthRequest   := 800
    , #heightRequest  := 600
    , on #deleteEvent  $ const (True, Closed)
    ] $
    paned
      [#margin := 5]
      (pane
        defaultPaneProperties
          { resize = False
          , shrink = False
          } $
        paneLeft state)
      (pane
        defaultPaneProperties
          { resize = True
          , shrink = False
          } $
        paneRight state)
