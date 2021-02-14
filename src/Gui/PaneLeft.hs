{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Gui.PaneLeft where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.IO (unsafePerformIO)
import qualified GHC.Int
import GI.Gtk hiding (Bin, Widget, on, set, (:=))
import GI.Gtk.Declarative
import GI.Pango.Enums
import Gui.Update
import Model.Contact

{- |
  The left of the first pane.
  Consists of a 'SearchEntry', a 'notebook' with two 'Page's, each containing a 'ListBox'.
-}
paneLeft :: State -> Widget Event
paneLeft state@State{..} =
  container
    Box
    [#orientation := OrientationVertical]
    [ BoxChild
        defaultBoxChildProperties
        $ widget SearchEntry [onM #searchChanged searchChangeHandler]
    , BoxChild
        defaultBoxChildProperties{fill = True, expand = True}
        $ notebook
          []
          [ makePage "Friend" friendRowHandler makeRow $ filterList friends
          , makePage "Group" groupRowHandler makeRow $ filterList groups
          ]
    ]
 where
  filterList :: Contact a => Vector (a, b) -> Vector (a, b)
  filterList = V.filter (\(contact, _) -> all (`T.isInfixOf` getSearch contact) searchTexts)
  searchChangeHandler :: SearchEntry -> IO Event
  searchChangeHandler entry =
    SearchChange . filter (/= "") . T.split (== ' ') . T.toLower
      <$> (getEntryBuffer entry >>= getEntryBufferText)

-- | Make a 'notebook' 'Page'.
makePage ::
  Contact a =>
  -- | The label of the page.
  Text ->
  -- | The signal Handler.
  (Vector (a, b) -> ListBoxRow -> IO Event) ->
  -- | The 'ListBoxRow' maker.
  (a -> Bin ListBoxRow Event) ->
  -- | The 'friendList' or 'groupList'.
  Vector (a, b) ->
  -- | The event to emit.
  Page Event
makePage name handler rowMaker list =
  page name $
    bin
      ScrolledWindow
      [ #hscrollbarPolicy := PolicyTypeNever
      , #vscrollbarPolicy := PolicyTypeAutomatic
      ]
      $ bin Viewport [] $
        container
          ListBox
          [ #widthRequest := 225
          , onM #rowActivated (\row _box -> handler list row)
          ]
          $ rowMaker . fst <$> list

-- | Make a 'ListBoxRow'.
makeRow :: Contact a => a -> Bin ListBoxRow Event
makeRow a =
  bin
    ListBoxRow
    [#heightRequest := 60]
    $ widget
      Label
      [ #label := getDisplay a
      , #wrap := True
      , #wrapMode := GI.Pango.Enums.WrapModeChar
      ]

-- | The signal handler for the 'ListBox'.
rowHandler ::
  Contact a =>
  -- | The wrapper for a 'friend'/'group'.
  (a -> CurrentContact) ->
  -- | The filtered 'friendList' or 'groupList'.
  Vector (a, b) ->
  -- | The activated 'ListBoxRow'.
  ListBoxRow ->
  -- | The event to emit.
  IO Event
rowHandler wrapper list row = do
  ix <- fromIntegral <$> listBoxRowGetIndex row
  return . ContactChange . wrapper . fst $ list V.! ix

-- | The generated signal handler for the 'ListBox' for 'friendList'.
friendRowHandler :: Vector (Friend, b) -> ListBoxRow -> IO Event
friendRowHandler = rowHandler CurrentFriend

-- | The generated signal handler for the 'ListBox' for 'groupList'.
groupRowHandler ::
  Vector (Group, b) ->
  ListBoxRow ->
  IO Event
groupRowHandler = rowHandler CurrentGroup