{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

makePage ::
  Contact a =>
  Text ->
  (Vector (a, b) -> ListBoxRow -> IO Event) ->
  (a -> Bin ListBoxRow Event) ->
  Vector (a, b) ->
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

rowHandler :: Contact a => (a -> CurrentContact) -> Vector (a, b) -> ListBoxRow -> IO Event
rowHandler packer list row = do
  ix <- fromIntegral <$> listBoxRowGetIndex row
  return . ContactChange . packer . fst $ list V.! ix

friendRowHandler :: Vector (Friend, b) -> ListBoxRow -> IO Event
friendRowHandler = rowHandler CurrentFriend

groupRowHandler :: Vector (Group, b) -> ListBoxRow -> IO Event
groupRowHandler = rowHandler CurrentGroup