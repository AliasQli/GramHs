{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

module Gui.PaneLeft where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Vector        (Vector)
import qualified Data.Vector        as V
import           GI.Gtk             hiding (Bin, Widget, on, set, (:=))
import           GI.Gtk.Declarative
import           GI.Pango.Enums
import           Gui.Update
import           Model.Contact

{- |
  The left of the first pane.
  Consists of a 'SearchEntry', a 'notebook' with two 'Page's, each containing a 'ListBox'.
-}
paneLeft :: State -> Widget Event
paneLeft State{..} =
  container
    Box
    [#orientation := OrientationVertical]
    [ BoxChild
        defaultBoxChildProperties $
        widget SearchEntry [onM #searchChanged searchChangeHandler]
    , BoxChild
        defaultBoxChildProperties
          { fill   = True
          , expand = True
          } $
        notebook []
          [ makePage "Friend" friendRowHandler makeRow $ filterList friends
          , makePage "Group" groupRowHandler makeRow $ filterList groups
          ]
    ]
 where
  filterList :: Contact a => Vector (a, b) -> Vector (a, b)
  filterList = V.filter (\(contact, _) -> all (`T.isInfixOf` getSearch contact) searchTexts)
  searchChangeHandler :: SearchEntry -> IO Event
  searchChangeHandler entry =
    SearchChange        .
    filter    (/= "")   .
    T.split   (== ' ')  .
    T.toLower          <$>
      (getEntryBuffer entry >>= getEntryBufferText)

-- | Make a 'notebook' 'Page'.
makePage
  :: Text                                       -- ^ The label of the page.
  -> (Vector (a, b) -> ListBoxRow -> ListBox -> IO Event)  -- ^ The signal Handler.
  -> (a -> Bin ListBoxRow Event)                -- ^ The 'ListBoxRow' maker.
  -> Vector (a, b)                              -- ^ The 'friendList' or 'groupList'.
  -> Page Event                                 -- ^ The event to emit.
makePage name handler rowMaker list =
  page name $
    bin
      ScrolledWindow
      [ #hscrollbarPolicy := PolicyTypeNever
      , #vscrollbarPolicy := PolicyTypeAutomatic
      ] $
      bin Viewport [] $
        container
          ListBox
          [ #widthRequest          := 225
          , onM #rowActivated      (handler list)
          ] $
          rowMaker . fst <$> list

-- | Make a 'ListBoxRow'.
makeRow :: Contact a => a -> Bin ListBoxRow Event
makeRow a =
  bin
    ListBoxRow
    [ #heightRequest := 60
    -- , #selectable := True
    ] $
    widget
      Label
      [ #label    := getDisplay a
      , #wrap     := True
      , #wrapMode := GI.Pango.Enums.WrapModeChar
      ]

-- | The signal handler for the 'ListBox'.
rowHandler
  :: (a -> CurrentContact)  -- ^ The wrapper for a 'friend'/'group'.
  -> Vector (a, b)          -- ^ The filtered 'friendList' or 'groupList'.
  -> ListBoxRow             -- ^ The activated 'ListBoxRow'.
  -> ListBox
  -> IO Event               -- ^ The event to emit.
rowHandler wrapper list row box = do
  ix <- fromIntegral <$> listBoxRowGetIndex row
  return $ ContactChange (wrapper $ fst $ list V.! ix) box

-- | The generated signal handler for the 'ListBox' for 'friendList'.
friendRowHandler :: Vector (Friend, b) -> ListBoxRow -> ListBox -> IO Event
friendRowHandler = rowHandler CurrentFriend

-- | The generated signal handler for the 'ListBox' for 'groupList'.
groupRowHandler
  :: Vector (Group, b)
  -> ListBoxRow
  -> ListBox
  -> IO Event
groupRowHandler = rowHandler CurrentGroup
