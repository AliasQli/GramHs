{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Model.Message where

import           Data.Aeson.TH
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Model.Contact
import           Prelude       hiding (id)
import           Type

-- | Piece of a message content.
data Message
  = Source
      { id   :: Int
      , time :: Int
      }
  | Quote
      { id       :: Int
      , groupId  :: Int
      , senderId :: Int
      , targetId :: Int
      , origin   :: MessageChain
      }
  | At
      { target  :: Int
      , display :: Text
      }
  | AtAll
  | Face
      { faceId :: Int
      , name   :: Text
      }
  | Plain
      { text :: Text
      }
  | Image
      { imageId :: Text
      , url     :: Text
      , path    :: Maybe Text
      }
  | FlashImage
      { imageId :: Text
      , url     :: Text
      , path    :: Maybe Text
      }
  | Voice
      { voiceId :: Text
      , url     :: Text
      , path    :: Maybe Text
      }
  | Xml
      { xml :: Text
      }
  | Json
      { json :: Text
      }
  | App
      { content :: Text
      }
  | Poke
      { name :: Text
      }
  deriving (Show, Eq)

escape :: Text -> Text
escape =
  foldr1 (.) $
    uncurry T.replace <$>
      [ ("<" ,  "&lt;")
      , ("\"",  "&quot;")
      , ("&" ,  "&amp;") -- & should be escaped first
      ]

instance ShowText Message where
  showText Plain{..}      = escape text

  showText At{..}         = escape display
  showText AtAll          = "@All"

  showText Image{..}      = "<a href=\"" <> url <> "\">img</a>"
  showText FlashImage{..} = "<a href=\"" <> url <> "\">flashimg</a>"
  showText Voice{..}      = "<a href=\"" <> url <> "\">voice</a>"

  showText Face{..}       = "[face|"  <> name           <> "|]"
  showText Xml{..}        = "[xml|"   <> escape xml     <> "|]"
  showText Json{..}       = "[json|"  <> escape json    <> "|]"
  showText App{..}        = "[app|"   <> escape content <> "|]"
  showText Poke{..}       = "[poke|"  <> name           <> "|]"

  showText Source{..}     = escape (T.pack $ show id) <> "\n"
  showText Quote{..}      = escape ("> " <> quoted <> "\n")
    where
      quoted = if null origin
        then T.pack $ show id
        else T.replace "\n" "\n> " $
          showText (V.dropWhile dropPred origin)
      dropPred = \case
        Source{} -> True
        Quote{}  -> True
        _        -> False

-- | The content of a message
type MessageChain = Vector Message

instance ShowText MessageChain where
  showText c = foldMap showText c

$( deriveJSON
    defaultOptions
      { omitNothingFields = True
      , sumEncoding = defaultTaggedObject{tagFieldName = "type"}
      }
    ''Message
 )

instance Address a => ShowText (Maybe a) where
  showText (Just a) = getDisplay a
  showText Nothing  = "You"

-- | A message object, including its content and its sender.
data MessageObject
  = FriendMessage
      { fmessageChain :: MessageChain
      , fsender       :: Maybe Friend
      }
  | GroupMessage
      { gmessageChain :: MessageChain
      , gsender       :: Maybe Member
      }
  deriving (Show, Eq)

instance ShowText MessageObject where
  showText (FriendMessage messageChain sender)
    = showText sender <> " > " <> showText messageChain
  showText (GroupMessage  messageChain sender)
    = showText sender <> " > " <> showText messageChain

$( deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , sumEncoding = defaultTaggedObject{tagFieldName = "type"}
      }
    ''MessageObject
 )
