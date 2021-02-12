{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Message where

import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Model.Contact
import Type

-- | Piece of a message content.
data Message
  = Source
      { id :: Int
      , time :: Int
      }
  | Quote
      { id :: Int
      , groupId :: Int
      , senderId :: Int
      , targetId :: Int
      , origin :: MessageChain
      }
  | At
      { target :: Int
      , display :: Text
      }
  | AtAll
  | Face
      { faceId :: Int
      , name :: Text
      }
  | Plain
      { text :: Text
      }
  | Image
      { imageId :: Text
      , url :: Text
      , path :: Maybe Text
      }
  | FlashImage
      { imageId :: Text
      , url :: Text
      , path :: Maybe Text
      }
  | Voice
      { voiceId :: Text
      , url :: Text
      , path :: Maybe Text
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
  foldr (.) Prelude.id $
    uncurry T.replace
      <$> [ ("<", "&lt;")
          , (">", "&gt;")
          , ("\"", "&quot;")
          , ("&", "&amp;")
          ]

instance ShowText Message where
  showText = \case
    Source{..} -> escape (T.pack $ show id) <> "\n"
    Quote{..} ->
      escape $
        "> "
          <> ( if null origin
                then T.pack $ show id
                else
                  T.replace
                    "\n"
                    "\n> "
                    ( showText $
                        V.dropWhile
                          ( \case
                              Source{..} -> True
                              Quote{..} -> True
                              _ -> False
                          )
                          origin
                    )
             )
          <> "\n"
    At{..} -> escape display
    AtAll -> "@All"
    Face{..} -> "[face|" <> name <> "|]"
    Plain{..} -> escape text
    Image{..} -> "<a href=\"" <> url <> "\">img</a>"
    FlashImage{..} -> "<a href=\"" <> url <> "\">flashimg</a>"
    Voice{..} -> "<a href=\"" <> url <> "\">voice</a>"
    Xml{..} -> "[xml|" <> escape xml <> "|]"
    Json{..} -> "[json|" <> escape json <> "|]"
    App{..} -> "[app|" <> escape content <> "|]"
    Poke{..} -> "[poke|" <> name <> "|]"

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
  showText = \case
    Just a -> getDisplay a
    Nothing -> "You"

-- | A message object, including its content and its sender.
data MessageObject
  = FriendMessage
      { fmessageChain :: MessageChain
      , fsender :: Maybe Friend
      }
  | GroupMessage
      { gmessageChain :: MessageChain
      , gsender :: Maybe Member
      }
  deriving (Show, Eq)

instance ShowText MessageObject where
  showText = \case
    FriendMessage messageChain sender ->
      showText sender <> " > " <> showText messageChain
    GroupMessage messageChain sender ->
      showText sender <> " > " <> showText messageChain

$( deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , sumEncoding = defaultTaggedObject{tagFieldName = "type"}
      }
    ''MessageObject
 )
