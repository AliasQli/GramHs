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
import Model.Contact
import Type

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

instance ShowText Message where
  showText = \case
    Source{..} -> T.pack $ show id <> "\n"
    Quote{..} -> "[" <> showText origin <> "]\n"
    At{..} -> display
    AtAll -> "@All"
    Face{..} -> "[face|" <> name <> "|]"
    Plain{..} -> text
    Image{..} -> "<a href=\"" <> url <> "\">img</a>"
    FlashImage{..} -> "<a href=\"" <> url <> "\">flashimg</a>"
    Voice{..} -> "<a href=\"" <> url <> "\">voice</a>"
    Xml{..} -> "[xml|" <> xml <> "|]"
    Json{..} -> "[json|" <> json <> "|]"
    App{..} -> "[app|" <> content <> "|]"
    Poke{..} -> "[poke|" <> name <> "|]"

type MessageChain = Vector Message

instance ShowText MessageChain where
  showText = foldMap showText

$( deriveJSON
    defaultOptions
      { omitNothingFields = True,
        sumEncoding = defaultTaggedObject{tagFieldName = "type"}
      }
    ''Message
 )

instance ShowText (Maybe Friend) where
  showText = \case
    Just friend -> getDisplay friend
    Nothing -> "You"

instance ShowText (Maybe Member) where
  showText = \case
    Just member -> getDisplay member
    Nothing -> "You"

data MessageObject
  = FriendMessage
      { fmessageChain :: MessageChain
      , fsender :: Maybe Friend
      }
  | GroupMessage
      { fmessageChain :: MessageChain
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
