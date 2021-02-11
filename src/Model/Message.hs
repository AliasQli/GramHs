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
import GHC.IO (unsafePerformIO)
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

-- .replace(/&/g, "&amp;")
--        .replace(/</g, "&lt;")
--        .replace(/>/g, "&gt;")
--        .replace(/"/g, "&quot;")
--        .replace(/'/g, "&#039;");

escape :: Text -> Text
escape = foldr (.) Prelude.id $ zipWith T.replace ["<", ">", "\"", "'"] ["&lt;", "&gt;", "&quot;", "&#039;"]

instance ShowText Message where
  showText = \case
    Source{..} -> escape (T.pack $ show id) <> "\n"
    Quote{..} ->
      escape $
        "> "
          <> T.replace
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
