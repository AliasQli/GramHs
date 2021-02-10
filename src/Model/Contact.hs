{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Contact where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)

class Address a where
  getId :: a -> Int
  getDisplay :: a -> Text
  {-# MINIMAL getId, getDisplay #-}

class Address a => Contact a where
  getSearch :: a -> Text
  getSearch a = T.pack (show $ getId a) <> " " <> T.toLower (getDisplay a)

data Friend = Friend
  { id :: Int
  , nickname :: Text
  , remark :: Text
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''Friend)

instance Address Friend where
  getId = Model.Contact.id
  getDisplay a = if remark a == "" then nickname a else remark a

instance Contact Friend where
  getSearch a = T.pack (show $ getId a) <> " " <> T.toLower (nickname a) <> " " <> T.toLower (remark a)

type FriendList = Vector Friend

data Group = Group
  { id :: Int
  , name :: Text
  , permission :: Text
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''Group)

instance Address Group where
  getId = Model.Contact.id
  getDisplay = name

instance Contact Group

type GroupList = Vector Group

data Member = Member
  { id :: Int
  , memberName :: Text
  , permission :: Text
  , group :: Group
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''Member)

instance Address Member where
  getId = Model.Contact.id
  getDisplay = memberName

type MemberList = Vector Member
