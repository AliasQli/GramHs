{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Model.Contact where

import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Maybe    (fromMaybe)
import           Data.String   (IsString)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Vector   (Vector)

class Address a where
  getId :: a -> Int
  getDisplay :: a -> Text
  {-# MINIMAL getId, getDisplay #-}

class Address a => Contact a where
  getSearch :: a -> Text
  getSearch a = T.pack (show $ getId a) <> " " <> T.toLower (getDisplay a)

data Friend = Friend
  { nickname :: Text
  , id       :: Int
  , remark   :: Text
  }
  deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions ''Friend)

instance Address Friend where
  getId = Model.Contact.id
  getDisplay a = if remark a == "" then nickname a else remark a

instance Contact Friend where
  getSearch a = T.pack (show $ getId a) <> " " <> T.toLower (nickname a) <> " " <> T.toLower (remark a)

type FriendList = Vector Friend

data Group = Group
  { name       :: Text
  , id         :: Int
  , permission :: Text
  }
  deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions ''Group)

instance Address Group where
  getId = Model.Contact.id
  getDisplay = name

instance Contact Group

type GroupList = Vector Group

data Member = Member
  { permission :: Text
  , memberName :: Text
  , id         :: Int
  , group      :: Group
  }
  deriving (Show, Eq)

getWeight :: (Eq a, IsString a) => a -> Int
getWeight perm = fromMaybe 3 $ lookup perm weightTable
 where
  weightTable = [("OWNER", 0), ("ADMINISTRATOR", 1), ("MEMBER", 2)]

instance Ord Member where
  compare (Member p1 m1 id1 g1) (Member p2 m2 id2 g2) = foldl f EQ ords
   where
    f EQ new = new
    f old _  = old
    ords =
      [ compare (getWeight p1)  (getWeight p2)
      , compare m1              m2
      , compare id1             id2
      , compare g1              g2
      ]

$(deriveJSON defaultOptions ''Member)

instance Address Member where
  getId = Model.Contact.id
  getDisplay = memberName

type MemberList = Vector Member
