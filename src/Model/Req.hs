{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Model.Req where

import           Data.Aeson.TH
import           Data.Text     (Text)
import           Model.Message

-- [POST] /auth
newtype AuthReq = AuthReq
  { authKey :: Text
  }
  deriving (Show)

$(deriveJSON defaultOptions ''AuthReq)

-- [POST] /verify
data VerifyReq = VerifyReq
  { sessionKey :: Text
  , qq         :: Int
  }
  deriving (Show)

$(deriveJSON defaultOptions ''VerifyReq)

-- [POST] /release
data ReleaseReq = ReleaseReq
  { sessionKey :: Text
  , qq         :: Int
  }
  deriving (Show)

$(deriveJSON defaultOptions ''ReleaseReq)

-- Assisting data type of SendMessageReq
data SendMessage = SendMessage
  { target       :: Int
  , quote        :: Maybe Int
  , messageChain :: MessageChain
  }
  deriving (Show)

-- [POST] /sendFriendMessage
-- [POST] /sendGroupMessage
data SendMessageReq = SendMessageReq
  { sessionKey   :: Text
  , target       :: Int
  , quote        :: Maybe Int
  , messageChain :: MessageChain
  }
  deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields = True} ''SendMessageReq)
