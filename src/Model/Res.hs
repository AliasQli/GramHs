{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Res where

import Control.Monad.Except
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Maybe
import Data.Text (Text)

codeTable :: [(Int, Text)]
codeTable =
  [ (0, "ok"),
    (1, "wrong auth key"),
    (2, "designated bot unexist"),
    (3, "session invalid or unexist"),
    (4, "session not verified"),
    (5, "target unexist"),
    (6, "designated file unexist"),
    (10, "bot not privileged"),
    (20, "bot muted"),
    (30, "message too long"),
    (400, "bad request")
  ]

class Response a where
  getCode :: a -> Int
  isOK :: a -> Bool
  isOK = (== 0) . getCode
  getInfo :: a -> Text
  getInfo a = fromMaybe "unknown error" $ lookup (getCode a) codeTable
  {-# MINIMAL getCode #-}

assertOK :: (MonadError Text m, Response a) => a -> m ()
assertOK res = unless (isOK res) $ throwError (getInfo res)

data AuthRes = AuthRes
  { code :: Int,
    session :: Text
  }

$(deriveJSON defaultOptions ''AuthRes)

instance Response AuthRes where
  getCode = code

data VerifyRes = VerifyRes
  { code :: Int,
    msg :: Text
  }

$(deriveJSON defaultOptions ''VerifyRes)

instance Response VerifyRes where
  getCode = code

data ReleaseRes = ReleaseRes
  { code :: Int,
    msg :: Text
  }

$(deriveJSON defaultOptions ''ReleaseRes)

instance Response ReleaseRes where
  getCode = code

data SendMessageRes = SendMessageRes
  { code :: Maybe Int,
    msg :: Maybe Text,
    messageId :: Int
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions ''SendMessageRes)

instance Response SendMessageRes where
  getCode = fromMaybe 0 . (code :: SendMessageRes -> Maybe Int)