{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Net.Http where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)
import Model.Contact
import Model.Req
import Model.Res
import Network.HTTP.Req
import Type

(/++) :: Foldable t => Url scheme -> t Text -> Url scheme
(/++) = foldl (/:)

get :: (FromJSON a, Foldable t1, Foldable t2) => t1 Text -> t2 (Option 'Http) -> Http (JsonResponse a)
get path query = do
  MiraiConfig{..} <- asks miraiConfig
  req GET (http baseUrl /++ path) NoReqBody jsonResponse $ fold query <> port miraiPort

post :: (ToJSON a1, FromJSON a2, Foldable t1, Foldable t2) => t1 Text -> t2 (Option 'Http) -> a1 -> Http (JsonResponse a2)
post path query payload = do
  MiraiConfig{..} <- asks miraiConfig
  req POST (http baseUrl /++ path) (ReqBodyJson payload) jsonResponse $ fold query <> port miraiPort

-- TODO: Move it elsewhere
{-# NOINLINE config #-}
config :: Config
config = unsafePerformIO $ do
  configFile <- L.readFile "config.json"
  let Just miraiConfig = decode configFile
  let config = Config "" miraiConfig defaultHttpConfig
  key <- runReaderT (runExceptT auth) config
  case key of
    Left err -> error . T.unpack $ err
    Right k -> return $ config{Type.sessionKey = k}

runHttp :: Http a -> IO (Either Text a)
runHttp a = runReaderT (runExceptT a) config

auth :: Http Text
auth = do
  authKey <- asks $ Type.authKey . miraiConfig
  res <- post ["auth"] [] $ AuthReq authKey
  let authRes = responseBody res
  assertOK authRes
  return $ session authRes

verify :: Http ()
verify = do
  sessionKey <- asks Type.sessionKey
  qq <- asks $ Type.qq . miraiConfig
  res <- post ["verify"] [] $ VerifyReq sessionKey qq
  let verifyRes = responseBody res :: VerifyRes
  assertOK verifyRes

getFriendList :: Http FriendList
getFriendList = do
  sessionKey <- asks Type.sessionKey
  res <- get ["friendList"] ["sessionKey" =: sessionKey]
  let friendList = responseBody res
  return friendList

getGroupList :: Http GroupList
getGroupList = do
  sessionKey <- asks Type.sessionKey
  res <- get ["groupList"] ["sessionKey" =: sessionKey]
  let groupList = responseBody res
  return groupList

getMemberList :: Int -> Http MemberList
getMemberList target = do
  sessionKey <- asks Type.sessionKey
  res <- get ["memberList"] ["sessionKey" =: sessionKey, "target" =: target]
  let memberList = responseBody res
  return memberList

sendMessage :: Text -> SendMessage -> Http Int
sendMessage path SendMessage{..} = do
  when (null messageChain) $ throwError "Invalid message!"
  sessionKey <- asks Type.sessionKey
  res <- post [path] [] $ SendMessageReq sessionKey target quote messageChain
  let sendMessageRes = responseBody res :: SendMessageRes
  assertOK sendMessageRes
  return $ messageId sendMessageRes

sendFriendMessage :: SendMessage -> Http Int
sendGroupMessage :: SendMessage -> Http Int
[sendFriendMessage, sendGroupMessage] = sendMessage <$> ["sendFriendMessage", "sendGroupMessage"]