{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Net.Ws where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Internal (unpackChars)
import qualified Data.Text as T
import Gui.Update
import Model.Contact
import Model.Message
import Net.Http (config)
import Network.WebSockets
import Pipes
import Type

runWs :: (Connection -> IO a) -> IO a
runWs = runClient (T.unpack baseUrl) miraiPort ("message?sessionKey=" <> T.unpack sessionKey)
 where
  Config{..} = config
  MiraiConfig{..} = miraiConfig

fromConnection :: MonadIO m => Connection -> Producer ByteString m ()
fromConnection conn = forever $ do
  message <- liftIO $ receiveData conn
  yield message

parseBS :: (MonadIO m) => Pipe ByteString MessageObject m ()
parseBS =
  forever $ do
    bs <- await
    let v = decode bs
    case v of
      Just obj@(FriendMessage _messageChain (Just _sender)) -> yield obj
      Just obj@(GroupMessage _messageChain (Just _sender)) -> yield obj
      Nothing -> liftIO $ putStrLn "Can't decode incoming message: " >> putStrLn (unpackChars bs)

toEvent :: (MonadIO m) => Pipe MessageObject Event m ()
toEvent =
  forever $
    await
      >>= \case
        object@(FriendMessage _messageChain (Just sender)) -> do
          yield $ ReceiveFriendMessage sender object
        object@(GroupMessage _messageChain (Just sender)) -> do
          yield $ ReceiveGroupMessage (group sender) object
        _ -> undefined

eventPipe :: MonadIO m => Connection -> Producer Event m ()
eventPipe conn = fromConnection conn >-> parseBS >-> toEvent