{-# LANGUAGE TemplateHaskell   #-}

module Type where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson.TH
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Req

data MiraiConfig = MiraiConfig
  { baseUrl   :: Text
  , miraiPort :: Int
  , authKey   :: Text
  , qq        :: Int
  }

$(deriveJSON defaultOptions ''MiraiConfig)

data Config = Config
  { sessionKey  :: Text
  , miraiConfig :: MiraiConfig
  , httpConfig  :: HttpConfig
  }

type Http = ExceptT Text (ReaderT Config IO)

-- | Used to print 'Text' to terminal.
putTextLn :: Text -> IO ()
putTextLn = putStrLn . T.unpack

instance MonadHttp Http where
  handleHttpException e = throwError . T.pack . show $ e
  getHttpConfig = asks httpConfig

class ShowText a where
  showText :: a -> Text
