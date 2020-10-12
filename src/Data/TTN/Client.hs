{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}

module Data.TTN.Client (
    ttnClient
  , ttnClientConf
  , withTTN
  , Conf(..)
  , envConfCfg
  , parseConfCfg
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (Handler (..), IOException, catches)
import Control.Monad
import Data.Text (Text)

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.TTN
import Network.MQTT.Client
import qualified Network.URI

import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.ByteString.Lazy

import Data.Ini.Config
import System.Directory
import System.FilePath.Posix
import qualified System.Environment

data Conf = Conf {
    appId         :: Text
  , appKey        :: Text
  , appRouter     :: Text
  , appRouterPort :: Integer
  }
  deriving (Eq, Show)

iniParser :: IniParser Conf
iniParser = section "app" $ do
  appId         <- field "id"
  appKey        <- field "key"
  appRouter     <- fieldDef "router" "eu.thethings.network"
  appRouterPort <- fieldDefOf "port" number 1883
  return $ Conf {..}

-- | Try parsing config from given 'FilePath'
parseConfCfg :: FilePath -> IO (Either String Conf)
parseConfCfg fpath = do
  rs <- Data.Text.IO.readFile fpath
  return $ parseIniFile rs iniParser

-- | Try loading config from location in @TTNCFG@ environment variable
-- or from @~/.ttn/config@
envConfCfg :: IO (Conf)
envConfCfg = do
  menv <- System.Environment.lookupEnv "TTNCFG"
  case menv of
    Nothing -> do
      udir <- getHomeDirectory
      let userConf = udir </> ".ttn" </> "config"
      hasCfg <- doesFileExist userConf
      case hasCfg of
        False -> putStrLn ("Unable to load config: no ~/.ttn/config or TTNCFG env variable set") >> exitFailure
        True -> do
          res <- parseConfCfg userConf
          case res of
            Left err -> putStrLn ("Unable to parse config: " ++ err) >> exitFailure
            Right cfg -> return cfg
    Just env -> do
      res <- parseConfCfg env
      case res of
        Left err -> putStrLn ("Unable to parse config: " ++ err) >> exitFailure
        Right cfg -> return cfg

parseType :: Text -> EventType
parseType t = typ
  where
    typ = case drop 3 sp of
      ["up"]                         -> Up
      ["down"]                       -> Down
      ["events", "down", "acks"]     -> DownAcked
      ["events", "down", "sent"]     -> DownSent
      ["events", "down", "schedule"] -> DownScheduled
      ["events", "activations"]      -> Activation
      ["events", "create"]           -> Create
      ["events", "update"]           -> Update
      ["events", "delete"]           -> Delete
      _                              -> Unknown

    sp = Data.Text.splitOn "/" t

-- | Try to load config from default locations and start actual client
ttnClient :: TChan Event -> IO ()
ttnClient chan = do
  conf <- envConfCfg
  ttnClientConf conf chan

-- | Start client with custom `Conf` config
ttnClientConf :: Conf -> TChan Event -> IO ()
ttnClientConf Conf{..} chan = do
  let (Just uri) = Network.URI.parseURI
        $ Data.Text.unpack
        $ Data.Text.concat [ "mqtt://", appId, ":", appKey, "@", appRouter ]

  putStrLn $ "Connecting to " ++ show uri
  mc <- connectURI mqttConfig { _msgCB = SimpleCallback msgReceived } uri

  putStrLn "Connected!"
  void $ subscribe mc [("#", subOptions)] mempty
  waitForClient mc
  where
    msgReceived _ topic msg _p = do
      case parse (Data.ByteString.Lazy.toStrict msg) of
        Left err -> do
          case parseError (Data.ByteString.Lazy.toStrict msg) of
            Left _  -> hPutStrLn stderr $ "Invalid JSON, error: " ++ err
            Right e -> atomically
              $ writeTChan chan
              $ ClientError
              $ Data.Text.unpack
              $ errorMsg e

        Right x -> do
          atomically $ writeTChan chan $ Event (parseType topic) x

withTTN :: (Event -> IO a) -> IO b
withTTN act = do
  c <- newTChanIO
  void $ async $ forever $ do
    msg <- atomically $ readTChan c
    act msg

  forever $ catches (ttnClient c)
    [ Handler (\(ex :: MQTTException) -> handler (show ex))
    , Handler (\(ex :: IOException) -> handler (show ex)) ]

  where
    handler e = putStrLn ("ERROR: " <> e) >> threadDelay 1000000
