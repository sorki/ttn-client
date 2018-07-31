{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}

module Data.TTN.Client where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.TTN
import qualified Network.MQTT as MQTT

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as BS

import Data.Ini.Config
import System.Environment
import System.Directory
import System.FilePath.Posix

data Conf = Conf {
    appId :: T.Text
  , appKey :: T.Text
  , appRouter :: T.Text
  , appRouterPort :: Integer
  }
  deriving (Eq, Show)

iniParser :: IniParser Conf
iniParser = section "app" $ do
  appId <- field "id"
  appKey <- field "key"
  appRouter <- fieldDef "router" "eu.thethings.network"
  appRouterPort <- fieldDefOf "port" number 1883
  return $ Conf {..}

parseConfCfg :: FilePath -> IO (Either String Conf)
parseConfCfg fpath = do
  rs <- T.readFile fpath
  return $ parseIniFile rs iniParser

envConfCfg :: IO (Conf)
envConfCfg = do
  menv <- lookupEnv "TTNCFG"
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


data EventType a =
    Up a
  | Down a
  | DownAcked a
  | DownSent a
  | DownScheduled a
  | Activation a
  | Create a
  | Update a
  | Delete a
  | Unknown a
  deriving (Eq, Ord, Show)

type Event = Either String (EventType Uplink)

topic:: MQTT.Topic
topic = "#"

parseType (MQTT.MqttText t) = typ
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

    sp = T.splitOn "/" t

ttnClient :: TChan Event -> IO ()
ttnClient chan = do
  conf <- envConfCfg
  ttnClientConf conf chan


ttnClientConf :: Conf -> TChan Event -> IO ()
ttnClientConf Conf{..} chan = do
  cmds <- MQTT.mkCommands
  pubChan <- newTChanIO
  let conf = (MQTT.defaultConfig cmds pubChan)
              { MQTT.cUsername = Just $ appId
              , MQTT.cHost = T.unpack appRouter
              , MQTT.cPort = fromInteger appRouterPort
              , MQTT.cPassword = Just appKey
              }

  _ <- forkIO $ do
    qosGranted <- MQTT.subscribe conf [(topic, MQTT.Handshake)]
    case qosGranted of
      [MQTT.Handshake] -> forever $ atomically (readTChan pubChan) >>= (mqttHandleChan chan)
      _ -> do
        hPutStrLn stderr $ "Wanted QoS Handshake, got " ++ show qosGranted
        exitFailure

  -- this will throw IOExceptions, how do we reconnect?
  forever $ do
    terminated <- MQTT.run conf
    hPutStrLn stderr $ "Terminated, restarting. Reason: " ++ show terminated

mqttHandleChan :: TChan Event -> MQTT.Message MQTT.PUBLISH -> IO ()
mqttHandleChan chan msg = do
    -- sometimes it's useful to ignore retained messages
    unless (MQTT.retain $ MQTT.header msg) $ do
      let t = MQTT.topic $ MQTT.body msg
          p = MQTT.payload $ MQTT.body msg

      putStrLn $ "Received on topic " ++ (show t)
      case parse p of
        Left err -> do
          case parseError p of
            Left _  -> hPutStrLn stderr $ "Invalid JSON, error: " ++ err
            Right e -> atomically $ writeTChan chan $ Left $ T.unpack $ errorMsg e

        Right u@Uplink{..} -> do
          let typ = parseType $ MQTT.fromTopic t

          atomically $ writeTChan chan $ Right $ typ u
