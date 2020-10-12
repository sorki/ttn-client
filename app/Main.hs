{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}

module Main where

import Data.TTN
import Data.TTN.Client
import Data.TTN.Client.Decode

import qualified Data.Text

import Data.Time

main :: IO ()
main = do
  withTTN $ \e -> do
    case e of
      (ClientError str) -> putStrLn $ "ClientErr " ++ str
      (Event etype Uplink{..}) -> do
        case etype of
          Up -> do
            n <- getZonedTime
            putStrLn $ unwords [
                formatTime defaultTimeLocale "%F %X" n
              , maybe "" Data.Text.unpack uplinkDevId
              , maybe "" (('#':) . show) uplinkCounter
              , maybe "" (pure "(retry)") uplinkIsRetry
              , (show $ decodeUplink e)
              ]
          _ -> return ()

-- unused
cvt :: TimeZone -> ZonedTime -> LocalTime
cvt tz t = utcToLocalTime tz $ zonedTimeToUTC t

renderTime :: TimeZone -> ZonedTime -> String
renderTime tz t = formatTime defaultTimeLocale "%F %X" (cvt tz t)
