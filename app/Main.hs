{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.TTN
import Data.TTN.Client
import Data.TTN.Client.Util

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Binary.Get

import Text.Pretty.Simple

import Data.Either
import qualified Data.Cayene as CLPP

tryDecode :: T.Text -> [Decoded]
tryDecode x = rights $ map (\f -> f . unbase64 $ x )
  [ decodeTH
  , decodeCLPP
  ]

main :: IO ()
main = do
  c <- atomically $ newTChan
  forkIO $ ttnClient c
  reader c

reader chan = do
  forever $ do
    msg <- atomically $ readTChan chan
    case msg of
      Left err -> putStrLn err
      Right evt -> do
        pPrint evt
        case evt of
          Up Uplink{..} -> do
            case uplinkPayloadRaw of
              Nothing -> hPutStrLn stderr "Uplink message with no payload received"
              Just payload -> do
                pPrint $ tryDecode payload
          _ -> return ()


data Decoded =
    TempHumidity Float Float
  | Cayene [CLPP.Reading]
  deriving (Show, Eq, Ord)

decodeTH x = case runGetOrFail desTH x of
  Left (_, _, err) -> Left err
  Right (_, _, a)  -> Right a

desTH :: Get Decoded
desTH = do
  t <- getFloathost
  h <- getFloathost
  return $ TempHumidity t h

decodeCLPP :: BSL.ByteString -> Either String Decoded
decodeCLPP x = case CLPP.decodeMany x of
  [] -> Left "no CLPP data decoded"
  x  -> Right $ Cayene x
