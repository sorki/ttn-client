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
import Data.TTN.Client.Decode

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Pretty.Simple

main :: IO ()
main = do
  c <- atomically $ newTChan
  forkIO $ ttnClient c
  reader c

reader chan = do
  forever $ do
    msg <- atomically $ readTChan chan
    pPrint msg
    pPrint $ decodeUplink msg
