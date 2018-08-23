module Data.TTN.Client.Decode where

import Data.TTN
import Data.TTN.Client.Util

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Binary.Get
import Data.Either
import qualified Data.Cayene as CLPP

decodeUplink (Event Up Uplink { uplinkPayloadRaw = Just payload }) = tryDecode payload
decodeUplink _ = []

tryDecode :: T.Text -> [Decoded]
tryDecode x = rights $ map (\f -> f . unbase64 $ x )
  [ decodeTH
  , decodeCLPP
  ]

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
