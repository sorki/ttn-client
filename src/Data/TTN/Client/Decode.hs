module Data.TTN.Client.Decode where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import Data.TTN (Event(..), EventType(Up), Uplink(..))

import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Base64
import qualified Data.Text

import Data.Binary.Get
import Data.Either
import qualified Data.Cayene as CLPP


data Decoded =
    TempHumidity Float Float
  | Cayene [CLPP.Reading]
  deriving (Show, Eq, Ord)

decodeUplink :: Event -> [Decoded]
decodeUplink (Event Up Uplink { uplinkPayloadRaw = Just payload }) =
  tryDecode payload
decodeUplink _ = []

tryDecode :: Text -> [Decoded]
tryDecode x = rights $ map (\f -> f . unbase64 $ x )
  [ decodeTH
  , decodeCLPP
  ]


decodeTH :: ByteString -> Either String Decoded
decodeTH x = case runGetOrFail desTH x of
  Left (_, _, err) -> Left err
  Right (_, _, a)  -> Right a

desTH :: Get Decoded
desTH = do
  t <- getFloathost
  h <- getFloathost
  return $ TempHumidity t h

decodeCLPP :: ByteString -> Either String Decoded
decodeCLPP x = case CLPP.decodeMany x of
  [] -> Left "no CLPP data decoded"
  c  -> Right $ Cayene c

unbase64 :: Text -> ByteString
unbase64 =
    Data.ByteString.Lazy.Char8.fromStrict
  . Data.ByteString.Base64.decodeLenient
  . Data.ByteString.Char8.pack
  . Data.Text.unpack


