# ttn-client

Receive and decode data from TTN MQTT API.

See https://www.thethingsnetwork.org/docs/applications/mqtt/api.html

## Command line utility

`ttnc` provides a simple command line utility to dump and pretty print incoming data.
It can also decode two types of payload

 * Cayene low power protocol format (via [cayene-lpp](https://github.com/sorki/cayene-lpp))
 * Simple temperature humidity (two big endian packed floats)

### Example output

```haskell
-- Received on topic "basetest/devices/tester/up"
Up
    ( Uplink
        { uplinkConfig = Nothing
        , uplinkGatewayId = Nothing
        , uplinkDevId = Just "tester"
        , uplinkPayload = Nothing
        , uplinkCounter = Just 1365.0
        , uplinkIsRetry = Nothing
        , uplinkMetadata = Just
            ( Metadata
                { metadataTime = Just ( TTNZonedTime { unwrap = 2018-07-31 20:43:29.331580984 +0000 } )
                , metadataFrequency = Just 868.1
                , metadataModulation = Just "LORA"
                , metadataDataRate = Just "SF12BW125"
                , metadataBitRate = Nothing
                , metadataAirtime = Just 1.482752e9
                , metadataCodingRate = Just "4/5"
                , metadataGateways =
                    [ GatewaysElt
                        { gatewaysEltGtwId = Just "eui-b827ebffffc6e42c"
                        , gatewaysEltGtwTrusted = Nothing
                        , gatewaysEltTimestamp = Just 2974169876
                        , gatewaysEltFineTimestamp = Nothing
                        , gatewaysEltFineTimestampEncrypted = Nothing
                        , gatewaysEltTime = Just ( TTNZonedTime { unwrap = 2018-07-31 20:43:29.305615 +0000 } )
                        , gatewaysEltAntenna = Nothing
                        , gatewaysEltChannel = 0.0
                        , gatewaysEltRSSI = -99.0
                        , gatewaysEltSNR = 9.8
                        , gatewaysEltRFChain = 1
                        , gatewaysEltLatitude = Just 49.560596
                        , gatewaysEltLongitude = Just 16.072577
                        , gatewaysEltAltitude = Just 601
                        , gatewaysEltAccuracy = Nothing
                        , gatewaysEltSource = Nothing
                        }
                    ]
                }
            )
        , uplinkPayloadRaw = Just "UgLJQVAyEkI="
        , uplinkMessage = Nothing
        , uplinkAppId = Just "basetest"
        , uplinkConfirmed = Just True
        , uplinkHardwareSerial = Just "0004A30B001E0D4D"
        , uplinkPort = Just 1.0
        }
    )
[ TempHumidity 25.126133 36.549133
, Cayene
    [
        ( 82
        , AnalogIn ( -140.15 )
        )
    ]
]
-- Received on topic "basetest/devices/tester/events/down/sent"
DownSent
    ( Uplink
        { uplinkConfig = Just
            ( Config
                { configFrequency = Just 8.69525e8
                , configDataRate = Just "SF9BW125"
                , configCounter = Just 1367.0
                , configAirtime = Just 1.44384e8
                , configPower = Just 27.0
                , configModulation = Just "LORA"
                }
            )
        , uplinkGatewayId = Just "eui-b827ebffffc6e42c"
        , uplinkDevId = Nothing
        , uplinkPayload = Just "YPIsASYgVwVGo1QT"
        , uplinkCounter = Nothing
        , uplinkIsRetry = Nothing
        , uplinkMetadata = Nothing
        , uplinkPayloadRaw = Nothing
        , uplinkMessage = Just
            ( Message
                { messageDevId = "tester"
                , messageAppId = "basetest"
                , messagePort = 0.0
                }
            )
        , uplinkAppId = Nothing
        , uplinkConfirmed = Nothing
        , uplinkHardwareSerial = Nothing
        , uplinkPort = Nothing
        }
    )
```

## Usage

`ttnClient` function takes channel which it uses to feed data to your application. Barebones application example:

```haskell
import Data.TTN
import Data.TTN.Client

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
      Right evt -> print evt
```
