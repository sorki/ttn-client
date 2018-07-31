module Data.TTN.Client.Util where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Base64 as BSB

unbase64 = BSL.fromStrict . BSB.decodeLenient . BS.pack . T.unpack
