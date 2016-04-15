module Network.N2O.Session where

import Crypto.Nonce
import Data.ByteString.Char8
 
generator :: IO Generator
generator = new

newSession :: Generator -> IO String
newSession = fmap unpack . nonce128url

-- encode $ toStrict $ n <> bytestringDigest (hmacSha1 k n)


