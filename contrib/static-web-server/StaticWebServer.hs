{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception(catch,IOException)
import System.FilePath
import Data.Map.Strict
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = serverWith defaultConfig { srvHost = "0.0.0.0", srvLog = stdLogger, srvPort = 8000 }
     $ \_ url request ->

  case rqMethod request of

    GET -> do
      let ext = takeExtension (url_path url)
      putStrLn $ url_path url
      returnFileContent (BS.pack ext) url `catch` (\(_ :: IOException) -> return $ err_response NotFound)

    _ -> return $ err_response MethodNotAllowed

sendText :: BS.ByteString -> StatusCode -> BS.ByteString -> Response BS.ByteString
sendText e s v    = insertHeader HdrContentType (chooseCType e)
                $ insertHeader HdrContentLength (show (BS.length v))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response BS.ByteString) { rspBody = v }

chooseCType ext = findWithDefault "application/octet-stream" ext ctm

ctm = fromList [ (".js","application/x-javascript")
               , (".css", "text/css")
               , (".html", "text/html")
               ]

returnFileContent :: BS.ByteString -> URL -> IO (Response BS.ByteString)
returnFileContent e a = do
  b <- BS.readFile $ url_path a
  return $ sendText e OK b
