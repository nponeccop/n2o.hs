{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Control.Exception(catch,IOException)
import System.FilePath
import Data.Map.Strict
import qualified Data.ByteString.Char8 as BS

main = serverWith defaultConfig { srvHost = "0.0.0.0" } $ \_ url request -> serve (rqMethod request) (url_path url)

serve GET path = returnFileContent (BS.pack $ takeExtension path) path `catch` (\(_ :: IOException) -> return $ err_response NotFound)

serve _ _ = return $ err_response MethodNotAllowed

sendText e v
    = Response
    { rspCode = (2,0,0)
    , rspReason = "OK"
    , rspHeaders =
      [ Header HdrContentType $ findWithDefault "application/octet-stream" e ctm
      , Header HdrContentLength $ show $ BS.length v
      , Header HdrContentEncoding "UTF-8"
      ]
    , rspBody = v
    }

ctm = fromList [ (".js","application/x-javascript")
               , (".css", "text/css")
               , (".html", "text/html")
               ]

returnFileContent e a = sendText e <$> BS.readFile a
