{-# LANGUAGE ScopedTypeVariables #-}
import Network.HTTP.Server
import Network.URL
import System.Directory
import System.FilePath
import Data.Map.Strict
import qualified Data.ByteString as BS

main = serverWith defaultConfig { srvHost = "0.0.0.0" }
    $ \_ url request -> serve (rqMethod request) (url_path url)

serve GET path = do
    f <- doesFileExist path
    if f
        then BS.readFile path >>= sendFile path
        else return $ err_response NotFound

serve _ _ = return $ err_response MethodNotAllowed

sendFile filePath fileData
    = return $ Response
    { rspCode = (2,0,0)
    , rspReason = "OK"
    , rspHeaders =
      [ Header HdrContentType $ findWithDefault "application/octet-stream" (takeExtension filePath) ctm
      , Header HdrContentLength $ show $ BS.length fileData
      , Header HdrContentEncoding "UTF-8"
      ]
    , rspBody = fileData
    }

ctm = fromList
    [ (".js"  , "application/x-javascript")
    , (".css" , "text/css")
    , (".html", "text/html")
    ]

