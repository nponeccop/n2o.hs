import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception(try,SomeException)
import System.FilePath
import Data.Map.Strict

main :: IO ()
main = serverWith defaultConfig { srvHost = "0.0.0.0", srvLog = stdLogger, srvPort = 8000 }
     $ \_ url request ->

  case rqMethod request of

    GET -> do
      let ext = takeExtension (url_path url)
      putStrLn $ url_path url
      mb_txt <- try (readFile $ url_path url)
      case mb_txt of
        Right a -> return $ sendText ext OK a
        Left e -> return $ sendText ".html" NotFound "Not found"

                 where _hack :: SomeException
                       _hack = e   -- to specify the type

    _ -> return $ sendText "" MethodNotAllowed "I don't understand"

sendText e s v    = insertHeader HdrContentType (chooseCType e)
                $ insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v

chooseCType ext = findWithDefault "application/octet-stream" ext ctm

ctm = fromList [ (".js","application/x-javascript")
                       , (".css", "text/css")
                       , (".html", "text/html")
                       ]
