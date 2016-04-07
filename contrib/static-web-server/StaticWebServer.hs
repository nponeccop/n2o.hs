import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Text.XHtml
import Codec.Binary.UTF8.String
import Control.Exception(try,SomeException)
import System.FilePath
import Data.List(isPrefixOf)

main :: IO ()
main = serverWith defaultConfig { srvLog = stdLogger, srvPort = 8000 }
     $ \_ url request ->

  case rqMethod request of

    GET -> do
      let ext = takeExtension (url_path url)
      putStrLn $ url_path url
      mb_txt <- try (readFile $ url_path url)
      case mb_txt of
        Right a -> return $
          case ext of
            ".html" -> sendHTML OK (primHtml a)
            ".css"  -> sendStyle OK a
            ".js"   -> sendScript OK a
            _       -> sendText OK a
        Left e -> return $ sendHTML NotFound $
                  thehtml $ concatHtml
                  [ thead noHtml
                  , body $ concatHtml
                    [ toHtml "I could not find "
                    , toHtml $ exportURL url { url_type = HostRelative }
                    ]
                  ]

                 where _hack :: SomeException
                       _hack = e   -- to specify the type

    _ -> return $ sendHTML BadRequest $ toHtml "I don't understand"

sendText       :: StatusCode -> String -> Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v

sendHTML       :: StatusCode -> Html -> Response String
sendHTML s v    = insertHeader HdrContentType "text/html"
                $ sendText s (renderHtml v)

sendScript     :: StatusCode -> String -> Response String
sendScript s v  = insertHeader HdrContentType "application/x-javascript"
                $ sendText s v

sendStyle     :: StatusCode -> String -> Response String
sendStyle s v  = insertHeader HdrContentType "text/css"
                $ sendText s v

extend url = case take 6 url of
  "static" -> "../../" ++ url
  _        -> "../../sample/" ++ url
