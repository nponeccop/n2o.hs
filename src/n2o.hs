{-# LANGUAGE OverloadedStrings #-}

import Data.BERT (BERT(..), Term(..))
import Data.Binary (encode)
-- import Data.Char (isPunctuation, isSpace)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
-- import Control.Exception (catch)
import Control.Monad (forM_)
import Control.Concurrent (newMVar, modifyMVar_, readMVar)
import qualified Data.ByteString.Lazy as BL
import qualified Network.WebSockets as WS

import Network.N2O

type ClientState = Text
type Client = (ClientState, WS.Connection)
type ServerState = [Client]

--clientExists :: Client -> ServerState -> Bool
--addClient    :: Client -> ServerState -> ServerState
--removeClient :: Client -> ServerState -> ServerState
--broadcast    :: Text   -> ServerState -> IO ()
--main         :: IO ()

clientExists client          = any ((== fst client) . fst)
addClient    client clients  = client : clients
removeClient client          = filter ((/= fst client) . fst)

broadcastBinary message clients 
	= forM_ clients $ \(_, conn) -> WS.sendBinaryData conn message

bar user = eval $ call "log" (user <> " joined") <>  call "addUser" user

logon state client @ (bar, connection)  = modifyMVar_ state $ \s -> do
    let s' = addClient client s
    print $ map fst s'
    send connection "joinSession()"
    --WS.sendTextData connection $ "Welcome! Users: " `mappend` T.intercalate ", " (map fst s)
    return s'
	-- talk connection state client

main = do
    state <- newMVar []
    simple  "0.0.0.0" 9160 $ handle state

sendMessage clients text = broadcastBinary (eval $ call "log" $ fromString text) clients

handle state connection loop [AtomTerm "LOGON", AtomTerm name] = logon state (fromString name,connection)
        
handle state connection loop [AtomTerm "MSG", AtomTerm text]
    = do
            clients    <- readMVar state
            sendMessage clients text
            loop

handle state connection loop _ = putStrLn "Protocol violation"

{-
             | not (prefix `T.isPrefixOf` WS.fromLazyByteString x) ->
                WS.sendTextData connection ("Wrong announcement" :: Text)
             | any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace] ->
                WS.sendTextData connection ("Name cannot " `mappend`
                    "contain punctuation or whitespace, and " `mappend`
                    "cannot be empty" :: Text)
             | clientExists client clients ->
                WS.sendTextData connection ("User already exists" :: Text)
             | otherwise -> do
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData connection $ "Welcome! Users: " `mappend` T.intercalate ", " (map fst s)
                    broadcastBinary (bar $ BL.fromStrict $ encodeUtf8 $ fst client) s'
                    return s'
          where
             prefix     = "Hi! I am "
             client     = (T.drop (T.length prefix) (WS.fromLazyByteString x), connection)
-}

