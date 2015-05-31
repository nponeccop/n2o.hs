{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

import Data.BERT (BERT(..), Term(..))
import Data.Binary (encode)
-- import Data.Char (isPunctuation, isSpace)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
-- import Control.Exception (catch)
import Control.Monad (forM_, liftM)
import Control.Concurrent (newMVar, modifyMVar_, readMVar)
import qualified Data.ByteString.Lazy as BL
import qualified Network.WebSockets as WS

import Network.N2O
import Network.N2O.PubSub
import Data.IxSet as I

type ClientState = Text
type Client = Entry
type ServerState = IxSet Client

clientExists :: () -> Bool
clientExists client = error "aaa" -- M.member (fst client)
addClient    client = error "bbb" -- uncurry M.insert
removeClient client = error "ccc" -- M.delete (fst client)

broadcastBinary message clients 
    = forM_ clients $ \(Entry {eConn}) -> WS.sendBinaryData eConn message

bar user = eval $ call "log" (user <> " joined") <>  call "addUser" user

justLog = eval . call "log"

logon state client = do 
    modifyMVar_ state $ return . subscribe client
    WS.sendBinaryData (eConn client) $ eval $ call "log" "ahaha" <> call "joinSession" ""

    --WS.sendTextData connection $ "Welcome! Users: " `mappend` T.intercalate ", " (map fst s)
    -- talk connection state client

main = do
    state <- newMVar newChannel
    putStrLn "Started"
    simple  "0.0.0.0" 9160 $ handle state
    print "ok"


sendMessage clients text = broadcastBinary (eval $ call "log" $ fromString text) clients

handle state connection loop [AtomTerm "LOGON", AtomTerm name] = logon state (Entry (Just $ fromString name) All connection) >> loop
        
handle state connection loop [AtomTerm "MSG", AtomTerm text]
    = do
            clients    <- toList <$> readMVar state
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

