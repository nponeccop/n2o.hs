{-# LANGUAGE OverloadedStrings #-}

import Data.BERT (showBERT, BERT(..), Term(..))
import Data.Binary (encode)
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type ClientState = Text
type Client = (ClientState, WS.Connection)
type ServerState = [Client]

data Message
    = Eval BL.ByteString
    | KeepAlive -- for illustration only

instance BERT Message where
    readBERT = error "client.readBERT"
    showBERT (Eval x) = showBERT x
    showBERT KeepAlive = NilTerm

--clientExists :: Client -> ServerState -> Bool
--addClient    :: Client -> ServerState -> ServerState
--removeClient :: Client -> ServerState -> ServerState
--broadcast    :: Text   -> ServerState -> IO ()
--main         :: IO ()

clientExists client          = any ((== fst client) . fst)
addClient    client clients  = client : clients
removeClient client          = filter ((/= fst client) . fst)
broadcast    message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcastBinary message clients = do
    forM_ clients $ \(_, conn) -> WS.sendBinaryData conn message

foo = BL.writeFile "foo.bert" $ bar "zorro"

call fun arg = BL.concat [fun,  "('", arg, "');"]

bar user = encode $ showBERT $ Eval $ call "log" (user `mappend` " joined") `mappend`  call "addUser" user

main = do
    state <- newMVar []
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    connection <- WS.acceptRequest pending
    message    <- WS.receiveDataMessage connection
    clients    <- liftIO $ readMVar state
    case message of
         WS.Binary x -> T.putStrLn (WS.fromLazyByteString x)
         WS.Text x
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
                talk connection state client
          where
             prefix     = "Hi! I am "
             client     = (T.drop (T.length prefix) (WS.fromLazyByteString x), connection)

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
     msg <- WS.receiveData conn
     liftIO $ readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)
