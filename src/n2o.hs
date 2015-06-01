{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main (main) where
import Data.BERT (Term(..))
import Data.Maybe
import Data.Monoid ((<>))
import Data.String (fromString)
import Control.Monad (forM_)
import Control.Concurrent (newMVar, readMVar)
import qualified Network.WebSockets as WS

import Network.N2O
import Network.N2O.PubSub
import Data.IxSet as I


broadcastBinary message clients 
    = forM_ clients $ \(Entry {eConn}) -> WS.sendBinaryData eConn message

-- bar user = eval $ call "log" (user <> " joined") <>  call "addUser" user

    --WS.sendTextData connection $ "Welcome! Users: " `mappend` T.intercalate ", " (map fst s)
    -- talk connection state client
    --

main = do
    state <- newMVar newChannel
    putStrLn "Started"
    simple  "0.0.0.0" 9160 handle state
    print "ok"

loggedOff :: Maybe String
loggedOff = Nothing

sendMessage text = broadcastBinary $ eval $ call "log" $ fromString text

loggedOn state = toList . getGT loggedOff . coSet <$> readMVar state

handle state connection socketId [AtomTerm "LOGON", AtomTerm name]
    = do
        send connection $ call "log" "ahaha" <> call "joinSession" ""
        setState state socketId $ Just $ fromString name
        clients <- loggedOn state
        let foo = concatMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
        broadcastBinary (eval $ call "$('#users').html" (fromString foo) <> call "log" (fromString name <> " joined")) clients
        
handle state _connection socketId [AtomTerm "MSG", AtomTerm text]
    = do
        clients <- loggedOn state
        sendMessage text clients
        print clients

handle state _connection _ [AtomTerm "N2O_DISCONNECT"]
    = do
        print "N2O_DISCONNECT"
        clients <- loggedOn state
        let foo = concatMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
        broadcastBinary (eval $ call "$('#users').html" (fromString foo) <> call "log" ("Someone" <> " left")) clients

handle _state _connection _ _ = putStrLn "Protocol violation"

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

