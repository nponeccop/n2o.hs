{-# LANGUAGE OverloadedStrings #-}
module Network.N2O (
    b2t,t2b,
    send,
    call,
    Network.N2O.runServer,
    broadcast
) where

import Control.Exception
import Data.BERT
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Network.WebSockets as WS hiding (send)
import Control.Concurrent
import Control.Monad
import Network.N2O.PubSub
import qualified Data.Text as T
import Data.Text.Encoding

b2t :: BL.ByteString -> T.Text
b2t = decodeUtf8 . BL.toStrict

t2b :: T.Text -> BL.ByteString
t2b = BL.fromStrict . encodeUtf8

eval :: BL.ByteString -> BL.ByteString
eval x = encode $ TupleTerm [AtomTerm "io", NilTerm, showBERT x]

call fun arg = t2b $ T.concat [fun,  "('", arg, "');"]

-- call0 fun = fun <> "()"

application nextMessage handle state pending = do
    connection <- WS.acceptRequest pending
    putStrLn "accepted"
    socketId <- modifyMVar state $ return . subscribe connection
    putStrLn $ "Connected socketId = " ++ show socketId

    (receiveN2O connection >> forever (nextMessage handle state connection socketId)) `catch` somecatch `catch` iocatch -- `catch` (\e -> print $ "Got exception " ++ show (e::WS.ConnectionException)) `catch` somecatch
    putStrLn $ "Disconnected socketId = " ++ show socketId

    entry <- byUnique state socketId
    modifyMVar_ state $ return . unsubscribe socketId
    handle state entry [AtomTerm "N2O_DISCONNECT"]

somecatch :: SomeException -> IO ()
somecatch e = print "SomeException" >> print e

iocatch :: IOException -> IO ()
iocatch _ = print "IOException"

runServer ip port handle userState = do
    print "Started"
    state <- newMVar newChannel
    WS.runServer ip port $ application nextMessage handle state

nextMessage handle state connection socketId = do
    message <- receiveMessage connection
    print "Parsed"
    print message
    entry <- byUnique state socketId
    handle state entry message

--simpleApp x = application simpleLoop x 

receiveN2O connection = do
    message    <- WS.receiveDataMessage connection
    case message of
        WS.Text "N2O," -> return ()
        WS.Binary _ -> putStrLn "Protocol violation 4"
        WS.Text x -> putStrLn "Protocol violation 3" >> print x

receiveMessage connection = do
    let loop = receiveMessage connection
    message    <- WS.receiveDataMessage connection
    putStrLn "message"
    print message
    case message of
         WS.Binary x -> print "Decoding" >> case decode x of
            TupleTerm x -> print "Decoded" >> return x
            _ -> print "haha" >> error "Protocol violation"

         WS.Text x
             | x == "PING" -> putStrLn "PING" >> loop
             | otherwise  -> do
                print x
                error "protocol violation 2"

send entry = WS.sendBinaryData (eConn entry) . eval

broadcast message
    = mapM_ $ \entry -> send entry message

