{-# LANGUAGE OverloadedStrings #-}
module Network.N2O where

import Control.Exception
import Data.BERT
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Monoid ((<>))
import Network.WebSockets as WS
import Control.Concurrent
import Network.N2O.PubSub

eval :: BL.ByteString -> BL.ByteString
eval x = encode $ TupleTerm [AtomTerm "io", NilTerm, showBERT x]

call fun arg = BL.concat [fun,  "('", arg, "');"]

call0 fun = fun <> "()"

application nextMessage state pending = do
    connection <- WS.acceptRequest pending
    putStrLn "accepted"
    socketId <- modifyMVar state $ return . subscribe connection
    putStrLn $ "Connected socketId = " ++ show socketId

    (receiveN2O connection >> nextMessage connection) `catch` somecatch `catch` iocatch -- `catch` (\e -> print $ "Got exception " ++ show (e::WS.ConnectionException)) `catch` somecatch
    putStrLn $ "Disconnected socketId = " ++ show socketId
    modifyMVar_ state $ return . unsubscribe socketId

somecatch :: SomeException -> IO ()
somecatch e = print "SomeException" >> print e

iocatch :: IOException -> IO ()
iocatch _ = print "IOException"

simple ip port handle state = WS.runServer ip port $ application (nextMessage $ handle state) state


nextMessage handle connection = do
    let loop = nextMessage handle connection 
    message <- receiveMessage connection
    print "Parsed"
    print message
    handle connection loop message

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

send connection = WS.sendBinaryData connection . eval
