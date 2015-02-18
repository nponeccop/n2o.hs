{-# LANGUAGE OverloadedStrings #-}
module Network.N2O where

import Control.Exception
import Data.BERT
import Data.Binary
import Network.WebSockets as WS

application nextMessage pending = do
    connection <- WS.acceptRequest pending
    putStrLn "accepted"
    receiveN2O connection
    nextMessage connection `catch` (\e -> print $ "Got exception " ++ show (e::WS.ConnectionException))
    putStrLn "disconnected"

receiveN2O connection = do
    message    <- WS.receiveDataMessage connection
    case message of
        WS.Text "N2O," -> return ()
        _ -> putStrLn "Protocol violation 3"

receiveMessage connection = do
    let loop = receiveMessage connection
    message    <- WS.receiveDataMessage connection
    putStrLn "message"
    case message of
         WS.Binary x -> case decode x of
			TupleTerm x -> return x
			_ -> error "Protocol violation"

         WS.Text x
			 | x == "PING" -> putStrLn "PING" >> loop
             | otherwise  -> do
                print x
                error "protocol violation 2"

