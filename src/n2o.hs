{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main (main) where
import Data.BERT (Term(..))
import Data.Char (isPunctuation, isSpace)
import Data.Maybe
import Data.Monoid ((<>))
import Data.String (fromString)
import Control.Concurrent (newMVar, readMVar)

import Network.N2O
import Network.N2O.PubSub
import Data.IxSet as I

main = do
    state <- newMVar newChannel
    putStrLn "Started"
    simple  "0.0.0.0" 9160 handle state

loggedOff :: Maybe String
loggedOff = Nothing

sendMessage text = broadcastBinary $ call "log" $ fromString text

loggedOn state = toList . getGT loggedOff . coSet <$> readMVar state

alert entry msg = send entry $ call "alert" msg

invalidName name = Prelude.null name || any isPunctuation name || any isSpace name

clientExists state name = not . I.null . getEQ (Just name) . coSet <$> readMVar state

handle state entry [AtomTerm "LOGON", AtomTerm name]
    | invalidName name = alert entry "Name cannot contain punctuation or whitespace, and cannot be empty"
    | otherwise = do
        ce <- clientExists state name
        if ce 
            then alert entry "User already exists"
            else do
                send entry $ call "joinSession" ""
                setState state (eSocketId entry) $ Just $ fromString name
                clients <- loggedOn state
                let foo = concatMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
                broadcastBinary (call "$('#users').html" (fromString foo) <> call "log" (fromString name <> " joined")) clients
        
handle state _entry [AtomTerm "MSG", AtomTerm text]
    = do
        clients <- loggedOn state
        sendMessage text clients

handle state entry [AtomTerm "N2O_DISCONNECT"]
    = do
        clients <- loggedOn state
        let foo = concatMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
        broadcastBinary (call "$('#users').html" (fromString foo) <> call "log" (fromMaybe "Someone" (fromString <$> eUser entry) <> " disconnected")) clients

handle _state _entry _ = putStrLn "Protocol violation"


