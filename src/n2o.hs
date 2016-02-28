{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main (main) where
import Data.BERT (Term(..))
import Data.Char (isPunctuation, isSpace)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T

import Network.N2O
import Network.N2O.PubSub (setState, Entry(..), Connections(..))
import Data.IxSet as I
import GHC.MVar

main :: IO ()
main = runServer "0.0.0.0" 9160 handle loggedOff

loggedOff :: Maybe T.Text
loggedOff = Nothing

sendMessage text = broadcast $ call "log" $ text

loggedOn state = toList . getGT loggedOff . coSet <$> readMVar state

alert entry msg = send entry $ call "alert" msg

invalidName :: T.Text -> Bool
invalidName name = T.null name || T.any isPunctuation name || T.any isSpace name

clientExists state name = not . I.null . getEQ (Just name) . coSet <$> readMVar state

handle :: MVar (Connections T.Text) -> Entry T.Text -> [Term] -> IO ()

handle state entry [AtomTerm "LOGON", BinaryTerm name]
  | invalidName $ b2t name = alert entry "Name cannot contain punctuation or whitespace, and cannot be empty"
    | otherwise = do
        let dname = b2t name
        print $ "login: " <> dname
        ce <- clientExists state dname
        if ce 
            then alert entry "User already exists"
            else do
                send entry $ call "joinSession" ""
                setState state (eSocketId entry) $ Just dname
                clients <- loggedOn state
                let foo = foldMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
                broadcast (call "$('#users').html" foo <> call "log" dname <> " joined") clients
        
handle state entry [AtomTerm "MSG", BinaryTerm text]
    = do
        clients <- loggedOn state
        let ctext = getUser entry <> ": " <> b2t text
        sendMessage ctext clients

handle state entry [AtomTerm "N2O_DISCONNECT"]
    = do
        clients <- loggedOn state
        let foo = foldMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
        broadcast (call "$('#users').html" foo <> call "log" (getUser entry <> " disconnected")) clients

handle _state _entry _ = putStrLn "Protocol violation"

getUser :: Entry T.Text -> T.Text
getUser entry = fromMaybe "Someone" $ eUser entry
