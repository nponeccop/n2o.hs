{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main (main) where
import Data.BERT (Term(..))
import Data.Char (isPunctuation, isSpace)
import Data.Maybe
import Data.Monoid ((<>))
import Data.String (fromString)
import Control.Concurrent (readMVar)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T

import Network.N2O
import Network.N2O.PubSub (setState, Entry(..), Connections(..))
import Data.IxSet as I

main :: IO ()
main = runServer "0.0.0.0" 9160 handle loggedOff

loggedOff :: Maybe String
loggedOff = Nothing

sendMessage text = broadcast $ call "log" $ text

loggedOn state = toList . getGT loggedOff . coSet <$> readMVar state

alert entry msg = send entry $ call "alert" msg

invalidName :: T.Text -> Bool
invalidName name = T.null name || T.any isPunctuation name || T.any isSpace name

clientExists state name = not . I.null . getEQ (Just name) . coSet <$> readMVar state

handle state entry [AtomTerm "LOGON", BinaryTerm name]
  | invalidName $ T.pack $ lazyDecodeUtf8 name = alert entry "Name cannot contain punctuation or whitespace, and cannot be empty"
    | otherwise = do
        let dname = lazyDecodeUtf8 name
        print $ "login: " <> dname
        ce <- clientExists state dname
        if ce 
            then alert entry "User already exists"
            else do
                send entry $ call "joinSession" ""
                setState state (eSocketId entry) $ Just dname
                clients <- loggedOn state
                let foo = lazyEncodeUtf8 $ concatMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
                broadcast (call "$('#users').html" foo <> call "log" (name <> " joined")) clients
        
handle state entry [AtomTerm "MSG", BinaryTerm text]
    = do
        clients <- loggedOn state
        let ctext = getEncodedUser entry <> ": " <> text
        sendMessage ctext clients

handle state entry [AtomTerm "N2O_DISCONNECT"]
    = do
        clients <- loggedOn state
        let foo = concatMap ((\x -> "<li>" <> fromString x <> "</li>") . fromJust . eUser) clients
        broadcast (call "$('#users').html" (fromString foo) <> call "log" (getUser entry <> " disconnected")) clients

handle _state _entry _ = putStrLn "Protocol violation"

getUser entry = fromMaybe "Someone" $ fromString <$> eUser entry
getEncodedUser = lazyEncodeUtf8 . getUser

lazyDecodeUtf8 = T.unpack . decodeUtf8 . toStrict
lazyEncodeUtf8 = fromStrict . encodeUtf8 . T.pack
