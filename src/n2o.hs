{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main (main) where
import Data.BERT (Term(..))
import Data.Char (isPunctuation, isSpace)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import Language.HJavaScript.Syntax hiding (call)

import Network.N2O
import Network.N2O.Jq
import Network.N2O.PubSub (setState, Entry(..), Connections(..))
import Data.IxSet as I
import GHC.MVar

main :: IO ()
main = runServer "0.0.0.0" 9160 handle loggedOff

loggedOff :: Maybe T.Text
loggedOff = Nothing

chatLog :: T.Text -> Stmt ()
chatLog msg = ExpStmt $ JCall (JConst "insertBottom") (JString "p", JString $ T.unpack msg, JString "messages")

joinSession = foldl Sequence EmptyBlock $
    [ jqHide "join-section"
    , jqShow "chat-section"
    , jqShow "users-section"
    ]

sendMessage text foo = do
    broadcast (T.pack $ show $ toBlock $ chatLog text) foo
    print     $ T.pack $ show $ toBlock $ chatLog text

loggedOn state = toList . getGT loggedOff . coSet <$> readMVar state

alert entry msg = send entry $ call "alert" msg

invalidName :: T.Text -> Bool
invalidName name = T.null name || T.any isPunctuation name || T.any isSpace name

clientExists state name = not . I.null . getEQ (Just name) . coSet <$> readMVar state

updateUsers state = do
    clients <- loggedOn state
    let allUsersHtml = foldMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
    broadcast (assign "qi('users')" allUsersHtml) clients

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
                send entry $ T.pack $ show $ joinSession

                setState state (eSocketId entry) $ Just dname
                updateUsers state
                loggedOn state >>= sendMessage (dname <> " joined")

handle state entry [AtomTerm "MSG", BinaryTerm text]
    = loggedOn state >>= sendMessage (getUser entry <> ": " <> b2t text)

handle state entry [AtomTerm "N2O_DISCONNECT"]
    = do
        updateUsers state
        loggedOn state >>= sendMessage (call "log" (getUser entry <> " disconnected"))

handle _state _entry _ = putStrLn "Protocol violation"

getUser :: Entry T.Text -> T.Text
getUser entry = fromMaybe "Someone" $ eUser entry
