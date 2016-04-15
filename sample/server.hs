{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main (main) where
import Control.Concurrent.MVar
import Data.BERT (Term(..))
import Data.Char (isPunctuation, isSpace)
import Data.Data hiding (Proxy)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import Language.HJavaScript.Syntax hiding (call)

import Network.N2O
import Network.N2O.Jq
import Network.N2O.PubSub
import qualified Network.WebSockets as WS
import Data.IxSet as I

main :: IO ()
main = do
    pubSub <- newMVar I.empty
    runServer "0.0.0.0" 9160 (handle pubSub) emptyEntry

emptyEntry = Entry Nothing

data Entry = Entry { eUser :: Maybe T.Text, eSocketId :: SocketId, eConn :: WS.Connection} deriving (Typeable, Show, Ord, Eq, Data)

instance Indexable Entry where
    empty = ixSet
        [ ixGen (Proxy :: Proxy SocketId)
        , ixGen (Proxy :: Proxy (Maybe T.Text))
        ]

loggedOff :: Maybe T.Text
loggedOff = Nothing

chatLog :: T.Text -> Stmt ()
chatLog msg = ExpStmt $ JCall (JConst "insertBottom") (JString "p", JString $ T.unpack msg, JString "messages")

sendMessage state text = map eConn <$> loggedOn state >>= broadcast (T.pack $ show $ toBlock $ chatLog text)

loggedOn state = toList . getGT loggedOff . coSet <$> readMVar state

alert entry msg = send (eConn entry) $ call "alert" msg

invalidName :: T.Text -> Bool
invalidName name = T.null name || T.any isPunctuation name || T.any isSpace name

clientExists state name = not . I.null . getEQ (Just name) . coSet <$> readMVar state

updateUsers state msg = do
    clients <- loggedOn state
    let allUsersHtml = foldMap ((\x -> "<li>" <> x <> "</li>") . fromJust . eUser) clients
    broadcast (assign "qi('users')" allUsersHtml) (map eConn clients)
    sendMessage state msg

handle :: MVar (I.IxSet ChannelData) -> MVar (Connections Entry) -> Entry -> [Term] -> IO ()
handle pubSub state entry [AtomTerm "LOGON", BinaryTerm name]
  | invalidName $ b2t name = alert entry "Name cannot contain punctuation or whitespace, and cannot be empty"
    | otherwise = do
        let dname = b2t name
        print $ "login: " <> dname
        ce <- clientExists state dname
        if ce 
            then alert entry "User already exists"
            else do
                setState state (eSocketId entry) $ \entry -> entry { eUser = Just dname }
                send (eConn entry) $ T.pack $ show $ foldl Sequence EmptyBlock
                    [ jqHide "join-section"
                    , jqShow "chat-section"
                    , jqShow "users-section"
                    ]
                sub pubSub (eSocketId entry) All
                updateUsers state $ dname <> " joined"

handle pubSub state entry [AtomTerm "MSG", BinaryTerm text]
    = pub pubSub All state $ T.pack $ show $ chatLog $ getUser entry <> ": " <> b2t text

handle pubSub state entry [AtomTerm "N2O_DISCONNECT"]
    = do
        unsubEverything pubSub (eSocketId entry)
        updateUsers state $ getUser entry <> " disconnected"

handle _ _state _entry _ = putStrLn "Protocol violation"

getUser :: Entry -> T.Text
getUser entry = fromMaybe "Someone" $ eUser entry

data ChannelName = All deriving (Data, Ord, Eq, Show)

data ChannelData = ChannelData
    { chSocket :: SocketId
    , _chChannel :: ChannelName
    } deriving (Data, Typeable, Ord, Eq)

instance Indexable ChannelData where
    empty = ixSet
        [ ixGen (I.Proxy :: I.Proxy SocketId)
        , ixGen (I.Proxy :: I.Proxy ChannelName)
        ]

sub pubSub channel id = modifyMVar_ pubSub $ return . I.insert (ChannelData channel id)

pub pubSub channel state text = do
    socketIds <- map chSocket . I.toList . (I.@= channel) <$> readMVar pubSub
    l <- map eConn . I.toList . (I.@+ socketIds) . coSet <$> readMVar state
    broadcast text l

unsubEverything pubSub id = modifyMVar_ pubSub $ return . I.deleteIx id
