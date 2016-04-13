{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns, ScopedTypeVariables  #-}
module Network.N2O.PubSub (
    subscribe,
    byUnique,
    unsubscribe,
    newChannel,
    Entry(..),
    Connections(..),
    setState
  , SocketId
) where

import Control.Concurrent
import Data.Data (Data, gunfold, toConstr, dataTypeOf)
import Data.IxSet as I
import Data.Maybe
import Network.WebSockets as WS

import Data.Typeable (Typeable)

deriving instance Typeable Connection

unimpl = error . (++ " is unimplemented in PubSub.hs")

instance Data Connection where
    gunfold = error "WS.Connection gunfold is unimplemented in PubSub.hs"
    toConstr =  error "WS.Connection toConstr is unimplemented in PubSub.hs"
    dataTypeOf = unimpl "WS.Connection dataTypeOf"

--data Foo  = Foo Int | All deriving (Ord, Eq, Show, Typeable, Data)

data Entry a = Entry { eUser :: Maybe a, eSocketId :: SocketId, eConn :: WS.Connection} deriving (Typeable, Show, Ord, Eq, Data)

instance (Data a, Ord a) => Indexable (Entry a) where
    empty = ixSet
        [ ixGen (Proxy :: Proxy SocketId)
        , ixGen (Proxy :: Proxy (Maybe a))
        ]

instance Eq Connection where
    (==) = unimpl "(==)"

instance Ord Connection where
    compare _ _ = EQ

instance Show Connection where
    show = const "{WS.Connection}"

newtype SocketId = SocketId Int deriving (Typeable, Show, Ord, Eq, Data)

data Connections a = Connections { coSet :: IxSet (Entry a), coId :: SocketId}


initialId :: SocketId
initialId = SocketId 0

nextId :: SocketId -> SocketId
nextId (SocketId a) = SocketId (succ a)

newChannel :: (Data a, Ord a) => Connections a
newChannel = Connections I.empty initialId

subscribe :: (Data a, Ord a) => WS.Connection -> Connections a -> (Connections a, SocketId)
subscribe conn Connections {coSet, coId} = (Connections {
    coId = nextId coId, coSet = I.insert (Entry Nothing coId conn) coSet}, coId)

unsubscribe :: (Data a, Ord a) => SocketId -> Connections a -> Connections a
unsubscribe socketId (co @ Connections { coSet }) = co { coSet = I.deleteIx socketId coSet }

setState :: (Data a, Ord a) => MVar (Connections a) -> SocketId -> Maybe a -> IO ()
setState state socketId userData = modifyMVar_ state $ return . foo where
    foo co = co { coSet = mo $ coSet co }
    mo s = I.updateIx socketId (old { eUser = userData }) s where
        old = fromJust $ getOne $ getEQ socketId s

byUnique state socketId = fromJust . getOne . getEQ socketId . coSet <$> readMVar state

