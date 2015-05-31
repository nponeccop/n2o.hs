{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns  #-}
module Network.N2O.PubSub where

import Data.Data (Data, gunfold, toConstr, dataTypeOf)
import Data.IxSet as I
import Network.WebSockets as WS

import Data.Typeable (Typeable)

deriving instance Typeable Connection

unimpl = error . (++ " is unimplemented in PubSub.hs")

instance Data Connection where
    gunfold = error "WS.Connection gunfold is unimplemented in PubSub.hs"
    toConstr =  error "WS.Connection toConstr is unimplemented in PubSub.hs"
    dataTypeOf = unimpl "WS.Connection dataTypeOf"

--data Foo  = Foo Int | All deriving (Ord, Eq, Show, Typeable, Data)

data Entry = Entry { eUser :: Maybe String, eSocketId :: SocketId, eConn :: WS.Connection} deriving (Typeable, Show, Ord, Eq, Data)

instance Indexable Entry where
    empty = ixSet
        [ ixGen (Proxy :: Proxy SocketId)
        , ixGen (Proxy :: Proxy (Maybe String))
        ]

instance Eq Connection where
    (==) = unimpl "(==)"

instance Ord Connection where
    compare _ _ = EQ

instance Show Connection where
    show = const "{WS.Connection}"

type SocketId = Int

data Connections = Connections { coSet :: IxSet Entry, coId :: SocketId }


initialId :: SocketId
initialId = 0

nextId :: SocketId -> SocketId
nextId = succ

newChannel :: Connections
newChannel = Connections I.empty initialId

subscribe :: WS.Connection -> Connections -> (Connections, SocketId)
subscribe conn (Connections {coSet, coId}) = (Connections {
    coId = nextId coId, coSet = I.insert (Entry Nothing coId conn) coSet}, coId)

unsubscribe :: SocketId -> Connections -> Connections
unsubscribe socketId (co @ Connections { coSet }) = co { coSet = I.deleteIx socketId coSet }


