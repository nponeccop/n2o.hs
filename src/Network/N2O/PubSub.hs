{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns, ScopedTypeVariables  #-}
module Network.N2O.PubSub (
    subscribe,
    byUnique,
    unsubscribe,
    newChannel,
    Connections(..)
  , setState
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

instance Eq Connection where
    (==) = unimpl "(==)"

instance Ord Connection where
    compare _ _ = EQ

instance Show Connection where
    show = const "{WS.Connection}"

newtype SocketId = SocketId Int deriving (Typeable, Show, Ord, Eq, Data)

data Connections a = Connections { coSet :: IxSet a, coId :: SocketId}


initialId :: SocketId
initialId = SocketId 0

nextId :: SocketId -> SocketId
nextId (SocketId a) = SocketId (succ a)

newChannel :: (Indexable a) => Connections a
newChannel = Connections I.empty initialId

subscribe conn emptyEntry Connections {coSet, coId} = (Connections {
    coId = nextId coId, coSet = I.insert (emptyEntry coId conn) coSet}, coId)

unsubscribe :: (Indexable a, Ord a, Typeable a) => SocketId -> Connections a -> Connections a
unsubscribe socketId (co @ Connections { coSet }) = co { coSet = I.deleteIx socketId coSet }

setState state socketId modify = modifyMVar_ state $ return . foo where
    foo co = co { coSet = mo $ coSet co }
    mo s = I.updateIx socketId (modify old) s where
        old = fromJust $ getOne $ getEQ socketId s

byUnique state socketId = fromJust . getOne . getEQ socketId . coSet <$> readMVar state

