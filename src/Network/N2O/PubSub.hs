{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, StandaloneDeriving  #-}
module Network.N2O.PubSub where

import Data.Data (Data, gfoldl, gunfold, toConstr, dataTypeOf)
import Data.IxSet as I
import Network.WebSockets as WS

import Data.Typeable (Typeable)

deriving instance Typeable Connection

unimpl = error . (++ " is unimplemented in PubSub.hs")

instance Data Connection where
    gunfold = error "WS.Connection gunfold is unimplemented in PubSub.hs"
    toConstr =  error "WS.Connection toConstr is unimplemented in PubSub.hs"
    dataTypeOf = unimpl "WS.Connection dataTypeOf"

data Foo  = Foo Int | All deriving (Ord, Eq, Show, Typeable, Data)

data Entry = Entry { eUser :: Maybe String, eFoo :: Foo, eConn :: WS.Connection} deriving (Typeable, Show, Ord, Eq, Data)

instance Indexable Entry where
    empty = ixSet
        [ ixGen (Proxy :: Proxy Foo)
        , ixGen (Proxy :: Proxy String)
        ]

instance Eq Connection where
    (==) = undefined

instance Ord Connection where
    compare = undefined

instance Show Connection where
    show = const "{WS.Connection}"

e a = Entry a All

newChannel :: IxSet Entry
newChannel = I.empty

subscribe = I.insert 

unsubscribe = I.deleteIx 

broadcast = I.toList . getEQ All


