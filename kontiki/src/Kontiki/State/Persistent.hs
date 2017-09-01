{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.State.Persistent (
      PersistentStateT
    , runPersistentStateT
    ) where

import GHC.Generics (Generic)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Control.Monad.Logger (MonadLogger)

import Data.Default (def)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BS

import qualified Database.LevelDB as L

import qualified Proto3.Suite.Class as Proto3

import Kontiki.Raft.Classes.State.Persistent
    (MonadPersistentState(Term, Node, Entry, Index,
                          getCurrentTerm, setCurrentTerm,
                          getVotedFor, setVotedFor,
                          getLogEntry, setLogEntry))
import Kontiki.Raft.Classes.Timers (MonadTimers)

import qualified Kontiki.Protocol.Types as T

newtype PersistentStateT m a = PersistentStateT { unPersistentStateT :: ReaderT L.DB m a }
    deriving {- stock -} (Functor
    {- deriving newtype ( -} , Applicative, Monad, MonadTrans, MonadIO
    {- deriving anyclass ( -} , MonadLogger, MonadTimers)

runPersistentStateT :: L.DB -> PersistentStateT m a -> m a
runPersistentStateT db = flip runReaderT db . unPersistentStateT

currentTermKey, votedForKey :: BS8.ByteString
currentTermKey = BS8.pack "currentTerm"
votedForKey = BS8.pack "votedFor"

instance (Monad m, MonadIO m) => MonadPersistentState (PersistentStateT m) where
    type Term (PersistentStateT m) = T.Term
    type Node (PersistentStateT m) = T.Node
    type Entry (PersistentStateT m) = T.Entry
    type Index (PersistentStateT m) = T.Index

    getCurrentTerm = doGet currentTermKey
    setCurrentTerm = doPut currentTermKey

    getVotedFor = unMaybeNode <$> doGet votedForKey
    setVotedFor = doPut votedForKey . maybeNode

    getLogEntry = error "Not implemented"
    setLogEntry = error "Not implemented"

data MaybeNode = MaybeNode { maybeNodeIsNull :: Bool
                           , maybeNodeNode :: T.Node
                           }
    deriving (Show, Eq, Generic)

instance Proto3.Message MaybeNode
instance Proto3.Named MaybeNode

maybeNode :: Maybe T.Node -> MaybeNode
maybeNode = maybe (MaybeNode True def) (\node -> MaybeNode False node)

unMaybeNode :: MaybeNode -> Maybe T.Node
unMaybeNode mn = if maybeNodeIsNull mn then Nothing else Just (maybeNodeNode mn)

doGet :: (Proto3.Message a, MonadIO m)
      => BS8.ByteString
      -> PersistentStateT m a
doGet key = PersistentStateT $ do
    db <- ask
    L.get db L.defaultReadOptions key >>= \case
        Nothing -> error $ "Database not properly initialized: key " ++ show key ++ " not found"
        Just v -> do
            let v' = Proto3.fromByteString v
            -- TODO Urgh
            return $! either (error "Urgh") id v'

doPut :: (Proto3.Message a, MonadIO m)
      => BS8.ByteString
      -> a
      -> PersistentStateT m ()
doPut key a = PersistentStateT $ do
    db <- ask
    L.put db L.defaultWriteOptions key (BS.toStrict $ Proto3.toLazyByteString a)
