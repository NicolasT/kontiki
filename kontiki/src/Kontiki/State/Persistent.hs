{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Katip (Katip, KatipContext)

import Control.Monad.Catch (Exception, MonadCatch, MonadMask, MonadThrow, throwM)

import Data.Default (def)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BS

import qualified Database.LevelDB as L

import qualified Proto3.Suite.Class as Proto3

import Control.Monad.Metrics (MonadMetrics)
import qualified Control.Monad.Metrics as Metrics

import qualified Kontiki.Raft.Classes.Config as C
import Kontiki.Raft.Classes.RPC (MonadRPC)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.State.Persistent
    (MonadPersistentState(Term, Node, Entry, Index,
                          getCurrentTerm, setCurrentTerm,
                          getVotedFor, setVotedFor,
                          getLogEntry, setLogEntry,
                          lastLogEntry))
import Kontiki.Raft.Classes.Timers (MonadTimers)

import qualified Kontiki.Protocol.Types as T

newtype PersistentStateT m a = PersistentStateT { unPersistentStateT :: ReaderT L.DB m a }
    deriving {- stock -} (Functor
    {- deriving newtype ( -} , Applicative, Monad, MonadTrans, MonadIO, Katip, KatipContext
    {- deriving anyclass ( -} , MonadLogger, MonadTimers, MonadCatch, MonadMask, MonadThrow)

runPersistentStateT :: L.DB -> PersistentStateT m a -> m a
runPersistentStateT db = flip runReaderT db . unPersistentStateT

currentTermKey, votedForKey :: BS8.ByteString
currentTermKey = BS8.pack "currentTerm"
votedForKey = BS8.pack "votedFor"

instance (Monad m, MonadIO m, MonadThrow m, MonadMetrics m) => MonadPersistentState (PersistentStateT m) where
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
    lastLogEntry = return Nothing --error "Not implemented"

instance (Monad m, MonadRPC m) => MonadRPC (PersistentStateT m) where
    type Node (PersistentStateT m) = RPC.Node m
    type RequestVoteRequest (PersistentStateT m) = RPC.RequestVoteRequest m
    type AppendEntriesRequest (PersistentStateT m) = RPC.AppendEntriesRequest m

instance (Monad m, C.MonadConfig m) => C.MonadConfig (PersistentStateT m) where
    type Node (PersistentStateT m) = C.Node m

data MaybeNode = MaybeNode { maybeNodeIsNull :: !Bool
                           , maybeNodeNode :: {-# UNPACK #-} !T.Node
                           }
    deriving (Show, Eq, Generic)

instance Proto3.Message MaybeNode
instance Proto3.Named MaybeNode

maybeNode :: Maybe T.Node -> MaybeNode
maybeNode = maybe (MaybeNode True def) (MaybeNode False)

unMaybeNode :: MaybeNode -> Maybe T.Node
unMaybeNode mn = if maybeNodeIsNull mn then Nothing else Just (maybeNodeNode mn)

newtype InitializationError = InitializationError String
    deriving (Show, Eq)
instance Exception InitializationError

doGet :: (Proto3.Message a, MonadIO m, MonadThrow m, MonadMetrics m)
      => BS8.ByteString
      -> PersistentStateT m a
doGet key = PersistentStateT $ Metrics.timed' Metrics.Milliseconds "kontiki.db.get" $ do
    db <- ask
    L.get db L.defaultReadOptions key >>= \case
        Nothing -> throwM $ InitializationError $ "Database not properly initialized: key " ++ show key ++ " not found"
        Just v -> either throwM return $ Proto3.fromByteString v

doPut :: (Proto3.Message a, MonadIO m, MonadMetrics m)
      => BS8.ByteString
      -> a
      -> PersistentStateT m ()
doPut key a = PersistentStateT $ Metrics.timed' Metrics.Milliseconds "kontiki.db.put" $ do
    db <- ask
    L.put db L.defaultWriteOptions key (BS.toStrict $ Proto3.toLazyByteString a)
