{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.State.Persistent (
      PersistentStateT
    , runPersistentStateT
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Control.Monad.Logger (MonadLogger)

import qualified Data.Binary as B

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BS

import qualified Database.LevelDB as L

import Kontiki.Raft.Classes.State.Persistent
    (MonadPersistentState(Term, Node, Entry, Index,
                          getCurrentTerm, setCurrentTerm,
                          getVotedFor, setVotedFor,
                          getLogEntry, setLogEntry))
import Kontiki.Raft.Classes.Timers (MonadTimers)

import qualified Kontiki.Protocol.Server as S

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
    type Term (PersistentStateT m) = S.Term
    type Node (PersistentStateT m) = S.Node
    type Entry (PersistentStateT m) = ()
    type Index (PersistentStateT m) = S.Index

    getCurrentTerm = doGet currentTermKey
    setCurrentTerm = doPut currentTermKey

    getVotedFor = doGet votedForKey
    setVotedFor = doPut votedForKey

    getLogEntry = error "Not implemented"
    setLogEntry = error "Not implemented"

doGet :: (B.Binary a, Show a, MonadIO m)
      => BS8.ByteString
      -> PersistentStateT m a
doGet key = PersistentStateT $ do
    db <- ask
    L.get db L.defaultReadOptions key >>= \case
        Nothing -> error $ "Database not properly initialized: key " ++ show key ++ " not found"
        Just v -> do
            let v' = B.decode (BS.fromStrict v)
            return $! v'

doPut :: (B.Binary a, Show a, MonadIO m)
      => BS8.ByteString
      -> a
      -> PersistentStateT m ()
doPut key a = PersistentStateT $ do
    db <- ask
    L.put db L.defaultWriteOptions key (BS.toStrict $ B.encode a)
