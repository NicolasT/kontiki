{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.State.Persistent (
      PersistentStateT
    , runPersistentStateT
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Control.Monad.Logger (MonadLogger, logDebugSH)

import qualified Data.Binary as B

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BS

import qualified Database.LevelDB as L

import Kontiki.Raft.Classes.State.Persistent
    (MonadPersistentState(Term, Node, Entry, Index,
                          getCurrentTerm, setCurrentTerm,
                          getVotedFor, setVotedFor,
                          getLogEntry, setLogEntry))

import qualified Kontiki.Types as T

newtype PersistentStateT m a = PersistentStateT { unPersistentStateT :: ReaderT L.DB m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

runPersistentStateT :: L.DB -> PersistentStateT m a -> m a
runPersistentStateT db = flip runReaderT db . unPersistentStateT

currentTermKey, votedForKey :: BS8.ByteString
currentTermKey = BS8.pack "currentTerm"
votedForKey = BS8.pack "votedFor"

instance (Monad m, MonadIO m, MonadLogger m) => MonadPersistentState (PersistentStateT m) where
    type Term (PersistentStateT m) = T.Term
    type Node (PersistentStateT m) = T.Node
    type Entry (PersistentStateT m) = ()
    type Index (PersistentStateT m) = T.Index

    getCurrentTerm = doGet currentTermKey
    setCurrentTerm = doPut currentTermKey

    getVotedFor = doGet votedForKey
    setVotedFor = doPut votedForKey

    getLogEntry = error "Not implemented"
    setLogEntry = error "Not implemented"

doGet :: (B.Binary a, Show a, MonadIO m, MonadLogger m)
      => BS8.ByteString
      -> PersistentStateT m a
doGet key = PersistentStateT $ do
    db <- ask
    L.get db L.defaultReadOptions key >>= \case
        Nothing -> error $ "Database not properly initialized: key " ++ show key ++ " not found"
        Just v -> do
            let v' = B.decode (BS.fromStrict v)
            $(logDebugSH) ("Get", key, v')
            return v'

doPut :: (B.Binary a, Show a, MonadIO m, MonadLogger m)
      => BS8.ByteString
      -> a
      -> PersistentStateT m ()
doPut key a = PersistentStateT $ do
    db <- ask
    $(logDebugSH) ("Put", key, a)
    L.put db L.defaultWriteOptions key (BS.toStrict $ B.encode a)