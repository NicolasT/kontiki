{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Config (
      ConfigT
    , runConfigT
    , Config(..)
    ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)

import Katip (Katip, KatipContext)

import Kontiki.Raft.Classes.Config (MonadConfig(Node, localNode))
import qualified Kontiki.Raft.Classes.RPC as RPC
import qualified Kontiki.Raft.Classes.State.Persistent as P
import qualified Kontiki.Raft.Classes.Timers as T

import qualified Kontiki.Protocol.Types as KPT

newtype Config = Config { configNode :: KPT.Node }
    deriving (Show, Eq)

newtype ConfigT m a = ConfigT { unConfigT :: ReaderT Config m a }
    deriving (Functor, Applicative, Monad,
              MonadIO, MonadTrans, MonadCatch, MonadMask, MonadThrow,
              Katip, KatipContext, T.MonadTimers, MonadLogger)

runConfigT :: Config -> ConfigT m a -> m a
runConfigT config = flip runReaderT config . unConfigT

instance Monad m => MonadConfig (ConfigT m) where
    type Node (ConfigT m) = KPT.Node

    localNode = ConfigT $ asks configNode

instance (Monad m, P.MonadPersistentState m) => P.MonadPersistentState (ConfigT m) where
    type Term (ConfigT m) = P.Term m
    type Node (ConfigT m) = P.Node m
    type Entry (ConfigT m) = P.Entry m
    type Index (ConfigT m) = P.Index m

instance (Monad m, RPC.MonadRPC m) => RPC.MonadRPC (ConfigT m) where
    type Node (ConfigT m) = RPC.Node m
    type RequestVoteRequest (ConfigT m) = RPC.RequestVoteRequest m
    type AppendEntriesRequest (ConfigT m) = RPC.AppendEntriesRequest m
