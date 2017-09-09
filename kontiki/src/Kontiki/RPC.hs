{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kontiki.RPC (
      RPCT
    , runRPCT
    ) where

import Control.Monad.Trans.Identity (IdentityT, mapIdentityT, runIdentityT)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader.Class (MonadReader(ask, local))
import Control.Monad.Trans.Class (MonadTrans, lift)

import Katip (Katip(getLogEnv, localLogEnv), KatipContext(getKatipContext, localKatipContext, getKatipNamespace, localKatipNamespace))

import Kontiki.Raft.Classes.RPC (MonadRPC(Node, RequestVoteRequest, AppendEntriesRequest, broadcastRequestVoteRequest, sendAppendEntriesRequest))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import qualified Kontiki.Raft.Classes.Timers as T

import qualified Kontiki.Protocol.Types as KPT

newtype RPCT m a = RPCT { unRPCT :: IdentityT m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadCatch, MonadMask, MonadThrow, MonadLogger)

instance Monad m => MonadRPC (RPCT m) where
    type Node (RPCT m) = KPT.Node
    type RequestVoteRequest (RPCT m) = KPT.RequestVoteRequest
    type AppendEntriesRequest (RPCT m) = KPT.AppendEntriesRequest

    broadcastRequestVoteRequest _req = return ()--error "Not implemented"
    sendAppendEntriesRequest _n _req = error "Not implemented"


instance Katip m => Katip (RPCT m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapRPCT . localLogEnv

instance KatipContext m => KatipContext (RPCT m) where
    getKatipContext = lift getKatipContext
    localKatipContext = mapRPCT . localKatipContext
    getKatipNamespace = lift getKatipNamespace
    localKatipNamespace = mapRPCT . localKatipNamespace

instance MonadReader r m => MonadReader r (RPCT m) where
    ask = lift ask
    local = mapRPCT . local

instance (Monad m, P.MonadPersistentState m) => P.MonadPersistentState (RPCT m) where
    type Term (RPCT m) = P.Term m
    type Node (RPCT m) = P.Node m
    type Entry (RPCT m) = P.Entry m
    type Index (RPCT m) = P.Index m

instance (Monad m, T.MonadTimers m) => T.MonadTimers (RPCT m)

mapRPCT :: (m a -> n b) -> RPCT m a -> RPCT n b
mapRPCT f (RPCT m) = RPCT $ mapIdentityT f m

runRPCT :: RPCT m a -> m a
runRPCT = runIdentityT . unRPCT
