{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.RPC (
      HasTerm(..)
    , MonadRPC(..)
    ) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict

import Control.Monad.Indexed.State (IxStateT)
import Control.Monad.Indexed.Trans (ilift)

import Kontiki.Raft.Classes.Lens (Lens')

class HasTerm msg where
    type Term msg

    term :: Lens' msg (Term msg)


class MonadRPC m where
    type Node m

    type RequestVoteRequest m
    type AppendEntriesRequest m

    broadcastRequestVoteRequest :: RequestVoteRequest m -> m ()
    default broadcastRequestVoteRequest :: (Monad m', MonadRPC m', MonadTrans t, m ~ t m') => RequestVoteRequest m' -> m ()
    broadcastRequestVoteRequest = lift . broadcastRequestVoteRequest
    sendAppendEntriesRequest :: Node m -> AppendEntriesRequest m -> m ()
    default sendAppendEntriesRequest :: (Monad m', MonadRPC m', MonadTrans t, m ~ t m') => Node m' -> AppendEntriesRequest m' -> m ()
    sendAppendEntriesRequest n a = lift $ sendAppendEntriesRequest n a

instance (Monad m, MonadRPC m) => MonadRPC (StateT s m) where
    type Node (StateT s m) = Node m
    type RequestVoteRequest (StateT s m) = RequestVoteRequest m
    type AppendEntriesRequest (StateT s m) = AppendEntriesRequest m

instance (Monad m, MonadRPC m) => MonadRPC (Strict.StateT s m) where
    type Node (Strict.StateT s m) = Node m
    type RequestVoteRequest (Strict.StateT s m) = RequestVoteRequest m
    type AppendEntriesRequest (Strict.StateT s m) = AppendEntriesRequest m

instance (Monad m, MonadRPC m) => MonadRPC (IxStateT m i i) where
    type Node (IxStateT m i i) = Node m
    type RequestVoteRequest (IxStateT m i i) = RequestVoteRequest m
    type AppendEntriesRequest (IxStateT m i i) = AppendEntriesRequest m

    broadcastRequestVoteRequest = ilift . broadcastRequestVoteRequest
    sendAppendEntriesRequest n m = ilift $ sendAppendEntriesRequest n m
