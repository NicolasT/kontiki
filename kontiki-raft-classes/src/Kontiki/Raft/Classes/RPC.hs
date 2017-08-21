{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.RPC (
      HasTerm(..)
    , MonadRPC(..)
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)

import Control.Monad.Indexed.State (IxStateT)
import Control.Monad.Indexed.Trans (ilift)

import Kontiki.Raft.Classes.Lens (Lens')

class HasTerm msg where
    type Term msg

    term :: Lens' msg (Term msg)


class MonadRPC m where
    type Node m

    type RequestVoteRequest m
    type RequestVoteResponse m
    type AppendEntriesRequest m
    type AppendEntriesResponse m

    broadcastRequestVoteRequest :: RequestVoteRequest m -> m ()
    sendRequestVoteResponse :: Node m -> RequestVoteResponse m -> m ()

    sendAppendEntriesRequest :: Node m -> AppendEntriesRequest m -> m ()
    sendAppendEntriesResponse :: Node m -> AppendEntriesResponse m -> m ()

instance (Monad m, MonadRPC m) => MonadRPC (StateT s m) where
    type Node (StateT s m) = Node m
    type RequestVoteRequest (StateT s m) = RequestVoteRequest m
    type RequestVoteResponse (StateT s m) = RequestVoteResponse m
    type AppendEntriesRequest (StateT s m) = AppendEntriesRequest m
    type AppendEntriesResponse (StateT s m) = AppendEntriesResponse m

    broadcastRequestVoteRequest = lift . broadcastRequestVoteRequest
    sendRequestVoteResponse n m = lift $ sendRequestVoteResponse n m
    sendAppendEntriesRequest n m = lift $ sendAppendEntriesRequest n m
    sendAppendEntriesResponse n m = lift $ sendAppendEntriesResponse n m

instance (Monad m, MonadRPC m) => MonadRPC (IxStateT m i i) where
    type Node (IxStateT m i i) = Node m
    type RequestVoteRequest (IxStateT m i i) = RequestVoteRequest m
    type RequestVoteResponse (IxStateT m i i) = RequestVoteResponse m
    type AppendEntriesRequest (IxStateT m i i) = AppendEntriesRequest m
    type AppendEntriesResponse (IxStateT m i i) = AppendEntriesResponse m

    broadcastRequestVoteRequest = ilift . broadcastRequestVoteRequest
    sendRequestVoteResponse n m = ilift $ sendRequestVoteResponse n m
    sendAppendEntriesRequest n m = ilift $ sendAppendEntriesRequest n m
    sendAppendEntriesResponse n m = ilift $ sendAppendEntriesResponse n m
