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
    type AppendEntriesRequest m

    broadcastRequestVoteRequest :: RequestVoteRequest m -> m ()
    sendAppendEntriesRequest :: Node m -> AppendEntriesRequest m -> m ()

instance (Monad m, MonadRPC m) => MonadRPC (StateT s m) where
    type Node (StateT s m) = Node m
    type RequestVoteRequest (StateT s m) = RequestVoteRequest m
    type AppendEntriesRequest (StateT s m) = AppendEntriesRequest m

    broadcastRequestVoteRequest = lift . broadcastRequestVoteRequest
    sendAppendEntriesRequest n m = lift $ sendAppendEntriesRequest n m

instance (Monad m, MonadRPC m) => MonadRPC (IxStateT m i i) where
    type Node (IxStateT m i i) = Node m
    type RequestVoteRequest (IxStateT m i i) = RequestVoteRequest m
    type AppendEntriesRequest (IxStateT m i i) = AppendEntriesRequest m

    broadcastRequestVoteRequest = ilift . broadcastRequestVoteRequest
    sendAppendEntriesRequest n m = ilift $ sendAppendEntriesRequest n m
