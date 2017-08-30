{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Kontiki.Raft.Classes.State.Persistent
-- Copyright: (C) 2017 Nicolas Trangez
-- License: Apache (see the file LICENSE)
-- Maintainer: Nicolas Trangez <ikke@nicolast.be>
-- Stability: alpha
-- Portability: FlexibleInstances, TypeFamilies
--
-- This module defines a monad to access persistent state stored by Raft
-- nodes.

module Kontiki.Raft.Classes.State.Persistent (
      MonadPersistentState(..)
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Strict as SStateT

import Control.Monad.Indexed.State (IxStateT)
import Control.Monad.Indexed.Trans (ilift)

-- | Persistent state on all servers
--
-- Updated on stable storage before responding to RPCs.
class MonadPersistentState m where
    type Term m
    type Node m
    type Entry m
    type Index m

    -- | Latest term server has seen
    --
    -- Initialized to 0 on first boot, increases monotoniically.
    getCurrentTerm :: m (Term m)
    setCurrentTerm :: Term m -> m ()

    -- | Candidate that received vote in current term
    --
    -- None if none.
    getVotedFor :: m (Maybe (Node m))
    setVotedFor :: Maybe (Node m) -> m ()

    -- | Log entries
    --
    -- Each entry contains command for state machine, and term when entry
    -- was received by leader. First index is 1.
    getLogEntry :: Index m -> m (Term m, Entry m)
    setLogEntry :: Index m -> Term m -> Entry m -> m ()


instance (Monad m, MonadPersistentState m) => MonadPersistentState (StateT s m) where
    type Term (StateT s m) = Term m
    type Node (StateT s m) = Node m
    type Entry (StateT s m) = Entry m
    type Index (StateT s m) = Index m

    getCurrentTerm = lift getCurrentTerm
    setCurrentTerm = lift . setCurrentTerm
    getVotedFor = lift getVotedFor
    setVotedFor = lift . setVotedFor
    getLogEntry = lift . getLogEntry
    setLogEntry i t e = lift $ setLogEntry i t e

instance (Monad m, MonadPersistentState m) => MonadPersistentState (SStateT.StateT s m) where
    type Term (SStateT.StateT s m) = Term m
    type Node (SStateT.StateT s m) = Node m
    type Entry (SStateT.StateT s m) = Entry m
    type Index (SStateT.StateT s m) = Index m

    getCurrentTerm = lift getCurrentTerm
    setCurrentTerm = lift . setCurrentTerm
    getVotedFor = lift getVotedFor
    setVotedFor = lift . setVotedFor
    getLogEntry = lift . getLogEntry
    setLogEntry i t e = lift $ setLogEntry i t e

instance (Monad m, MonadPersistentState m) => MonadPersistentState (IxStateT m i i) where
    type Term (IxStateT m i i) = Term m
    type Node (IxStateT m i i) = Node m
    type Entry (IxStateT m i i) = Entry m
    type Index (IxStateT m i i) = Index m

    getCurrentTerm = ilift getCurrentTerm
    setCurrentTerm = ilift . setCurrentTerm
    getVotedFor = ilift getVotedFor
    setVotedFor = ilift . setVotedFor
    getLogEntry = ilift . getLogEntry
    setLogEntry i t e = ilift $ setLogEntry i t e
