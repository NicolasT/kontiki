{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Kontiki.Raft.Classes.State.Persistent
-- Copyright: (C) 2017 Nicolas Trangez
-- License: Apache (see the file LICENSE)
-- Maintainer: Nicolas Trangez <ikke@nicolast.be>
-- Stability: alpha
-- Portability: TypeFamilies
--
-- This module defines a monad to access persistent state stored by Raft
-- nodes.

module Kontiki.Raft.Classes.State.Persistent (
      MonadPersistentState(..)
    ) where

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
