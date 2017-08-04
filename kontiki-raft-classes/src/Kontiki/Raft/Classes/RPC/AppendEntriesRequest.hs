{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Kontiki.Raft.Classes.RPC.AppendEntriesRequest
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
-- Maintainer:  Nicolas Trangez <ikke@nicolast.be>
-- Stability:   alpha
-- Portability: TypeFamilies
--
-- This module defines an AppendEntries RPC request message.

module Kontiki.Raft.Classes.RPC.AppendEntriesRequest (
      AppendEntriesRequest(..)
    ) where

import Kontiki.Raft.Classes.Lens (Lens')
import Kontiki.Raft.Classes.RPC (HasTerm(Term))

-- | AppendEntries RPC request message
--
-- Invoked by leader to replicate log entries; also used as heartbeat.
class HasTerm msg => AppendEntriesRequest msg where
    type Node msg
    type Index msg
    type Entry msg

    -- | Leader, so follower can redirect clients
    leaderId :: Lens' msg (Node msg)
    -- | Index of log entry immediately preceding new ones
    prevLogIndex :: Lens' msg (Index msg)
    -- | Term of 'prevLogIndex' entry
    prevLogTerm :: Lens' msg (Term msg)
    -- | Log entries to store (empty for heartbeat; may send more than one
    -- for efficiency)
    entries :: Lens' msg [Entry msg]
    -- | Leader's commitIndex
    leaderCommit :: Lens' msg (Index msg)
