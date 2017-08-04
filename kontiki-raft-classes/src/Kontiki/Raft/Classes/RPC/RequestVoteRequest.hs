{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Kontiki.Raft.Classes.RPC.RequestVoteRequest
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
-- Maintainer:  Nicolas Trangez <ikke@nicolast.be>
-- Stability:   alpha
-- Portability: TypeFamilies
--
-- This module defines a RequestVote RPC request message.

module Kontiki.Raft.Classes.RPC.RequestVoteRequest (
      RequestVoteRequest(..)
    ) where

import Kontiki.Raft.Classes.Lens (Lens')
import Kontiki.Raft.Classes.RPC (HasTerm(Term))

-- | RequestVote RPC request message
--
-- Invoked by candidates to gather votes.
class HasTerm msg => RequestVoteRequest msg where
    type Node msg
    type Index msg

    -- | Candidate requesting vote
    candidateId :: Lens' msg (Node msg)
    -- | Index of candidate's last log entry
    lastLogIndex :: Lens' msg (Index msg)
    -- | Term of candidate's last log entry
    lastLogTerm :: Lens' msg (Term msg)
