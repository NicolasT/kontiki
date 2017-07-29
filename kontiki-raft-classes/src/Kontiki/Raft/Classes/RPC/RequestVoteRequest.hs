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

-- | RequestVote RPC request message
--
-- Invoked by candidates to gather votes.
class RequestVoteRequest msg where
    type Term msg
    type Node msg
    type Index msg

    -- | Candidate's term
    term :: Lens' msg (Term msg)
    -- | Candidate requesting vote
    candidateId :: Lens' msg (Node msg)
    -- | Index of candidate's last log entry
    lastLogIndex :: Lens' msg (Index msg)
    -- | Term of candidate's last log entry
    lastLogTerm :: Lens' msg (Term msg)
