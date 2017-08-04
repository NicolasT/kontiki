{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Kontiki.Raft.Classes.RPC.RequestVoteResponse
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
-- Maintainer:  Nicolas Trangez <ikke@nicolast.be>
-- Stability:   alpha
-- Portability: TypeFamilies
--
-- This module defines a RequestVote RPC response message.

module Kontiki.Raft.Classes.RPC.RequestVoteResponse (
      RequestVoteResponse(..)
    ) where

import Kontiki.Raft.Classes.Lens (Lens')
import Kontiki.Raft.Classes.RPC (HasTerm)

-- | RequestVote RPC response message
--
-- Returned by nodes upon a 'RequestVoteRequest' message.
class HasTerm msg => RequestVoteResponse msg where
    -- | True means candidate received vote
    voteGranted :: Lens' msg Bool
