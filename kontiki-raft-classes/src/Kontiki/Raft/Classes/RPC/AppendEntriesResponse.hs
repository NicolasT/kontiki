{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Kontiki.Raft.Classes.RPC.AppendEntriesResponse
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
-- Maintainer:  Nicolas Trangez <ikke@nicolast.be>
-- Stability:   alpha
-- Portability: TypeFamilies
--
-- This module defines an AppendEntries RPC response message.

module Kontiki.Raft.Classes.RPC.AppendEntriesResponse (
      AppendEntriesResponse(..)
    ) where

import Kontiki.Raft.Classes.Lens (Lens')
import Kontiki.Raft.Classes.RPC (HasTerm)

-- | AppendEntries RPC response message
--
-- Returned by nodes upon an 'AppendEntriesRequest' message.
class HasTerm msg => AppendEntriesResponse msg where
    -- | True if follower contained entry matching 'prevLogIndex' and
    -- 'prevLogTerm'
    success :: Lens' msg Bool
