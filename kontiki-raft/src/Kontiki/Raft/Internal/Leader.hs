{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Internal.Leader (
      convertToLeader
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import GHC.Stack (HasCallStack)

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Kontiki.Raft.Classes.State.Volatile (
    Conversion(CandidateToLeader), Role(Candidate, Leader), VolatileState, convert)
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startHeartbeatTimer)

convertToLeader :: forall m m' volatileState.
                   ( IxMonadState m
                   , m' ~ m (volatileState 'Leader) (volatileState 'Leader)
                   , VolatileState volatileState
                   , MonadTimers m'
                   )
                => m (volatileState 'Candidate) (volatileState 'Leader) ()
convertToLeader =
    imodify (convert CandidateToLeader) >>> (cancelElectionTimer :: m' ()) >>> (startHeartbeatTimer :: m' ())
  where
    a >>> b = Ix.ibind (const b) a

onRequestVoteRequest :: HasCallStack
                     => a
                     -> b
onRequestVoteRequest _ = error "Not implemented"

onRequestVoteResponse :: HasCallStack
                      => a
                      -> b
onRequestVoteResponse _ = error "Not implemented"

onAppendEntriesRequest :: HasCallStack
                       => a
                       -> b
onAppendEntriesRequest _ = error "Not implemented"

onAppendEntriesResponse :: HasCallStack
                        => a
                        -> b
onAppendEntriesResponse _ = error "Not implemented"

onElectionTimeout :: HasCallStack => a
onElectionTimeout = error "Not implemented"

onHeartbeatTimeout :: HasCallStack => a
onHeartbeatTimeout = error "Not implemented"
