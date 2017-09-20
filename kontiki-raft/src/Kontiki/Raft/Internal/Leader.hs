{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Kontiki.Raft.Internal.Leader (
      convertToLeader
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import Prelude hiding ((>>), (>>=), return)

import GHC.Stack (HasCallStack)

import Data.Default.Class (def)

import Control.Monad.Indexed.State (IxMonadState, imodify)

import qualified Language.Haskell.Rebindable.Do as Use

import Kontiki.Raft.Classes.State.Volatile (
    Conversion(CandidateToLeader), Role(Candidate, Leader), VolatileState, convert)
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startHeartbeatTimer)

convertToLeader :: forall m mL {- mL = 'm in Leader state' -} volatileState.
                   ( IxMonadState m
                   , mL ~ m (volatileState 'Leader) (volatileState 'Leader)
                   , MonadTimers mL
                   , VolatileState volatileState
                   )
                => m (volatileState 'Candidate) (volatileState 'Leader) ()
convertToLeader = let Use.IxMonad{..} = def in do
    imodify (convert CandidateToLeader)
    cancelElectionTimer @mL
    startHeartbeatTimer @mL

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
