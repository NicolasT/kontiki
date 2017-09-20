{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.String (fromString)

import GHC.Stack (HasCallStack)

import Control.Lens ((&), (.~))

import Control.Monad.Logger (MonadLogger, logDebug)

import Data.Default.Class (Default, def)

import Control.Monad.Indexed.State (IxMonadState, imodify)

import qualified Language.Haskell.Rebindable.Do as Use

import Kontiki.Raft.Classes.RPC (term)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse, voteGranted)
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse, success)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState, getCurrentTerm)
import qualified Kontiki.Raft.Classes.State.Persistent as Persistent
import Kontiki.Raft.Classes.State.Volatile (
    Conversion(CandidateToLeader), Role(Candidate, Leader), VolatileState, convert)
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startHeartbeatTimer)

convertToLeader :: forall m mL {- mL = 'm in Leader state' -} volatileState.
                   ( IxMonadState m
                   , mL ~ m (volatileState 'Leader) (volatileState 'Leader)
                   , MonadTimers mL
                   , Monad mL
                   , VolatileState volatileState
                   )
                => m (volatileState 'Candidate) (volatileState 'Leader) ()
convertToLeader = let Use.IxMonad{..} = def in do
    imodify (convert CandidateToLeader)
    cancelElectionTimer
    startHeartbeatTimer :: mL ()

    sendInitialEmptyAppendEntriesRPCs
  where
    sendInitialEmptyAppendEntriesRPCs = let Use.Monad{..} = def in do
        error "Not implemented"

onRequestVoteRequest :: ( MonadLogger m
                        , MonadPersistentState m
                        , RequestVoteResponse resp
                        , Default resp
                        , RPC.Term resp ~ Persistent.Term m
                        )
                     => req
                     -> m resp
onRequestVoteRequest _ = let Use.Monad{..} = def in do
    $(logDebug) "Received RequestVote request in Leader mode, rejecting"
    term' <- getCurrentTerm
    return $ def & term .~ term'
                 & voteGranted .~ False

onRequestVoteResponse :: MonadLogger m
                      => sender
                      -> resp
                      -> m ()
onRequestVoteResponse _ _ =
    $(logDebug) "Received RequestVote response in Leader mode, ignoring"

onAppendEntriesRequest :: ( MonadLogger m
                          , MonadPersistentState m
                          , RPC.Term resp ~ Persistent.Term m
                          , AppendEntriesResponse resp
                          , Default resp
                          )
                       => req
                       -> m resp
onAppendEntriesRequest _ = let Use.Monad{..} = def in do
    $(logDebug) "Received AppendEntries request in Leader mode, rejecting"
    term' <- getCurrentTerm
    return $ def & term .~ term'
                 & success .~ False

onAppendEntriesResponse :: HasCallStack
                        => a
                        -> b
onAppendEntriesResponse _ = error "Not implemented"

onElectionTimeout :: MonadLogger m => m ()
onElectionTimeout =
    $(logDebug) "Received election timeout in Leader mode, ignoring"

onHeartbeatTimeout :: HasCallStack => a
onHeartbeatTimeout = error "Not implemented"
