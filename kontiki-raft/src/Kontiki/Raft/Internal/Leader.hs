{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (unless, forM_)

import GHC.Stack (HasCallStack)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Lens ((&), (.~), (.=), use, view)

import Control.Monad.Logger (MonadLogger, logDebug)

import Data.Default.Class (Default, def)

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Indexed.State (IxMonadState, imodify)

import qualified Language.Haskell.Rebindable.Do as Use

import Kontiki.Raft.Classes.Config (Config, localNode, nodes)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (MonadRPC, sendAppendEntriesRequest, term)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse, voteGranted)
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest, leaderId, prevLogIndex, prevLogTerm, entries, leaderCommit)
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as
        AppendEntriesRequest
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse, success)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState, getCurrentTerm, lastLogEntry)
import qualified Kontiki.Raft.Classes.State.Persistent as Persistent
import Kontiki.Raft.Classes.State.Volatile (
    Conversion(CandidateToLeader), Role(Candidate, Leader), VolatileState,
    commitIndex, convert, matchIndex, nextIndex)
import qualified Kontiki.Raft.Classes.State.Volatile as Volatile
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startHeartbeatTimer)
import Kontiki.Raft.Classes.Types (Index, Term, index0, succIndex, term0)

convertToLeader :: forall m config mL {- mL = 'm in Leader state' -} volatileState.
                   ( IxMonadState m
                   , mL ~ m (volatileState 'Leader) (volatileState 'Leader)
                   , MonadTimers mL
                   , Monad mL
                   , MonadReader config mL
                   , MonadPersistentState mL
                   , MonadState (volatileState 'Leader) mL
                   , Config config
                   , VolatileState volatileState
                   , Index (Persistent.Index mL)
                   , Index (Volatile.Index volatileState)
                   , Ord (Config.Node config)
                   , Persistent.Index mL ~ Volatile.Index volatileState
                   , Config.Node config ~ Volatile.Node volatileState
                   , Persistent.Term mL ~ RPC.Term (RPC.AppendEntriesRequest mL)
                   , AppendEntriesRequest.Index (RPC.AppendEntriesRequest mL) ~ Volatile.Index volatileState
                   , AppendEntriesRequest.Node (RPC.AppendEntriesRequest mL) ~ Volatile.Node volatileState
                   , RPC.Node mL ~ Volatile.Node volatileState
                   , Term (RPC.Term (RPC.AppendEntriesRequest mL))
                   , Default (RPC.AppendEntriesRequest mL)
                   , AppendEntriesRequest (RPC.AppendEntriesRequest mL)
                   , MonadRPC mL
                   )
                => m (volatileState 'Candidate) (volatileState 'Leader) ()
convertToLeader = let Use.IxMonad{..} = def in do
    imodify (convert CandidateToLeader)

    initializeState :: mL ()

    cancelElectionTimer
    startHeartbeatTimer :: mL ()

    sendInitialEmptyAppendEntriesRPCs
  where
    initializeState = let Use.Monad{..} = def in do
        initializeNextIndex
        initializeMatchIndex

    initializeNextIndex = let Use.Monad{..} = def in do
        lastLogIndex <- lastLogEntry >>= \case
            Nothing -> return index0
            Just (lastLogIndex, _, _) -> return lastLogIndex
        let nextLogIndex = succIndex lastLogIndex
        nodes' <- view nodes
        nextIndex .= Map.fromList [(node, nextLogIndex) | node <- Set.toList nodes']

    initializeMatchIndex = let Use.Monad{..} = def in do
        nodes' <- view nodes
        matchIndex .= Map.fromList [(node, index0) | node <- Set.toList nodes']

    sendInitialEmptyAppendEntriesRPCs = let Use.Monad{..} = def in do
        self <- view localNode
        nodes' <- view nodes
        commitIndex' <- use commitIndex
        term' <- getCurrentTerm
        (lastLogIndex, lastLogTerm) <- lastLogEntry >>= \case
            Nothing -> return (index0, term0)
            Just (lastLogIndex, lastLogTerm, _) -> return (lastLogIndex, lastLogTerm)

        let msg = def & term .~ term'
                      & leaderId .~ self
                      & prevLogIndex .~ lastLogIndex
                      & prevLogTerm .~ lastLogTerm
                      & entries .~ []
                      & leaderCommit .~ commitIndex'

        forM_ nodes' $ \node ->
            unless (node == self) $
                sendAppendEntriesRequest node msg

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
