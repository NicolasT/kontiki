{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Kontiki.Raft.Internal.Candidate (
      convertToCandidate
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import GHC.Stack (HasCallStack)

import Control.Monad.Indexed ((>>>=))
import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.), (%=), use, view)

import Data.Default.Class (Default, def)

import Control.Monad.Logger (MonadLogger, logDebug, logInfo)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)

import qualified Data.Set as Set

import Kontiki.Raft.Classes.Config (Config, localNode)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (MonadRPC, broadcastRequestVoteRequest, term)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest, candidateId, lastLogIndex, lastLogTerm)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RequestVoteRequest
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse, voteGranted)
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse, success)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState, getCurrentTerm, lastLogEntry, setCurrentTerm)
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState, votesGranted)
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startElectionTimer)
import Kontiki.Raft.Classes.Types (succTerm)
import qualified Kontiki.Raft.Classes.Types as T

import {-# SOURCE #-} qualified  Kontiki.Raft.Internal.Follower as Follower
import Kontiki.Raft.Internal.Leader (convertToLeader)
import Kontiki.Raft.Internal.State (Some, wrap)

convertToCandidate :: forall m m' volatileState config index node req term.
                      ( IxMonadState m
                      , MonadReader config m'
                      , V.VolatileState volatileState
                      , T.Term term
                      , T.Index index
                      , Config config
                      , MonadState (volatileState 'V.Candidate) m'
                      , MonadLogger m'
                      , MonadPersistentState m'
                      , MonadTimers m'
                      , MonadRPC m'
                      , RequestVoteRequest req
                      , Default req
                      , Ord node
                      , m' ~ m (volatileState 'V.Candidate) (volatileState 'V.Candidate)
                      , req ~ RPC.RequestVoteRequest m'
                      , index ~ RequestVoteRequest.Index req
                      , index ~ P.Index m'
                      , term ~ RPC.Term req
                      , term ~ P.Term m'
                      , node ~ Config.Node config
                      , node ~ RequestVoteRequest.Node req
                      , node ~ V.Node volatileState
                      )
                   => m (volatileState 'V.Follower) (volatileState 'V.Candidate) ()
convertToCandidate = changeState >>> startElection
  where
    changeState = imodify (V.convert V.FollowerToCandidate)
    m >>> n = Ix.ibind (const n) m

startElection :: forall volatileState m req config.
                 ( Monad m
                 , MonadPersistentState m
                 , MonadTimers m
                 , MonadRPC m
                 , MonadReader config m
                 , MonadLogger m
                 , MonadState (volatileState 'V.Candidate) m
                 , Config config
                 , T.Index (P.Index m)
                 , T.Term (P.Term m)
                 , req ~ RPC.RequestVoteRequest m
                 , RequestVoteRequest req
                 , Default req
                 , RequestVoteRequest.Index req ~ P.Index m
                 , RPC.Term req ~ P.Term m
                 , Config.Node config ~ RequestVoteRequest.Node req
                 , V.VolatileState volatileState
                 , V.Node volatileState ~ RequestVoteRequest.Node req
                 , Ord (V.Node volatileState)
                 )
              => m ()
startElection = do
    $(logInfo) "Starting election"
    incrementCurrentTerm
    voteForSelf
    resetElectionTimer
    sendRequestVoteToOtherServers
  where
    incrementCurrentTerm = do
        currentTerm <- getCurrentTerm
        let newTerm = succTerm currentTerm
        setCurrentTerm newTerm
    voteForSelf =
        voteFor =<< view localNode
    resetElectionTimer = do
        cancelElectionTimer
        startElectionTimer
    sendRequestVoteToOtherServers = do
        me <- view localNode
        currentTerm <- getCurrentTerm
        (idx, term') <- maybe (T.index0, T.term0) (\(i, t, _) -> (i, t)) <$> lastLogEntry
        let req = def & term .~ currentTerm
                      & candidateId .~ me
                      & lastLogIndex .~ idx
                      & lastLogTerm .~ term'
        broadcastRequestVoteRequest req

voteFor :: forall m m' volatileState.
           ( IxMonadState m
           , m' ~ m (volatileState 'V.Candidate) (volatileState 'V.Candidate)
           , V.VolatileState volatileState
           , Ord (V.Node volatileState)
           )
        => V.Node volatileState
        -> m (volatileState 'V.Candidate) (Some volatileState) ()
voteFor node =
    votesGranted %= Set.insert node >>>= \() -> use votesGranted >>>= \votes ->
        if Set.size votes >= 2
            then convertToLeader >>>= \() -> wrap
            else wrap

onRequestVoteRequest :: ( MonadLogger m
                        , MonadPersistentState m
                        , Default resp
                        , RequestVoteResponse resp
                        , RPC.Term resp ~ P.Term m
                        )
                     => req
                     -> m resp
onRequestVoteRequest _ = do
    $(logDebug) "Received RequestVote request in Candidate mode, ignoring"
    currentTerm <- getCurrentTerm
    let msg = def & term .~ currentTerm
                  & voteGranted .~ False
    return msg

onRequestVoteResponse :: forall m m' term node resp volatileState.
                         ( IxMonadState m
                         , m' ~ m (volatileState 'V.Candidate) (volatileState 'V.Candidate)
                         , MonadPersistentState m'
                         , MonadLogger m'
                         , P.Term m' ~ term
                         , RPC.Term resp ~ term
                         , V.VolatileState volatileState
                         , MonadState (volatileState 'V.Candidate) m'
                         , V.Node volatileState ~ node
                         , RequestVoteResponse resp
                         , Eq term
                         , Ord node
                         , HasCallStack
                         )
                      => node
                      -> resp
                      -> m (volatileState 'V.Candidate) (Some volatileState) ()
onRequestVoteResponse sender msg =
    (getCurrentTerm :: m' term) >>>= \currentTerm ->
        if msg ^. term == currentTerm
            then
                if msg ^. voteGranted
                    then
                        (voteFor sender :: m' ()) >>>= \() -> wrap
                    else
                        ($(logDebug) "Received RequestVote response but vote not granted" :: m' ()) >>>= \() -> wrap
            else
                ($(logDebug) "Received RequestVote response for old term" :: m' ()) >>>= \() -> wrap

onAppendEntriesRequest :: forall m m' volatileState req resp term.
                          ( IxMonadState m
                          , AppendEntriesRequest req
                          , AppendEntriesResponse resp
                          , VolatileState volatileState
                          , Default resp
                          , P.Term m' ~ term
                          , RPC.Term req ~ term
                          , RPC.Term resp ~ term
                          , Ord term
                          , m' ~ m (volatileState 'V.Candidate) (volatileState 'V.Candidate)
                          , MonadPersistentState m'
                          , MonadTimers (m (volatileState 'V.Follower) (volatileState 'V.Follower))
                          , MonadState (volatileState 'V.Candidate) m'
                          )
                       => req
                       -> m (volatileState 'V.Candidate) (Some volatileState) resp
onAppendEntriesRequest req =
    (getCurrentTerm :: m' term) >>>= \currentTerm ->
    if currentTerm <= req ^. term
        then
            Follower.convertToFollower >>>= \() ->
                Follower.onAppendEntriesRequest req >>>= \res ->
                    wrap >>>= \() ->
                        Ix.ireturn res
        else
            let res = def & term .~ currentTerm
                          & success .~ False in
            wrap >>>= \() ->
                Ix.ireturn res
  where
    a >>>= b = Ix.ibind b a

onAppendEntriesResponse :: MonadLogger m
                        => node
                        -> resp
                        -> m ()
onAppendEntriesResponse _ _ =
    $(logDebug) "Received AppendEntries response in Candidate mode, ignoring"

onElectionTimeout :: ( MonadPersistentState m
                     , MonadTimers m
                     , MonadRPC m
                     , MonadReader config m
                     , MonadLogger m
                     , MonadState (volatileState 'V.Candidate) m
                     , Config config
                     , T.Index (P.Index m)
                     , T.Term (P.Term m)
                     , req ~ RPC.RequestVoteRequest m
                     , RequestVoteRequest req
                     , Default req
                     , RequestVoteRequest.Index req ~ P.Index m
                     , RPC.Term req ~ P.Term m
                     , Config.Node config ~ RequestVoteRequest.Node req
                     , V.VolatileState volatileState
                     , V.Node volatileState ~ RequestVoteRequest.Node req
                     , Ord (V.Node volatileState)
                     )
                  => m ()
onElectionTimeout = startElection

onHeartbeatTimeout :: MonadLogger m => m ()
onHeartbeatTimeout =
    $(logDebug) "Heartbeat timeout in Candidate mode, ignoring"
