{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
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

import Prelude hiding ((>>), (>>=), return)
import Control.Monad (when)
import Data.String (fromString)

import GHC.Stack (HasCallStack)

import Control.Monad.Indexed ((>>>=))
import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.), (%=), (<%=), use, view)

import Data.Default.Class (Default, def)

import Control.Monad.Logger (MonadLogger, logDebug, logInfo)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)

import qualified Data.Set as Set

import qualified Language.Haskell.Rebindable as Use

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

{-# ANN ifThenElse ("HLint: ignore" :: String) #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse b t f = case b of
    True -> t
    False -> f

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
convertToCandidate = let Use.IxMonad{..} = def in do
    changeState
    startElection
  where
    changeState = imodify (V.convert V.FollowerToCandidate)

{-
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
        -}
startElection :: ( IxMonadState m
                 )
              => m (volatileState 'V.Candidate) (Some volatileState) ()
startElection = let Use.IxMonad{..} = def in do
    $(logInfo) "Starting election"
    incrementCurrentTerm
    voteForSelf
    -- resetElectionTimer
    -- sendRequestVoteToOtherServers
  where
    incrementCurrentTerm :: ( MonadState (volatileState 'V.Candidate) m
                            , MonadPersistentState m
                            )
                         => m ()
    incrementCurrentTerm = let Use.Monad{..} = def in do
        currentTerm <- getCurrentTerm
        let newTerm = succTerm currentTerm
        setCurrentTerm newTerm
    voteForSelf = let Use.IxMonad{..} = def in do
        self <- view localNode
        voteFor self
    resetElectionTimer = let Use.Monad{..} = def in do
        cancelElectionTimer
        startElectionTimer
    sendRequestVoteToOtherServers = let Use.Monad{..} = def in do
        me <- view localNode
        currentTerm <- getCurrentTerm
        (idx, term') <- maybe (T.index0, T.term0) (\(i, t, _) -> (i, t)) <$> lastLogEntry
        broadcastRequestVoteRequest $ def & term .~ currentTerm
                                          & candidateId .~ me
                                          & lastLogIndex .~ idx
                                          & lastLogTerm .~ term'

voteFor :: forall m m' volatileState.
           ( IxMonadState m
           , m' ~ m (volatileState 'V.Candidate) (volatileState 'V.Candidate)
           , MonadState (volatileState 'V.Candidate) m'
           , MonadTimers (m (volatileState 'V.Leader) (volatileState 'V.Leader))
           , V.VolatileState volatileState
           , Ord (V.Node volatileState)
           )
        => V.Node volatileState
        -> m (volatileState 'V.Candidate) (Some volatileState) ()
voteFor node = let Use.IxMonad{..} = def in do
    votes <- votesGranted <%= Set.insert node
    -- TODO
    if Set.size votes >= 2
        then convertToLeader >> wrap
        else wrap

onRequestVoteRequest :: ( MonadLogger m
                        , MonadPersistentState m
                        , Default resp
                        , RequestVoteResponse resp
                        , RPC.Term resp ~ P.Term m
                        )
                     => req
                     -> m resp
onRequestVoteRequest _ = let Use.Monad{..} = def in do
    $(logDebug) "Received RequestVote request in Candidate mode, ignoring"
    currentTerm <- getCurrentTerm
    return $ def & term .~ currentTerm
                 & voteGranted .~ False

onRequestVoteResponse :: forall m m' term node resp volatileState.
                         ( IxMonadState m
                         , m' ~ m (volatileState 'V.Candidate) (volatileState 'V.Candidate)
                         , MonadPersistentState m'
                         , MonadLogger m'
                         , MonadTimers (m (volatileState 'V.Leader) (volatileState 'V.Leader))
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
onRequestVoteResponse sender msg = let Use.IxMonad{..} = def in do
    currentTerm <- getCurrentTerm :: m' term
    if msg ^. term == currentTerm
        then
            if msg ^. voteGranted
                then
                    voteFor sender
                else do
                    $(logDebug) "Received RequestVote response but vote not granted" :: m' ()
                    wrap
        else do
            $(logDebug) "Received RequestVote response for old term" :: m' ()
            wrap

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
onAppendEntriesRequest req = let Use.IxMonad{..} = def in do
    currentTerm <- getCurrentTerm :: m' term
    if currentTerm <= req ^. term
        then do
            Follower.convertToFollower
            res <- Follower.onAppendEntriesRequest req
            wrap
            return res
        else do
            let res = def & term .~ currentTerm
                          & success .~ False
            wrap
            return res

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
