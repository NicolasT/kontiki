{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.String (fromString)

import qualified Data.Set as Set

import Control.Lens ((^.), (.~), (&), (.=), (%=), use, view)

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Indexed.State (IxMonadState, iget, imodify, iput)

import Data.Default.Class (Default, def)

import Control.Monad.Logger (MonadLogger, logDebug, logInfo)

import qualified Language.Haskell.Rebindable.Do as Use

import Kontiki.Raft.Classes.Config (Config, localNode)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (MonadRPC, broadcastRequestVoteRequest, term)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest, candidateId, lastLogIndex, lastLogTerm)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RequestVoteRequest
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse, voteGranted)
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as AppendEntriesRequest
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState, getCurrentTerm, setCurrentTerm, lastLogEntry)
import qualified Kontiki.Raft.Classes.State.Persistent as Persistent
import Kontiki.Raft.Classes.State.Volatile (Conversion(FollowerToCandidate), Role(Candidate, Follower, Leader), VolatileState, convert, dispatch, votesGranted)
import qualified Kontiki.Raft.Classes.State.Volatile as Volatile
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startElectionTimer)
import Kontiki.Raft.Classes.Types (Index, Term, index0, succTerm, term0)

import {-# SOURCE #-} Kontiki.Raft.Internal.Follower (convertToFollower)
import {-# SOURCE #-} qualified Kontiki.Raft.Internal.Follower as F
import Kontiki.Raft.Internal.Leader (convertToLeader)
import Kontiki.Raft.Internal.State (Some(Some), wrap)

convertToCandidate :: ( IxMonadState m
                      , VolatileState volatileState
                      , Config.Config config
                      , MonadReader config (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , MonadTimers (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , MonadPersistentState (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , Term (Persistent.Term (m (volatileState 'Candidate) (volatileState 'Candidate)))
                      , RequestVoteRequest.Node (RPC.RequestVoteRequest (m (volatileState 'Candidate) (volatileState 'Candidate))) ~ Config.Node config
                      , RPC.Term (RPC.RequestVoteRequest (m (volatileState 'Candidate) (volatileState 'Candidate))) ~ Persistent.Term (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , RequestVoteRequest.Index (RPC.RequestVoteRequest (m (volatileState 'Candidate) (volatileState 'Candidate))) ~ Persistent.Index (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , Default (RPC.RequestVoteRequest (m (volatileState 'Candidate) (volatileState 'Candidate)))
                      , RequestVoteRequest (RPC.RequestVoteRequest (m (volatileState 'Candidate) (volatileState 'Candidate)))
                      , MonadRPC (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , Index (Persistent.Index (m (volatileState 'Candidate) (volatileState 'Candidate)))
                      , Volatile.Node volatileState ~ Config.Node config
                      , Ord (Config.Node config)
                      , MonadState (volatileState 'Candidate) (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , MonadTimers (m (volatileState 'Leader) (volatileState 'Leader))
                      , MonadReader config (m (volatileState 'Leader) (volatileState 'Leader))
                      , Persistent.Index (m (volatileState 'Leader) (volatileState 'Leader)) ~ Persistent.Index (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , Volatile.Index volatileState ~ Persistent.Index (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , MonadPersistentState (m (volatileState 'Leader) (volatileState 'Leader))
                      , MonadState (volatileState 'Leader) (m (volatileState 'Leader) (volatileState 'Leader))
                      , RPC.Node (m (volatileState 'Leader) (volatileState 'Leader)) ~ Config.Node config
                      , AppendEntriesRequest.Index (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ Persistent.Index (m (volatileState 'Leader) (volatileState 'Leader))
                      , AppendEntriesRequest.Node (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ Config.Node config
                      , Persistent.Term (m (volatileState 'Leader) (volatileState 'Leader)) ~ RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                      , Term (RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))))
                      , Default (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                      , AppendEntriesRequest (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                      , MonadRPC (m (volatileState 'Leader) (volatileState 'Leader))
                      )
                   => m (volatileState 'Follower) (Some volatileState) ()
convertToCandidate = let Use.IxMonad{..} = def in do
    imodify (convert FollowerToCandidate)
    startElection

startElection :: forall m config index node requestVoteRequest term volatileState mCC mLL.
                 ( mCC ~ m (volatileState 'Candidate) (volatileState 'Candidate)
                 , mLL ~ m (volatileState 'Leader) (volatileState 'Leader)

                 , IxMonadState m
                 , MonadPersistentState mCC
                 , MonadReader config mCC
                 , MonadTimers mCC
                 , MonadRPC mCC
                 , MonadState (volatileState 'Candidate) mCC
                 , MonadTimers mLL
                 , MonadReader config mLL
                 , MonadState (volatileState 'Leader) mLL
                 , MonadPersistentState (m (volatileState 'Leader) (volatileState 'Leader))

                 , Config config
                 , Index index
                 , VolatileState volatileState
                 , RequestVoteRequest requestVoteRequest
                 , Default requestVoteRequest
                 , Ord node

                 , Config.Node config ~ node
                 , RequestVoteRequest.Node requestVoteRequest ~ node
                 , Volatile.Node volatileState ~ node
                 , RPC.Node (m (volatileState 'Leader) (volatileState 'Leader)) ~ node
                 , AppendEntriesRequest.Node (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ node

                 , Term term
                 , Persistent.Term mCC ~ term
                 , RPC.Term requestVoteRequest ~ term

                 , RPC.RequestVoteRequest mCC ~ requestVoteRequest

                 , Persistent.Index mCC ~ index
                 , Persistent.Index mLL ~ index
                 , RequestVoteRequest.Index requestVoteRequest ~ index
                 , Volatile.Index volatileState ~ index
                 , AppendEntriesRequest.Index (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ index
                 , Persistent.Term (m (volatileState 'Leader) (volatileState 'Leader)) ~ RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                 , Term (RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))))
                 , Default (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                 , AppendEntriesRequest (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                 , MonadRPC (m (volatileState 'Leader) (volatileState 'Leader))
                 )
              => m (volatileState 'Candidate) (Some volatileState) ()
startElection = let Use.IxMonad{..} = def in do
    -- When starting a new election round, reset any prior votes
    votesGranted .= Set.empty :: mCC ()

    incrementCurrentTerm
    voteForSelf
    -- In theory, after vote, we could've been turned into Leader
    whenStillCandidate $ do
        resetElectionTimer
        sendRequestVoteToOtherServers
        wrap
  where
    incrementCurrentTerm = let Use.Monad{..} = def in do
        currentTerm <- getCurrentTerm
        setCurrentTerm $ succTerm currentTerm
    voteForSelf = let Use.IxMonad{..} = def in do
        self <- view localNode :: mCC node
        voteFor self
    resetElectionTimer = let Use.Monad{..} = def in do
        cancelElectionTimer
        startElectionTimer
    sendRequestVoteToOtherServers = let Use.Monad{..} = def in do
        me <- view localNode
        currentTerm <- getCurrentTerm :: mCC term
        (idx, term') <- maybe (index0, term0) (\(i, t, _) -> (i, t)) <$> lastLogEntry
        broadcastRequestVoteRequest $ def & term .~ currentTerm
                                          & candidateId .~ me
                                          & lastLogIndex .~ idx
                                          & lastLogTerm .~ term'

    whenStillCandidate act = let Use.IxMonad{..} = def in do
        let remain _ = return ()
            runAct s = iput s >> act
        iget >>= \case
            Some s -> dispatch remain runAct remain s

voteFor :: forall m config volatileState node mC.
           ( IxMonadState m
           , mC ~ m (volatileState 'Candidate) (volatileState 'Candidate)
           , MonadState (volatileState 'Candidate) mC
           , MonadTimers (m (volatileState 'Leader) (volatileState 'Leader))
           , MonadReader config mC
           , MonadReader config (m (volatileState 'Leader) (volatileState 'Leader))
           , VolatileState volatileState
           , Volatile.Node volatileState ~ node
           , Ord node
           , Config.Node config ~ node
           , Persistent.Index (m (volatileState 'Leader) (volatileState 'Leader)) ~ Volatile.Index volatileState
           , MonadPersistentState (m (volatileState 'Leader) (volatileState 'Leader))
           , MonadState (volatileState 'Leader) (m (volatileState 'Leader) (volatileState 'Leader))
           , Config config
           , Index (Volatile.Index volatileState)
           , RPC.Node (m (volatileState 'Leader) (volatileState 'Leader)) ~ node
           , AppendEntriesRequest.Index (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ Volatile.Index volatileState
           , AppendEntriesRequest.Node (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ node
           , Persistent.Term (m (volatileState 'Leader) (volatileState 'Leader)) ~ RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
           , Term (RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))))
           , Default (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
           , AppendEntriesRequest (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
           , MonadRPC (m (volatileState 'Leader) (volatileState 'Leader))
           )
        => node
        -> m (volatileState 'Candidate) (Some volatileState) ()
voteFor node = let Use.IxMonad{..} = def in do
    votesGranted %= Set.insert node :: mC ()
    votesReceivedFromMajorityOfServers >>= \case
        True -> convertToLeader >> wrap
        False -> wrap
  where
    votesReceivedFromMajorityOfServers = let Use.Monad{..} = def in do
        votes <- use votesGranted
        nodes' <- view Config.nodes
        return $ isQuorum votes nodes'
    isQuorum v n = Set.size v >= (Set.size n `div` 2) + 1

onRequestVoteRequest :: ( MonadLogger m
                        , MonadPersistentState m
                        , RequestVoteResponse resp
                        , Persistent.Term m ~ RPC.Term resp
                        , Default resp
                        )
                     => req
                     -> m resp
onRequestVoteRequest _ = let Use.Monad{..} = def in do
    $(logDebug) "Received RequestVote request in Candidate mode, rejecting"
    term' <- getCurrentTerm
    return $ def & term .~ term'
                 & voteGranted .~ False

onRequestVoteResponse :: forall m config node resp term volatileState mCC.
                         ( IxMonadState m
                         , mCC ~ m (volatileState 'Candidate) (volatileState 'Candidate)
                         , MonadPersistentState mCC
                         , MonadLogger mCC
                         , MonadState (volatileState 'Candidate) mCC
                         , MonadTimers (m (volatileState 'Leader) (volatileState 'Leader))
                         , MonadReader config mCC
                         , MonadReader config (m (volatileState 'Leader) (volatileState 'Leader))
                         , VolatileState volatileState
                         , Eq term
                         , Ord node
                         , Persistent.Term mCC ~ term
                         , RPC.Term resp ~ term
                         , Volatile.Node volatileState ~ node
                         , RequestVoteResponse resp
                         , Persistent.Index (m (volatileState 'Leader) (volatileState 'Leader)) ~ Volatile.Index volatileState
                         , Config config
                         , Config.Node config ~ node
                         , MonadPersistentState (m (volatileState 'Leader) (volatileState 'Leader))
                         , MonadState (volatileState 'Leader) (m (volatileState 'Leader) (volatileState 'Leader))
                         , Index (Volatile.Index volatileState)
                         , Persistent.Term (m (volatileState 'Leader) (volatileState 'Leader)) ~ RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                         , AppendEntriesRequest.Node (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ node
                         , RPC.Node (m (volatileState 'Leader) (volatileState 'Leader)) ~ node
                         , AppendEntriesRequest.Index (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))) ~ Volatile.Index volatileState
                         , Term (RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader))))
                         , Default (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                         , AppendEntriesRequest (RPC.AppendEntriesRequest (m (volatileState 'Leader) (volatileState 'Leader)))
                         , MonadRPC (m (volatileState 'Leader) (volatileState 'Leader))
                         )
                      => node
                      -> resp
                      -> m (volatileState 'Candidate) (Some volatileState) ()
onRequestVoteResponse sender resp = let Use.IxMonad{..} = def in do
    term' <- getCurrentTerm
    if resp ^. term /= term'
        then do
            $(logDebug) "Received RequestVote response for other term, ignoring" :: mCC ()
            wrap
        else
            voteFor sender


onAppendEntriesRequest :: forall m req resp volatileState mCC mFF.
                          ( IxMonadState m
                          , mCC ~ m (volatileState 'Candidate) (volatileState 'Candidate)
                          , mFF ~ m (volatileState 'Follower) (volatileState 'Follower)
                          , MonadLogger mCC
                          , MonadTimers mFF
                          , VolatileState volatileState
                          )
                       => req
                       -> m (volatileState 'Candidate) (volatileState 'Follower) resp
onAppendEntriesRequest req = let Use.IxMonad{..} = def in do
    $(logDebug) "Received AppendEntries request in Candidate mode, stepping down to Follower" :: mCC ()
    convertToFollower
    F.onAppendEntriesRequest req

onAppendEntriesResponse :: MonadLogger m
                        => node -> resp -> m ()
onAppendEntriesResponse _ _ =
    $(logDebug) "Received AppendEntries response in Candidate mode, ignoring"


onElectionTimeout :: forall m config index node requestVoteRequest term volatileState mCC mLL.
                     ( mCC ~ m (volatileState 'Candidate) (volatileState 'Candidate)
                     , mLL ~ m (volatileState 'Leader) (volatileState 'Leader)

                     , IxMonadState m
                     , MonadPersistentState mCC
                     , MonadReader config mCC
                     , MonadTimers mCC
                     , MonadRPC mCC
                     , MonadState (volatileState 'Candidate) mCC
                     , MonadTimers mLL
                     , MonadLogger mCC
                     , MonadReader config mLL
                     , MonadState (volatileState 'Leader) mLL
                     , MonadPersistentState mLL
                     , MonadRPC mLL

                     , Config config
                     , Index index
                     , VolatileState volatileState
                     , RequestVoteRequest requestVoteRequest
                     , Default requestVoteRequest
                     , Ord node
                     , Default (RPC.AppendEntriesRequest mLL)
                     , AppendEntriesRequest (RPC.AppendEntriesRequest mLL)

                     , Config.Node config ~ node
                     , RequestVoteRequest.Node requestVoteRequest ~ node
                     , Volatile.Node volatileState ~ node
                     , RPC.Node mLL ~ node
                     , AppendEntriesRequest.Node (RPC.AppendEntriesRequest mLL) ~ node

                     , Term term
                     , Persistent.Term mCC ~ term
                     , RPC.Term requestVoteRequest ~ term
                     , Persistent.Term mLL ~ term
                     , RPC.Term (RPC.AppendEntriesRequest mLL) ~ term

                     , RPC.RequestVoteRequest mCC ~ requestVoteRequest

                     , Persistent.Index mCC ~ index
                     , RequestVoteRequest.Index requestVoteRequest ~ index
                     , Persistent.Index mLL ~ index
                     , Volatile.Index volatileState ~ index
                     , AppendEntriesRequest.Index (RPC.AppendEntriesRequest mLL) ~ index
                     )
                  => m (volatileState 'Candidate) (Some volatileState) ()
onElectionTimeout = let Use.IxMonad{..} = def in do
    $(logInfo) "Election timeout while in Candidate mode, starting new election"
    startElection

onHeartbeatTimeout :: MonadLogger m
                   => m ()
onHeartbeatTimeout =
    $(logDebug) "Received heartbeat timeout in Candidate mode, ignoring"


ifThenElse :: Bool -> a -> a -> a
ifThenElse b t f = case b of
    True -> t
    False -> f
