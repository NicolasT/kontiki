{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.))

import Data.Default.Class (Default, def)

import Kontiki.Raft.Classes.Config (MonadConfig, localNode)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (MonadRPC, broadcastRequestVoteRequest)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest, candidateId, lastLogIndex, lastLogTerm)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RequestVoteRequest
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState, getCurrentTerm, lastLogEntry, setCurrentTerm)
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState, commitIndex, lastApplied)
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startElectionTimer)
import Kontiki.Raft.Classes.Types (succTerm)
import qualified Kontiki.Raft.Classes.Types as T

import Kontiki.Raft.Internal.State (Role(Candidate, Follower), State(F, C))

convertToCandidate :: forall m vs vls requestVoteRequest index term node.
                      ( IxMonadState m
                      , Monad (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadConfig (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadRPC (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadTimers (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadPersistentState (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , VolatileState vs
                      , Default vs
                      , requestVoteRequest ~ RPC.RequestVoteRequest (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , RequestVoteRequest requestVoteRequest
                      , RequestVoteRequest.Index requestVoteRequest ~ index
                      , P.Index (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ index
                      , RPC.Term requestVoteRequest ~ term
                      , P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ term
                      , RequestVoteRequest.Node requestVoteRequest ~ node
                      , Config.Node (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ node
                      , T.Term term
                      , T.Index index
                      , Default requestVoteRequest
                      , HasCallStack
                      )
                   => m (State vs vls 'Follower) (State vs vls 'Candidate) ()
convertToCandidate = changeState >>> startElection
  where
    changeState :: m (State vs vls 'Follower) (State vs vls 'Candidate) ()
    changeState = imodify $ \case
        F s -> C $ def & commitIndex .~ s ^. commitIndex
                       & lastApplied .~ s ^. lastApplied
    m >>> n = Ix.ibind (const n) m

startElection :: forall m req.
                 ( Monad m
                 , MonadPersistentState m
                 , MonadTimers m
                 , MonadRPC m
                 , MonadConfig m
                 , T.Index (P.Index m)
                 , T.Term (P.Term m)
                 , req ~ RPC.RequestVoteRequest m
                 , RequestVoteRequest req
                 , Default req
                 , RequestVoteRequest.Index req ~ P.Index m
                 , RPC.Term req ~ P.Term m
                 , Config.Node m ~ RequestVoteRequest.Node req
                 )
              => m ()
startElection = do
    incrementCurrentTerm
    -- voteForSelf
    resetElectionTimer
    sendRequestVoteToOtherServers
  where
    incrementCurrentTerm = do
        currentTerm <- getCurrentTerm
        setCurrentTerm (succTerm currentTerm)
    resetElectionTimer = do
        cancelElectionTimer
        startElectionTimer
    sendRequestVoteToOtherServers = do
        me <- localNode
        (idx, term) <- maybe (T.index0, T.term0) (\(i, t, _) -> (i, t)) <$> lastLogEntry
        let req = def & candidateId .~ me
                      & lastLogIndex .~ idx
                      & lastLogTerm .~ term
        broadcastRequestVoteRequest req

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
