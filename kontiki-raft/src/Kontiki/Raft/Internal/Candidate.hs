{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Internal.Candidate (
      convertToCandidate
    , onRequestVoteRequest
    , onRequestVoteResponse

    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import Prelude hiding ((>>), (>>=), return)

import Control.Lens ((.~), (&), view)

import Control.Monad.Reader.Class (MonadReader)

import Control.Monad.Indexed.State (IxMonadState, iget, imodify, iput, runIxStateT)

import Data.Default.Class (def)

import qualified Language.Haskell.Rebindable.Do as Use

import Kontiki.Raft.Classes.Config (localNode)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (broadcastRequestVoteRequest, term)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest, candidateId, lastLogIndex, lastLogTerm)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RequestVoteRequest
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState, getCurrentTerm, setCurrentTerm, lastLogEntry)
import qualified Kontiki.Raft.Classes.State.Persistent as Persistent
import Kontiki.Raft.Classes.State.Volatile (Conversion(FollowerToCandidate), Role(Candidate, Follower), VolatileState, convert, dispatch)
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startElectionTimer)
import Kontiki.Raft.Classes.Types (Term, index0, succTerm, term0)

import Kontiki.Raft.Internal.State (Some(Some), wrap)

convertToCandidate :: ( IxMonadState m
                      , VolatileState volatileState
                      , Config.Config config
                      , MonadReader config (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , MonadPersistentState (m (volatileState 'Candidate) (volatileState 'Candidate))
                      , Term (Persistent.Term (m (volatileState 'Candidate) (volatileState 'Candidate)))
                      )
                   => m (volatileState 'Follower) (Some volatileState) ()
convertToCandidate = let Use.IxMonad{..} = def in do
    imodify (convert FollowerToCandidate)
    startElection

startElection :: forall m config node term volatileState mC mCS.
                 ( IxMonadState m
                 , mC ~ m (volatileState 'Candidate) (volatileState 'Candidate)
                 , mCS ~ m (volatileState 'Candidate) (Some volatileState)
                 , MonadPersistentState mC
                 , VolatileState volatileState
                 , Config.Config config
                 , Config.Node config ~ node
                 , MonadReader config mC
                 , Term term
                 , Persistent.Term mC ~ term
                 )
              => m (volatileState 'Candidate) (Some volatileState) ()
startElection = let Use.IxMonad{..} = def in do
    incrementCurrentTerm :: mC ()
    voteForSelf
    let impossible = error "Can't be in Follower state right here"
        stillCandidate s = do
            iput s
            resetElectionTimer
            sendRequestVoteToOtherServers
            wrap
        remainLeader s = iput (Some s)
    iget >>= \case
        Some s -> dispatch impossible stillCandidate remainLeader s
  where
    incrementCurrentTerm = let Use.Monad{..} = def in do
        currentTerm <- getCurrentTerm
        setCurrentTerm $ succTerm currentTerm
    voteForSelf = let Use.IxMonad{..} = def in do
        self <- view localNode :: mC node
        voteFor self
    resetElectionTimer = let Use.Monad{..} = def in do
        cancelElectionTimer
        startElectionTimer
    sendRequestVoteToOtherServers = let Use.Monad{..} = def in do
        me <- view localNode
        currentTerm <- getCurrentTerm
        (idx, term') <- maybe (index0, term0) (\(i, t, _) -> (i, t)) <$> lastLogEntry
        broadcastRequestVoteRequest $ def & term .~ currentTerm
                                          & candidateId .~ me
                                          & lastLogIndex .~ idx
                                          & lastLogTerm .~ term'

voteFor :: ( IxMonadState m
           )
        => node
        -> m (volatileState 'Candidate) (Some volatileState) ()
voteFor = undefined

onRequestVoteRequest :: a
onRequestVoteRequest = undefined
onRequestVoteResponse :: a
onRequestVoteResponse = undefined
onElectionTimeout :: a
onElectionTimeout = undefined
onHeartbeatTimeout :: a
onHeartbeatTimeout = undefined
