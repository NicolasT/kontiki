{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Internal.Follower (
      convertToFollower
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import Control.Monad (when)
import Data.Maybe (isNothing)

import Control.Monad.State.Class (MonadState)

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.))

import Data.Default.Class (Default(def))

import Control.Monad.Logger (MonadLogger, logDebug)
import Control.Monad.Reader.Class (MonadReader)

import Kontiki.Raft.Classes.Config (Config)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (HasTerm(term), MonadRPC)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest, candidateId)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RequestVoteRequest
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (voteGranted)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as AppendEntriesRequest
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse, success)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(getCurrentTerm, getVotedFor, setVotedFor))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState)
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startElectionTimer)
import qualified Kontiki.Raft.Classes.Types as T

import Kontiki.Raft.Internal.Candidate (convertToCandidate)
import Kontiki.Raft.Internal.Orphans ()
import Kontiki.Raft.Internal.State (Some)

convertToFollower :: ( IxMonadState m
                     , Monad (m (volatileState 'V.Follower) (volatileState 'V.Follower))
                     , MonadTimers (m (volatileState 'V.Follower) (volatileState 'V.Follower))
                     , VolatileState volatileState
                     )
                  => m (volatileState r) (volatileState 'V.Follower) ()
convertToFollower = imodify (V.convert V.AnyToFollower) >>> resetElectionTimer
  where
    m >>> n = Ix.ibind (const n) m

onRequestVoteRequest :: ( MonadState (volatileState 'V.Follower) m
                        , MonadPersistentState m
                        , MonadTimers m
                        , RequestVoteRequest req
                        , RVResp.RequestVoteResponse resp
                        , Default resp
                        , P.Term m ~ term
                        , RPC.Term req ~ term
                        , RPC.Term resp ~ term
                        , Ord term
                        , RequestVoteRequest.Node req ~ node
                        , P.Node m ~ node
                        , Eq node
                        )
                     => req
                     -> m resp
onRequestVoteRequest req = do
    currentTerm <- getCurrentTerm
    if req ^. term < currentTerm
        then -- Reply false if term < currentTerm (§5.1)
            return $ def & term .~ currentTerm
                         & voteGranted .~ False
        else do
            vf <- getVotedFor
            let node = req ^. candidateId
            let maybeGrantVote = maybe True (== node) vf
            if maybeGrantVote
                then do
                    candidateLogUpToDate <- isCandidateLogUpToDate req
                    when (isNothing vf && candidateLogUpToDate) $
                        setVotedFor (Just node)
                    when candidateLogUpToDate $
                        resetElectionTimer

                    return $ def & term .~ currentTerm
                                 & voteGranted .~ candidateLogUpToDate
                else
                    return $ def & term .~ currentTerm
                                 & voteGranted .~ False
  where
    isCandidateLogUpToDate _ = return True -- TODO

onRequestVoteResponse :: ( MonadLogger m
                         )
                      => node -> resp -> m ()
onRequestVoteResponse _ _ =
    $(logDebug) "Received RequestVote response in Follower mode, ignoring"

onAppendEntriesRequest :: ( Monad m
                          , MonadTimers m
                          , MonadPersistentState m
                          , P.Term m ~ RPC.Term resp
                          , Default resp
                          , AppendEntriesResponse resp
                          )
                       => req
                       -> m resp
onAppendEntriesRequest _ = do
    resetElectionTimer

    term' <- getCurrentTerm
    return $ def & term .~ term'
                 & success .~ False

onAppendEntriesResponse :: ( MonadLogger m
                           )
                        => node -> resp -> m ()
onAppendEntriesResponse _ _ =
    $(logDebug) "Received AppendEntries response in Follower mode, ignoring"

onElectionTimeout :: ( IxMonadState m
                     , MonadReader config m'
                     , MonadPersistentState m'
                     , MonadTimers m'
                     , MonadRPC m'
                     , VolatileState volatileState
                     , Config config
                     , m' ~ m (volatileState 'V.Candidate) (volatileState 'V.Candidate)
                     , req ~ RPC.RequestVoteRequest m'
                     , RPC.Term req ~ term
                     , P.Term m' ~ term
                     , T.Term term
                     , RequestVoteRequest.Index req ~ index
                     , P.Index m' ~ index
                     , T.Index index
                     , Config.Node config ~ node
                     , RequestVoteRequest.Node req ~ node
                     , RequestVoteRequest req
                     , Default req
                     , V.Node volatileState ~ node
                     , Ord node
                     , MonadState (volatileState 'V.Candidate) (m (volatileState 'V.Candidate) (volatileState 'V.Candidate))
                     , MonadTimers (m (volatileState 'V.Leader) (volatileState 'V.Leader))
                     , P.Index (m (volatileState 'V.Leader) (volatileState 'V.Leader)) ~ index
                     , V.Index volatileState ~ index
                     , MonadReader config (m (volatileState 'V.Leader) (volatileState 'V.Leader))
                     , MonadPersistentState (m (volatileState 'V.Leader) (volatileState 'V.Leader))
                     , MonadState (volatileState 'V.Leader) (m (volatileState 'V.Leader) (volatileState 'V.Leader))
                     , P.Term (m (volatileState 'V.Leader) (volatileState 'V.Leader)) ~ RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'V.Leader) (volatileState 'V.Leader)))
                     , AppendEntriesRequest.Node (RPC.AppendEntriesRequest (m (volatileState 'V.Leader) (volatileState 'V.Leader))) ~ node
                     , RPC.Node (m (volatileState 'V.Leader) (volatileState 'V.Leader)) ~ node
                     , AppendEntriesRequest.Index (RPC.AppendEntriesRequest (m (volatileState 'V.Leader) (volatileState 'V.Leader))) ~ index
                     , T.Term (RPC.Term (RPC.AppendEntriesRequest (m (volatileState 'V.Leader) (volatileState 'V.Leader))))
                     , Default (RPC.AppendEntriesRequest (m (volatileState 'V.Leader) (volatileState 'V.Leader)))
                     , AppendEntriesRequest (RPC.AppendEntriesRequest (m (volatileState 'V.Leader) (volatileState 'V.Leader)))
                     , MonadRPC (m (volatileState 'V.Leader) (volatileState 'V.Leader))
                     )
                  => m (volatileState 'V.Follower) (Some volatileState) ()
onElectionTimeout = convertToCandidate

onHeartbeatTimeout :: ( MonadLogger m
                      )
                   => m ()
onHeartbeatTimeout =
    $(logDebug) "Heartbeat timeout in Follower mode, ignoring"


resetElectionTimer :: (Monad m, MonadTimers m) => m ()
resetElectionTimer = do
    cancelElectionTimer
    startElectionTimer
