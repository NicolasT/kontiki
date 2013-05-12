{-# LANGUAGE GADTs,
             MultiWayIf,
             RecordWildCards,
             OverloadedStrings #-}

module Network.Kontiki.Raft.Candidate (
      handle
    ) where

import Prelude hiding (log)

import Control.Monad.State.Class (get)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.ByteString.Char8 ()
import qualified Data.ByteString.Builder as B

import Control.Lens

import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils (MessageHandler, TimeoutHandler, handleGeneric, stepDown)

-- | Utility to determine whether a)set of votes forms a majority.
isMajority :: Monad m => Set NodeId -> TransitionT s m Bool
isMajority votes = do
    nodes <- view configNodes
    return $ Set.size votes >= Set.size nodes `div` 2 + 1

currentState :: (Functor m, Monad m)
             => TransitionT (CandidateState a) m (SomeState a)
currentState = (wrap . Candidate) `fmap` get

handleRequestVote :: (Functor m, Monad m)
                  => MessageHandler RequestVote CandidateState m a
handleRequestVote sender RequestVote{..} = do
    currentTerm <- use cCurrentTerm
    l <- use cLog

    if | rvTerm > currentTerm -> stepDown sender rvTerm l
       | otherwise -> do
             log [ B.byteString "Denying term "
                 , logTerm rvTerm
                 , B.byteString " to "
                 , B.byteString sender
                 ]
             send sender $ MRequestVoteResponse
                         $ RequestVoteResponse { rvrTerm = currentTerm
                                               , rvrVoteGranted = False
                                               }
             currentState

handleRequestVoteResponse :: (Functor m, Monad m)
                          => MessageHandler RequestVoteResponse CandidateState m a
handleRequestVoteResponse sender RequestVoteResponse{..} = do
    currentTerm <- use cCurrentTerm
    l <- use cLog

    if | rvrTerm < currentTerm -> do
             logS "Ignoring RequestVoteResponse for old term"
             currentState
       | rvrTerm > currentTerm -> stepDown sender rvrTerm l
       | not rvrVoteGranted -> do
             logS "Ignoring RequestVoteResponse since vote wasn't granted"
             currentState
       | otherwise -> do
             logS "Received valid RequestVoteResponse"

             votes <- Set.insert sender `fmap` use cVotes

             m <- isMajority votes

             if m
                 then becomeLeader
                 else get >>= \s -> return $ wrap $ Candidate $ s { _cVotes = votes }

handleHeartbeat :: (Functor m, Monad m)
                => MessageHandler Heartbeat CandidateState m a
handleHeartbeat sender (Heartbeat term) = do
    currentTerm <- use cCurrentTerm
    l <- use cLog

    if | term > currentTerm -> stepDown sender term l
       | otherwise -> do
             logS "Ignoring heartbeat"
             currentState

handleElectionTimeout :: (Functor m, Monad m)
                      => TimeoutHandler CandidateState m a
handleElectionTimeout = do
    nodeId <- view configNodeId
    currentTerm <- use cCurrentTerm
    l <- use cLog

    logS "Election timeout occurred"

    let state = CandidateState { _cCurrentTerm = succTerm $ currentTerm
                               , _cVotes = Set.singleton nodeId
                               , _cLog = l
                               }

    resetElectionTimeout

    broadcast $ MRequestVote
              $ RequestVote { rvTerm = currentTerm
                            , rvCandidateId = nodeId
                            , rvLastLogIndex = logLastIndex l
                            , rvLastLogTerm = logLastTerm l
                            }

    return $ wrap $ Candidate state

handleHeartbeatTimeout :: (Functor m, Monad m)
                       => TimeoutHandler CandidateState m a
handleHeartbeatTimeout = do
    logS "Ignoring heartbeat timer timeout"
    currentState

becomeLeader :: Monad m
             => TransitionT (CandidateState a) m (SomeState a)
becomeLeader = do
    currentTerm <- use cCurrentTerm
    l <- use cLog

    log [ B.byteString "Becoming leader for term"
        , logTerm $ currentTerm
        ]

    resetHeartbeatTimeout
    broadcast $ MHeartbeat
              $ Heartbeat $ currentTerm

    return $ wrap $ Leader
                  $ LeaderState { _lCurrentTerm = currentTerm
                                , _lLog = l
                                }


-- | Handler for events when in `Candidate' state.
handle :: (Functor m, Monad m) => Handler Candidate a m
handle =
    handleGeneric
        candidateLens
        handleRequestVote
        handleRequestVoteResponse
        handleHeartbeat
        handleElectionTimeout
        handleHeartbeatTimeout
  where
    candidateLens = lens (\(Candidate s) -> s) (\_ s -> Candidate s)
