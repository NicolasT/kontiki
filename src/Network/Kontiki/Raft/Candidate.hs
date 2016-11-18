{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             MultiWayIf #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Raft.Candidate
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- This module implements the behavior of a node in 
-- `Network.Kontiki.Types.MCandidate' mode.
-----------------------------------------------------------------------------
module Network.Kontiki.Raft.Candidate where

import qualified Data.Set as Set

import Data.ByteString.Char8 ()

import Control.Lens (use, view, (%=))

import Network.Kontiki.Log
import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils
import qualified Network.Kontiki.Raft.Leader as Leader

-- | Handles `RequestVote'.
handleRequestVote :: (Monad m) => MessageHandler RequestVote a Candidate m
handleRequestVote sender RequestVote{..} = do
    currentTerm <- use cCurrentTerm
    commitIndex <- use cCommitIndex

    if rvTerm > currentTerm
        then stepDown sender rvTerm commitIndex
        else do
            logS "Not granting vote"
            send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                              , rvrVoteGranted = False
                                              }
            currentState

-- | Handles `RequestVoteResponse'.
handleRequestVoteResponse :: (Monad m, MonadLog m a)
                          => MessageHandler RequestVoteResponse a Candidate m
handleRequestVoteResponse sender RequestVoteResponse{..} = do
    currentTerm <- use cCurrentTerm
    commitIndex <- use cCommitIndex
    votes <- use cVotes

    if | rvrTerm < currentTerm -> do
           logS "Ignoring RequestVoteResponse for old term"
           currentState
       | rvrTerm > currentTerm -> stepDown sender rvrTerm commitIndex
       | not rvrVoteGranted -> do
           logS "Ignoring RequestVoteResponse since vote wasn't granted"
           currentState
       | Set.member sender votes -> do
           logS "Ignoring duplicate RequestVoteResponse"
           currentState
       | otherwise -> do
           logS "Received valid RequestVoteResponse"
           cVotes %= Set.insert sender

           hasMajority <- isMajority =<< use cVotes

           if (not hasMajority)
               then do
                   logS "No majority yet"
                   currentState
               else do
                   logS "Reached a majority, becoming Leader"
                   Leader.stepUp currentTerm commitIndex

-- | Handles `AppendEntries'.
handleAppendEntries :: (Monad m)
                    => MessageHandler (AppendEntries a) a Candidate m
handleAppendEntries sender AppendEntries{..} = do
    currentTerm <- use cCurrentTerm
    commitIndex <- use cCommitIndex

    if currentTerm <= aeTerm
        then do
            logS "Received AppendEntries for current or newer term"
            stepDown sender aeTerm commitIndex
        else do
            logS "Ignoring AppendEntries for old term"
            currentState

-- | Handles `AppendEntriesResponse'.
handleAppendEntriesResponse :: (Monad m)
                            => MessageHandler AppendEntriesResponse a Candidate m
handleAppendEntriesResponse _ _ = do
    logS "Ignoring AppendEntriesResponse message in Candidate mode"
    currentState

-- | Handles `ElectionTimeout'.
handleElectionTimeout :: (Monad m, MonadLog m a)
                      => TimeoutHandler ElectionTimeout a Candidate m
handleElectionTimeout = do
    logS "Election timeout in Candidate state"

    resetElectionTimeout

    nodeId <- view configNodeId
    nextTerm <- succTerm `fmap` use cCurrentTerm
    commitIndex <- use cCommitIndex

    e <- logLastEntry
    let lastIndex = maybe index0 eIndex e
        lastTerm = maybe term0 eTerm e

    broadcast RequestVote { rvTerm = nextTerm
                          , rvCandidateId = nodeId
                          , rvLastLogIndex = lastIndex
                          , rvLastLogTerm = lastTerm
                          }

    return $ wrap CandidateState { _cCurrentTerm = nextTerm
                                 , _cCommitIndex = commitIndex
                                 , _cVotes = Set.singleton nodeId
                                 }

-- | Handles `HeartbeatTimeout'.
handleHeartbeatTimeout :: (Monad m)
                       => TimeoutHandler HeartbeatTimeout a Candidate m
handleHeartbeatTimeout = do
    resetHeartbeatTimeout
    logS "Ignoring heartbeat timeout in Candidate state"
    currentState

-- | `Handler' for `MCandidate' mode.
handle :: (Monad m, MonadLog m a)
       => Handler a Candidate m
handle = handleGeneric
            handleRequestVote
            handleRequestVoteResponse
            handleAppendEntries
            handleAppendEntriesResponse
            handleElectionTimeout
            handleHeartbeatTimeout

-- | Transitions into `MCandidate' mode with this `term'
-- and resets the election timer. 
stepUp :: (Monad m, MonadLog m a)
       => Term
       -> Index
       -> TransitionT a s m SomeState
stepUp term commitIndex = do
    logS "Becoming candidate"

    resetElectionTimeout

    nodeId <- view configNodeId

    e <- logLastEntry
    let lastIndex = maybe index0 eIndex e
        lastTerm = maybe term0 eTerm e

    broadcast $ RequestVote { rvTerm = term
                            , rvCandidateId = nodeId
                            , rvLastLogIndex = lastIndex
                            , rvLastLogTerm = lastTerm
                            }

    return $ wrap CandidateState { _cCurrentTerm = term
                                 , _cCommitIndex = commitIndex
                                 , _cVotes = Set.singleton nodeId
                                 }
