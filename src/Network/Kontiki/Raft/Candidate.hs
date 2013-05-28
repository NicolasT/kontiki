{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             MultiWayIf #-}

module Network.Kontiki.Raft.Candidate where

import qualified Data.Set as Set

import Data.ByteString.Char8 ()

import Control.Lens

import Network.Kontiki.Log
import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils
import qualified Network.Kontiki.Raft.Leader as Leader

handleRequestVote :: (Functor m, Monad m) => MessageHandler RequestVote a Candidate m
handleRequestVote sender RequestVote{..} = do
    currentTerm <- use cCurrentTerm

    if rvTerm > currentTerm
        then stepDown rvTerm
        else do
            logS "Not granting vote"
            send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                              , rvrVoteGranted = False
                                              }
            currentState

handleRequestVoteResponse :: (Functor m, Monad m, MonadLog m a)
                          => MessageHandler RequestVoteResponse a Candidate m
handleRequestVoteResponse sender RequestVoteResponse{..} = do
    currentTerm <- use cCurrentTerm
    votes <- use cVotes

    if | rvrTerm < currentTerm -> do
           logS "Ignoring RequestVoteResponse for old term"
           currentState
       | rvrTerm > currentTerm -> stepDown rvrTerm
       | not rvrVoteGranted -> do
           logS "Ignoring RequestVoteResponse since vote wasn't granted"
           currentState
       | Set.member sender votes -> do
           logS "Ignoring duplicate RequestVoteResponse"
           currentState
       | otherwise -> do
           logS "Received valid RequestVoteResponse"
           cVotes %= Set.insert sender

           quorum <- quorumSize
           votes' <- use cVotes

           if Set.size votes' < quorum
               then do
                   logS "No majority yet"
                   currentState
               else do
                   logS "Reached a majority, becoming Leader"
                   Leader.stepUp currentTerm


handleAppendEntries :: (Functor m, Monad m)
                    => MessageHandler (AppendEntries a) a Candidate m
handleAppendEntries _ AppendEntries{..} = do
    currentTerm <- use cCurrentTerm

    if currentTerm <= aeTerm
        then do
            logS "Received AppendEntries for current or newer term"
            stepDown aeTerm
        else do
            logS "Ignoring AppendEntries for old term"
            currentState

handleAppendEntriesResponse :: (Functor m, Monad m)
                            => MessageHandler AppendEntriesResponse a Candidate m
handleAppendEntriesResponse _ _ = do
    logS "Ignoring AppendEntriesResponse message in Candidate mode"
    currentState

handleElectionTimeout :: (Functor m, Monad m, MonadLog m a)
                      => TimeoutHandler ElectionTimeout a Candidate m
handleElectionTimeout = do
    logS "Election timeout in Candidate state"

    resetElectionTimeout

    nodeId <- view configNodeId
    nextTerm <- succTerm `fmap` use cCurrentTerm

    e <- logLastEntry
    let lastIndex = maybe index0 eIndex e
        lastTerm = maybe term0 eTerm e

    broadcast $ RequestVote { rvTerm = nextTerm
                            , rvCandidateId = nodeId
                            , rvLastLogIndex = lastIndex
                            , rvLastLogTerm = lastTerm
                            }

    return $ wrap $ CandidateState { _cCurrentTerm = nextTerm
                                   , _cVotes = Set.singleton nodeId
                                   }


handleHeartbeatTimeout :: (Functor m, Monad m)
                       => TimeoutHandler HeartbeatTimeout a Candidate m
handleHeartbeatTimeout = do
    logS "Ignoring heartbeat timeout in Candidate state"
    currentState

handle :: (Functor m, Monad m, MonadLog m a)
       => Handler a Candidate m
handle = handleGeneric
            handleRequestVote
            handleRequestVoteResponse
            handleAppendEntries
            handleAppendEntriesResponse
            handleElectionTimeout
            handleHeartbeatTimeout


stepUp :: (Functor m, Monad m, MonadLog m a)
       => Term
       -> TransitionT a s m SomeState
stepUp term = do
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

    return $ wrap $ CandidateState { _cCurrentTerm = term
                                   , _cVotes = Set.singleton nodeId
                                   }
