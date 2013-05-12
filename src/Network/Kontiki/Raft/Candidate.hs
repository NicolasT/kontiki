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
import Network.Kontiki.Raft.Utils

-- | Utility to determine whether a)set of votes forms a majority.
isMajority :: Monad m => Set NodeId -> TransitionT s m Bool
isMajority votes = do
    nodes <- view configNodes
    return $ Set.size votes >= Set.size nodes `div` 2 + 1

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
    ignore = (wrap . Candidate) `fmap` get

    handleRequestVote sender RequestVote{..} = do
        currentTerm <- use cCurrentTerm

        if | rvTerm > currentTerm -> stepDown sender rvTerm
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
                 ignore

    handleRequestVoteResponse sender RequestVoteResponse{..} = do
        currentTerm <- use cCurrentTerm
        if | rvrTerm < currentTerm -> do
                 logS "Ignoring RequestVoteResponse for old term"
                 ignore
           | rvrTerm > currentTerm -> stepDown sender rvrTerm
           | not rvrVoteGranted -> do
                 logS "Ignoring RequestVoteResponse since vote wasn't granted"
                 ignore
           | otherwise -> do
                 logS "Received valid RequestVoteResponse"

                 votes <- Set.insert sender `fmap` use cVotes

                 m <- isMajority votes

                 if m
                     then becomeLeader
                     else get >>= \s -> return $ wrap $ Candidate $ s { _cVotes = votes }

    handleHeartbeat sender (Heartbeat term) = do
        currentTerm <- use cCurrentTerm
        if | term > currentTerm -> stepDown sender term
           | otherwise -> do
                 logS "Ignoring heartbeat"
                 ignore

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

    handleHeartbeatTimeout = do
        logS "Ignoring heartbeat timer timeout"
        ignore

    stepDown sender term = do
        log [ B.byteString "Stepping down, received term "
            , logTerm term
            , B.byteString " from "
            , B.byteString sender
            ]

        send sender $ MRequestVoteResponse
                    $ RequestVoteResponse { rvrTerm = term
                                          , rvrVoteGranted = True
                                          }
        resetElectionTimeout

        l <- use cLog

        return $ wrap $ Follower
                      $ FollowerState { _fCurrentTerm = term
                                      , _fVotedFor = Just sender
                                      , _fLog = l
                                      }

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


