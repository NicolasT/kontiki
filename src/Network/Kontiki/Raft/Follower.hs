{-# LANGUAGE GADTs,
             MultiWayIf,
             RecordWildCards,
             OverloadedStrings #-}

module Network.Kontiki.Raft.Follower (
      handle
    ) where

import Prelude hiding (log)

import Control.Monad.State.Class (get)

import qualified Data.Set as Set

import Data.ByteString.Char8 ()
import qualified Data.ByteString.Builder as B

import Control.Lens

import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils (handleGeneric)

handle :: (Functor m, Monad m) => Handler Follower a m
handle =
    handleGeneric
        followerLens
        handleRequestVote
        handleRequestVoteResponse
        handleHeartbeat
        handleElectionTimeout
        handleHeartbeatTimeout
  where
    followerLens = lens (\(Follower s) -> s) (\_ s -> Follower s)
    ignore = (wrap . Follower) `fmap` get

    handleRequestVote sender msg@RequestVote{..} = do
        currentTerm <- use fCurrentTerm

        if | rvTerm < currentTerm -> do
               logS "Received RequestVote for old term"
               send sender $ MRequestVoteResponse
                           $ RequestVoteResponse { rvrTerm = currentTerm
                                                 , rvrVoteGranted = False
                                                 }
               ignore
           | rvTerm > currentTerm -> do
               logS "Received RequestVote for newer term, bumping"

               fCurrentTerm .= rvTerm
               fVotedFor .= Nothing

               handleRequestVote2 sender msg
           | otherwise -> do
               logS "Recieved RequestVote for current term"
               handleRequestVote2 sender msg

    handleRequestVote2 sender RequestVote{..} = do
        votedFor <- use fVotedFor
        currentTerm <- use fCurrentTerm
        l <- use fLog

        if | (votedFor == Nothing || votedFor == Just rvCandidateId)
               && (rvLastLogIndex >= logLastIndex l && rvLastLogTerm >= logLastTerm l) -> do
               log [ B.byteString "Granting vote for term "
                   , logTerm rvTerm
                   , B.byteString " to "
                   , B.byteString sender
                   ]

               send sender $ MRequestVoteResponse
                           $ RequestVoteResponse { rvrTerm = currentTerm
                                                 , rvrVoteGranted = True
                                                 }

               resetElectionTimeout

               get >>= \s -> return $ wrap $ Follower s{ _fVotedFor = Just sender }
            | otherwise -> do
               log [ B.byteString "Rejecting vote for term "
                   , logTerm rvTerm
                   , B.byteString " to "
                   , B.byteString sender
                   ]
               send sender $ MRequestVoteResponse
                           $ RequestVoteResponse { rvrTerm = currentTerm
                                                 , rvrVoteGranted = False
                                                 }
               ignore

    handleRequestVoteResponse _ _ = do
        logS "Received RequestVoteResponse message in Follower state, ignoring"
        ignore

    handleHeartbeat _ (Heartbeat term) = do
        currentTerm <- use fCurrentTerm

        if | term == currentTerm -> do
               logS "Received heartbeat"
               resetElectionTimeout
               ignore
           | otherwise -> do
               logS "Ignoring heartbeat"
               ignore

    handleElectionTimeout = becomeCandidate

    handleHeartbeatTimeout = do
        logS "Ignoring heartbeat timeout"
        ignore

    -- TODO Handle single-node case
    becomeCandidate = do
        currentTerm <- use fCurrentTerm
        let nextTerm = succTerm currentTerm

        log [ B.byteString "Becoming candidate for term "
            , logTerm nextTerm
            ]

        resetElectionTimeout

        nodeId <- view configNodeId
        l <- use fLog

        let state = CandidateState { _cCurrentTerm = nextTerm
                                   , _cVotes = Set.singleton nodeId
                                   , _cLog = l
                                   }

        let l = state ^. cLog
            t = state ^. cCurrentTerm

        broadcast $ MRequestVote
                  $ RequestVote { rvTerm = t
                                , rvCandidateId = nodeId
                                , rvLastLogIndex = logLastIndex l
                                , rvLastLogTerm = logLastTerm l
                                }

        return $ wrap $ Candidate state


