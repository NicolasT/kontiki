{-# LANGUAGE MultiWayIf,
             RecordWildCards,
             OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Raft.Follower
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- This module implements the behavior of a node in 
-- `Network.Kontiki.Types.MFollower' mode.
-----------------------------------------------------------------------------
module Network.Kontiki.Raft.Follower where

import Prelude hiding (log)

import Data.ByteString.Char8 ()

import Control.Lens

import Control.Monad (when)

import Network.Kontiki.Log
import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils
import qualified Network.Kontiki.Raft.Candidate as Candidate
import qualified Network.Kontiki.Raft.Leader as Leader

-- | Handles `RequestVote'.
handleRequestVote :: (Functor m, Monad m, MonadLog m a)
                  => MessageHandler RequestVote a Follower m
handleRequestVote sender RequestVote{..} = do
    currentTerm <- use fCurrentTerm

    if | rvTerm < currentTerm -> do
           logS "Received RequestVote for old term"
           send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                             , rvrVoteGranted = False
                                             }
           currentState
       | rvTerm > currentTerm -> do
           logS "Received RequestVote for newer term, bumping"

           fCurrentTerm .= rvTerm
           fVotedFor .= Nothing

           handle'
       | otherwise -> do
           logS "Received RequestVote for current term"
           handle'
  where
    handle' = do
        votedFor <- use fVotedFor
        currentTerm <- use fCurrentTerm

        if | votedFor /= Nothing && votedFor /= Just rvCandidateId -> do
               logS "Granted vote to other node, rejecting"
               send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                                 , rvrVoteGranted = False
                                                 }
               currentState
           | otherwise -> do
               e <- logLastEntry

               let validTerm = maybe True (\e' -> rvLastLogTerm >= eTerm e') e
                   validIndex = maybe True (\e' -> rvLastLogIndex >= eIndex e') e

               if validTerm && validIndex
                   then do
                       logS "Granting vote"
                       send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                                         , rvrVoteGranted = True
                                                         }
                       fVotedFor .= Just sender
                       resetElectionTimeout
                   else do
                       logS "Node out-of-date, rejecting vote"
                       send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                                         , rvrVoteGranted = False
                                                         }

               currentState

-- | Handles `RequestVoteResponse'.
handleRequestVoteResponse :: (Functor m, Monad m)
                          => MessageHandler RequestVoteResponse a Follower m
handleRequestVoteResponse sender RequestVoteResponse{..} = do
    currentTerm <- use fCurrentTerm
    commitIndex <- use fCommitIndex

    if rvrTerm > currentTerm
        then stepDown sender rvrTerm commitIndex
        else currentState

-- | Handles `AppendEntries'.
handleAppendEntries :: (Functor m, Monad m, MonadLog m a)
                    => MessageHandler (AppendEntries a) a Follower m
handleAppendEntries sender AppendEntries{..} = do
    currentTerm <- use fCurrentTerm
    commitIndex <- use fCommitIndex
    e <- logLastEntry
    let lastIndex = maybe index0 eIndex e

    if | aeTerm > currentTerm -> stepDown sender aeTerm commitIndex
       | aeTerm < currentTerm -> do
           send sender $ AppendEntriesResponse { aerTerm = currentTerm
                                               , aerSuccess = False
                                               , aerLastIndex = lastIndex
                                               }
           currentState
       | otherwise -> do
           e' <- logEntry aePrevLogIndex
           let t = maybe term0 eTerm e'

           resetElectionTimeout

           if t /= aePrevLogTerm
               then do
                   send sender $ AppendEntriesResponse { aerTerm = aeTerm
                                                       , aerSuccess = False
                                                       , aerLastIndex = lastIndex
                                                       }
                   currentState
               else do
                   es <- dropWhileM checkTerm aeEntries
                   lastIndex' <- if (not $ null es)
                       then do
                           let truncateTo = prevIndex $ eIndex $ head es
                           truncateLog truncateTo
                           logEntries es
                           return $ eIndex $ last es
                       else return lastIndex

                   when (commitIndex /= aeCommitIndex) $ setCommitIndex aeCommitIndex

                   send sender $ AppendEntriesResponse { aerTerm = aeTerm
                                                       , aerSuccess = True
                                                       , aerLastIndex = lastIndex'
                                                       }

                   currentState

-- | Checks if we have an `Entry' at the index of `e'
-- and if so, whether the terms of the entries match.
checkTerm :: (Monad m, MonadLog m a)
          => Entry a
          -> m Bool
checkTerm e = do
    e' <- logEntry $ eIndex e
    return $ case e' of
        Nothing -> False
        Just e'' -> eTerm e'' == eTerm e

-- | Monadic version of `dropWhile'.
dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM p = loop
  where
    loop es = case es of
        [] -> return []
        (x : xs) -> do
            q <- p x
            if q
                then dropWhileM p xs
                else return (x : xs)

-- | Handles `AppendEntriesResponse'.
handleAppendEntriesResponse :: (Functor m, Monad m)
                            => MessageHandler AppendEntriesResponse a Follower m
handleAppendEntriesResponse _ _ = do
    logS "Received AppendEntriesResponse message in Follower state, ignoring"
    currentState

-- | Handle `ElectionTimeout'.
handleElectionTimeout :: (Functor m, Monad m, MonadLog m a)
                      => TimeoutHandler ElectionTimeout a Follower m
handleElectionTimeout = do
    logS "Election timeout, stepping up"

    currentTerm <- use fCurrentTerm
    let nextTerm = succTerm currentTerm
    commitIndex <- use fCommitIndex

    fCurrentTerm .= nextTerm

    quorum <- quorumSize

    if quorum == 1
        then Leader.stepUp nextTerm commitIndex
        else Candidate.stepUp nextTerm commitIndex

-- | Handles `HeartbeatTimeout'.
handleHeartbeatTimeout :: (Functor m, Monad m)
                       => TimeoutHandler HeartbeatTimeout a Follower m
handleHeartbeatTimeout = do
    logS "Ignoring heartbeat timeout in Follower state"
    currentState

-- | `Handler' for `MFollower' mode.
handle :: (Functor m, Monad m, MonadLog m a) => Handler a Follower m
handle = handleGeneric
            handleRequestVote
            handleRequestVoteResponse
            handleAppendEntries
            handleAppendEntriesResponse
            handleElectionTimeout
            handleHeartbeatTimeout
