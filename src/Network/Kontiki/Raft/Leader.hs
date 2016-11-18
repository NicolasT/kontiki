{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             ScopedTypeVariables,
             MultiWayIf #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Raft.Leader
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- This module implements the behavior of a node in 
-- `Network.Kontiki.Types.MLeader' mode.
-----------------------------------------------------------------------------
module Network.Kontiki.Raft.Leader where

import Data.List (sortBy)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.ByteString.Char8 ()

import Control.Monad (when)

import Control.Lens hiding (Index)

import Network.Kontiki.Log
import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils


-- | Handles `RequestVote'.
handleRequestVote :: (Monad m)
                  => MessageHandler RequestVote a Leader m
handleRequestVote sender RequestVote{..} = do
    currentTerm <- use lCurrentTerm
    commitIndex <- use lCommitIndex

    if rvTerm > currentTerm
        then stepDown sender rvTerm commitIndex
        else do
            logS "Not granting vote"
            send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                              , rvrVoteGranted = False
                                              }
            currentState

-- | Handle `RequestVoteResponse'.
handleRequestVoteResponse :: (Monad m)
                          => MessageHandler RequestVoteResponse a Leader m
handleRequestVoteResponse sender RequestVoteResponse{..} = do
    currentTerm <- use lCurrentTerm
    commitIndex <- use lCommitIndex

    if rvrTerm > currentTerm
        then stepDown sender rvrTerm commitIndex
        else currentState

-- | Handles `AppendEntries'.
handleAppendEntries :: (Monad m)
                    => MessageHandler (AppendEntries a) a Leader m
handleAppendEntries sender AppendEntries{..} = do
    currentTerm <- use lCurrentTerm
    commitIndex <- use lCommitIndex

    if aeTerm > currentTerm
        then stepDown sender aeTerm commitIndex
        else currentState

-- | Handles `AppendEntriesResponse'.
handleAppendEntriesResponse :: (Monad m)
                            => MessageHandler AppendEntriesResponse a Leader m
handleAppendEntriesResponse sender AppendEntriesResponse{..} = do
    currentTerm <- use lCurrentTerm
    commitIndex <- use lCommitIndex

    if | aerTerm < currentTerm -> do
           logS "Ignoring old AppendEntriesResponse"
           currentState
       | aerTerm > currentTerm -> stepDown sender aerTerm commitIndex
       | not aerSuccess -> do
           lNextIndex %= Map.alter (\i -> Just $ maybe index0 prevIndex i) sender
           currentState
       | otherwise -> do
           lastIndices <- use lLastIndex
           let li = maybe index0 id (Map.lookup sender lastIndices)
           -- Ignore if this is an old message
           when (aerLastIndex >= li) $ do
               lLastIndex %= Map.insert sender aerLastIndex
               lNextIndex %= Map.insert sender aerLastIndex
               newQuorumIndex <- quorumIndex
               when (newQuorumIndex > commitIndex) $ do
                   lCommitIndex .= newQuorumIndex
                   setCommitIndex newQuorumIndex
           currentState

-- | Calculates current quorum `Index' from nodes' latest indices
quorumIndex :: (Monad m)
            => TransitionT a LeaderState m Index
quorumIndex = do
    lastIndices <- Map.elems `fmap` use lLastIndex
    let sorted = sortBy (\a b -> compare b a) lastIndices
    quorum <- quorumSize
    return $ sorted !! (quorum - 1)

-- | Handles `ElectionTimeout'.
handleElectionTimeout :: (Monad m)
                      => TimeoutHandler ElectionTimeout a Leader m
handleElectionTimeout =  do
  resetElectionTimeout
  logS "Ignoring election timeout in Leader state"
  currentState

-- | Handles `HeartbeatTimeout'.
handleHeartbeatTimeout :: (Monad m, MonadLog m a)
                       => TimeoutHandler HeartbeatTimeout a Leader m
handleHeartbeatTimeout = do
    resetHeartbeatTimeout

    commitIndex <- use lCommitIndex

    lastEntry <- logLastEntry

    nodeId <- view configNodeId
    lLastIndex %= Map.insert nodeId (maybe index0 eIndex lastEntry)

    nodes <- view configNodes
    let otherNodes = filter (/= nodeId) (Set.toList nodes)
    mapM_ (sendAppendEntries lastEntry commitIndex) otherNodes

    currentState

-- | Sends `AppendEntries' to a particular `NodeId'.
sendAppendEntries :: (Monad m, MonadLog m a)
                  => Maybe (Entry a) -- ^ `Entry' to append
                  -> Index           -- ^ `Index' up to which the `Follower' should commit
                  -> NodeId          -- ^ `NodeId' to send to
                  -> TransitionT a LeaderState m ()
sendAppendEntries lastEntry commitIndex node = do
    currentTerm <- use lCurrentTerm

    nextIndices <- use lNextIndex

    let lastIndex = maybe index0 eIndex lastEntry
        lastTerm = maybe term0 eTerm lastEntry
        nextIndex = (Map.!) nextIndices node

    let getEntries acc idx
            | idx <= nextIndex = return acc
            | otherwise = do
                entry <- logEntry idx
                -- TODO Handle failure
                getEntries (maybe undefined id entry : acc) (prevIndex idx)

    entries <- getEntries [] lastIndex

    nodeId <- view configNodeId

    if null entries
        then send node AppendEntries { aeTerm = currentTerm
                                     , aeLeaderId = nodeId
                                     , aePrevLogIndex = lastIndex
                                     , aePrevLogTerm = lastTerm
                                     , aeEntries = []
                                     , aeCommitIndex = commitIndex
                                     }
        else do
            e <- logEntry (prevIndex $ eIndex $ head entries)
            send node AppendEntries { aeTerm = currentTerm
                                    , aeLeaderId = nodeId
                                    , aePrevLogIndex = maybe index0 eIndex e
                                    , aePrevLogTerm = maybe term0 eTerm e
                                    , aeEntries = entries
                                    , aeCommitIndex = commitIndex
                                    }

-- | `Handler' for `MLeader' mode.
handle :: (Monad m, MonadLog m a)
       => Handler a Leader m
handle = handleGeneric
            handleRequestVote
            handleRequestVoteResponse
            handleAppendEntries
            handleAppendEntriesResponse
            handleElectionTimeout
            handleHeartbeatTimeout

-- | Transitions into `MLeader' mode by broadcasting heartbeat `AppendEntries'
-- to all nodes and changing state to `LeaderState'. 
stepUp :: (Monad m, MonadLog m a)
       => Term    -- ^ `Term' of the `Leader'
       -> Index   -- ^ commit `Index'
       -> TransitionT a f m SomeState
stepUp term commitIndex = do
    logS "Becoming leader"

    resetHeartbeatTimeout

    e <- logLastEntry
    let lastIndex = maybe index0 eIndex e
        lastTerm = maybe term0 eTerm e

    nodeId <- view configNodeId

    broadcast $ AppendEntries { aeTerm = term
                              , aeLeaderId = nodeId
                              , aePrevLogIndex = lastIndex
                              , aePrevLogTerm = lastTerm
                              , aeEntries = []
                              , aeCommitIndex = index0
                              }

    nodes <- view configNodes
    let ni = Map.fromList $ map (\n -> (n, succIndex lastIndex)) (Set.toList nodes)
        li = Map.fromList $ map (\n -> (n, index0)) (Set.toList nodes)

    return $ wrap $ LeaderState { _lCurrentTerm = term
                                , _lCommitIndex = commitIndex
                                , _lNextIndex = ni
                                , _lLastIndex = li
                                }
