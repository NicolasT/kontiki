{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             ScopedTypeVariables,
             MultiWayIf #-}

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

handleRequestVote :: (Functor m, Monad m)
                  => MessageHandler RequestVote a Leader m
handleRequestVote sender RequestVote{..} = do
    currentTerm <- use lCurrentTerm

    if rvTerm > currentTerm
        then stepDown rvTerm
        else do
            logS "Not granting vote"
            send sender $ RequestVoteResponse { rvrTerm = currentTerm
                                              , rvrVoteGranted = False
                                              }
            currentState

handleRequestVoteResponse :: (Functor m, Monad m)
                          => MessageHandler RequestVoteResponse a Leader m
handleRequestVoteResponse _ RequestVoteResponse{..} = do
    currentTerm <- use lCurrentTerm

    if rvrTerm > currentTerm
        then stepDown rvrTerm
        else currentState

handleAppendEntries :: (Functor m, Monad m)
                    => MessageHandler (AppendEntries a) a Leader m
handleAppendEntries _ AppendEntries{..} = do
    currentTerm <- use lCurrentTerm

    if aeTerm > currentTerm
        then stepDown aeTerm
        else currentState

handleAppendEntriesResponse :: (Functor m, Monad m)
                            => MessageHandler AppendEntriesResponse a Leader m
handleAppendEntriesResponse sender AppendEntriesResponse{..} = do
    currentTerm <- use lCurrentTerm

    if | aerTerm < currentTerm -> do
           logS "Ignoring old AppendEntriesResponse"
           currentState
       | aerTerm > currentTerm -> stepDown aerTerm
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
           currentState

handleElectionTimeout :: (Functor m, Monad m)
                      => TimeoutHandler ElectionTimeout a Leader m
handleElectionTimeout = currentState

handleHeartbeatTimeout :: (Functor m, Monad m, MonadLog m a)
                       => TimeoutHandler HeartbeatTimeout a Leader m
handleHeartbeatTimeout = do
    resetHeartbeatTimeout

    currentTerm <- use lCurrentTerm

    lastEntry <- logLastEntry

    lastIndices <- Map.elems `fmap` use lLastIndex
    let sorted = sortBy (\a b -> compare b a) lastIndices
    quorum <- quorumSize
    let quorumIndex = sorted !! (quorum - 1)

    -- TODO Check paper. CommitIndex can only be in current term if there's
    -- a prior accepted item in the same term?

    e <- logEntry quorumIndex
    let commitIndex =
            if maybe term0 eTerm e >= currentTerm
                then quorumIndex
                else index0

    nodes <- view configNodes
    nodeId <- view configNodeId
    let otherNodes = filter (/= nodeId) (Set.toList nodes)
    mapM_ (sendAppendEntries lastEntry commitIndex) otherNodes

    currentState

sendAppendEntries :: (Monad m, MonadLog m a)
                  => Maybe (Entry a)
                  -> Index
                  -> NodeId
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
        then send node $ AppendEntries { aeTerm = currentTerm
                                       , aeLeaderId = nodeId
                                       , aePrevLogIndex = lastIndex
                                       , aePrevLogTerm = lastTerm
                                       , aeEntries = []
                                       , aeCommitIndex = commitIndex
                                       }
        else do
            e <- logEntry (prevIndex $ eIndex $ head entries)
            send node $ AppendEntries { aeTerm = currentTerm
                                      , aeLeaderId = nodeId
                                      , aePrevLogIndex = maybe index0 eIndex e
                                      , aePrevLogTerm = maybe term0 eTerm e
                                      , aeEntries = entries
                                      , aeCommitIndex = commitIndex
                                      }

handle :: (Functor m, Monad m, MonadLog m a)
       => Handler a Leader m
handle = handleGeneric
            handleRequestVote
            handleRequestVoteResponse
            handleAppendEntries
            handleAppendEntriesResponse
            handleElectionTimeout
            handleHeartbeatTimeout


stepUp :: (Functor m, Monad m, MonadLog m a)
       => Term
       -> TransitionT a f m SomeState
stepUp t = do
    logS "Becoming leader"

    resetHeartbeatTimeout

    e <- logLastEntry
    let lastIndex = maybe index0 eIndex e
        lastTerm = maybe term0 eTerm e

    nodeId <- view configNodeId

    broadcast $ AppendEntries { aeTerm = t
                              , aeLeaderId = nodeId
                              , aePrevLogIndex = lastIndex
                              , aePrevLogTerm = lastTerm
                              , aeEntries = []
                              , aeCommitIndex = index0
                              }

    nodes <- view configNodes
    let ni = Map.fromList $ map (\n -> (n, succIndex lastIndex)) (Set.toList nodes)
        li = Map.fromList $ map (\n -> (n, index0)) (Set.toList nodes)

    return $ wrap $ LeaderState { _lCurrentTerm = t
                                , _lNextIndex = ni
                                , _lLastIndex = li
                                }
