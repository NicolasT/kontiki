{-# LANGUAGE MultiWayIf,
             RecordWildCards,
             OverloadedStrings #-}

module Network.Kontiki.Raft.Follower where

import Prelude hiding (log)

import Data.ByteString.Char8 ()

import Control.Lens

import Network.Kontiki.Log
import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils
import qualified Network.Kontiki.Raft.Candidate as Candidate
import qualified Network.Kontiki.Raft.Leader as Leader

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

handleRequestVoteResponse :: (Functor m, Monad m)
                          => MessageHandler RequestVoteResponse a Follower m
handleRequestVoteResponse _ RequestVoteResponse{..} = do
    currentTerm <- use fCurrentTerm

    if rvrTerm > currentTerm
        then stepDown rvrTerm
        else currentState

handleAppendEntries :: (Functor m, Monad m, MonadLog m a)
                    => MessageHandler (AppendEntries a) a Follower m
handleAppendEntries sender AppendEntries{..} = do
    currentTerm <- use fCurrentTerm
    e <- logLastEntry
    let lastIndex = maybe index0 eIndex e

    if | aeTerm > currentTerm -> stepDown aeTerm
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
                           truncateLog aePrevLogIndex
                           logEntries es
                           return $ eIndex $ last es
                       else return lastIndex

                   send sender $ AppendEntriesResponse { aerTerm = aeTerm
                                                       , aerSuccess = True
                                                       , aerLastIndex = lastIndex'
                                                       }

                   currentState

checkTerm :: (Monad m, MonadLog m a)
          => Entry a
          -> m Bool
checkTerm e = do
    e' <- logEntry $ eIndex e
    return $ case e' of
        Nothing -> False
        Just e'' -> eTerm e'' == eTerm e

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


handleAppendEntriesResponse :: (Functor m, Monad m)
                            => MessageHandler AppendEntriesResponse a Follower m
handleAppendEntriesResponse _ _ = do
    logS "Received AppendEntriesResponse message in Follower state, ignoring"
    currentState


handleElectionTimeout :: (Functor m, Monad m, MonadLog m a)
                      => TimeoutHandler ElectionTimeout a Follower m
handleElectionTimeout = do
    logS "Election timeout, stepping up"

    currentTerm <- use fCurrentTerm
    let nextTerm = succTerm currentTerm

    fCurrentTerm .= nextTerm

    quorum <- quorumSize

    if quorum == 1
        then Leader.stepUp nextTerm
        else Candidate.stepUp nextTerm

handleHeartbeatTimeout :: (Functor m, Monad m)
                       => TimeoutHandler HeartbeatTimeout a Follower m
handleHeartbeatTimeout = do
    logS "Ignoring heartbeat timeout in Follower state"
    currentState

handle :: (Functor m, Monad m, MonadLog m a) => Handler a Follower m
handle = handleGeneric
            handleRequestVote
            handleRequestVoteResponse
            handleAppendEntries
            handleAppendEntriesResponse
            handleElectionTimeout
            handleHeartbeatTimeout
