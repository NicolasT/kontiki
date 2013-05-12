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
import Network.Kontiki.Raft.Utils (MessageHandler, TimeoutHandler, handleGeneric)

currentState :: (Functor m, Monad m)
             => TransitionT (FollowerState a) m (SomeState a)
currentState = (wrap . Follower) `fmap` get

handleRequestVote :: (Functor m, Monad m)
                  => MessageHandler RequestVote FollowerState m a
handleRequestVote sender msg@RequestVote{..} = do
    currentTerm <- use fCurrentTerm

    if | rvTerm < currentTerm -> do
           logS "Received RequestVote for old term"
           send sender $ MRequestVoteResponse
                       $ RequestVoteResponse { rvrTerm = currentTerm
                                             , rvrVoteGranted = False
                                             }
           currentState
       | rvTerm > currentTerm -> do
           logS "Received RequestVote for newer term, bumping"

           fCurrentTerm .= rvTerm
           fVotedFor .= Nothing

           handleRequestVote2 sender msg
       | otherwise -> do
           logS "Recieved RequestVote for current term"
           handleRequestVote2 sender msg

handleRequestVote2 :: (Functor m, Monad m)
                   => MessageHandler RequestVote FollowerState m a
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
           currentState

handleRequestVoteResponse :: (Functor m, Monad m)
                          => MessageHandler RequestVoteResponse FollowerState m a
handleRequestVoteResponse _ _ = do
    logS "Received RequestVoteResponse message in Follower state, ignoring"
    currentState

handleHeartbeat :: (Functor m, Monad m)
                => MessageHandler Heartbeat FollowerState m a
handleHeartbeat _ (Heartbeat term) = do
    currentTerm <- use fCurrentTerm

    if | term == currentTerm -> do
           logS "Received heartbeat"
           resetElectionTimeout
           currentState
       | otherwise -> do
           logS "Ignoring heartbeat"
           currentState

handleElectionTimeout :: (Functor m, Monad m)
                      => TimeoutHandler FollowerState m a
handleElectionTimeout = becomeCandidate

handleHeartbeatTimeout :: (Functor m, Monad m)
                       => TimeoutHandler FollowerState m a
handleHeartbeatTimeout = do
    logS "Ignoring heartbeat timeout"
    currentState

-- TODO Handle single-node case
becomeCandidate :: (Functor m, Monad m)
                => TransitionT (FollowerState a) m (SomeState a)
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


handle :: (Functor m, Monad m)
       => Handler Follower a m
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


