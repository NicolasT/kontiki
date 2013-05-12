{-# LANGUAGE GADTs,
             MultiWayIf,
             RecordWildCards,
             OverloadedStrings #-}

module Network.Kontiki.Raft.Leader (
      handle
    ) where

import Prelude hiding (log)

import Control.Monad.State.Class (get)

import Data.ByteString.Char8 ()

import Control.Lens

import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils (MessageHandler, TimeoutHandler, handleGeneric, stepDown)

currentState :: (Functor m, Monad m)
             => TransitionT (LeaderState a) m (SomeState a)
currentState = (wrap . Leader) `fmap` get

handleRequestVote :: (Functor m, Monad m)
                  => MessageHandler RequestVote LeaderState m a
handleRequestVote sender RequestVote{..} = do
    currentTerm <- use lCurrentTerm
    l <- use lLog

    if | rvTerm > currentTerm -> stepDown sender rvTerm l
       | otherwise -> do
           logS "Ignore RequestVote for old term"
           currentState

handleRequestVoteResponse :: (Functor m, Monad m)
                          => MessageHandler RequestVoteResponse LeaderState m a
handleRequestVoteResponse _ _ = do
    -- TODO Stepdown if rvrTerm > current?
    logS "Ignoring RequestVoteResponse in leader state"
    currentState

handleHeartbeat :: (Functor m, Monad m)
                => MessageHandler Heartbeat LeaderState m a
handleHeartbeat sender (Heartbeat term) = do
    currentTerm <- use lCurrentTerm
    l <- use lLog
    if | term > currentTerm -> stepDown sender term l
       | otherwise -> do
           logS "Ignore heartbeat of old term"
           currentState

handleElectionTimeout :: (Functor m, Monad m)
                      => TimeoutHandler LeaderState m a
handleElectionTimeout = do
    -- TODO Can this be ignored? Stepdown instead?
    logS "Ignore election timeout"
    currentState

handleHeartbeatTimeout :: (Functor m, Monad m)
                       => TimeoutHandler LeaderState m a
handleHeartbeatTimeout = do
    logS "Sending heartbeats"

    resetHeartbeatTimeout

    currentTerm <- use lCurrentTerm

    broadcast $ MHeartbeat
              $ Heartbeat currentTerm

    currentState


-- | Handler for events when in `Leader' state.
handle :: (Functor m, Monad m)
       => Handler Leader a m
handle =
    handleGeneric
        leaderLens
        handleRequestVote
        handleRequestVoteResponse
        handleHeartbeat
        handleElectionTimeout
        handleHeartbeatTimeout
  where
    leaderLens :: Lens' (Leader a) (LeaderState a)
    leaderLens = lens (\(Leader s) -> s) (\_ s -> Leader s)
