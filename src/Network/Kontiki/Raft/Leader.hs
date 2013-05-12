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
import qualified Data.ByteString.Builder as B

import Control.Lens

import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils (handleGeneric)

-- | Handler for events when in `Leader' state.
handle :: (Functor m, Monad m) => Handler Leader a m
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
    ignore = (wrap . Leader) `fmap` get

    handleRequestVote sender RequestVote{..} = do
        currentTerm <- use lCurrentTerm
        if | rvTerm > currentTerm -> stepDown sender rvTerm
           | otherwise -> logS "Ignore RequestVote for old term" >> ignore

    handleRequestVoteResponse _ _ = do
        -- TODO Stepdown if rvrTerm > current?
        logS "Ignoring RequestVoteResponse in leader state"
        ignore

    handleHeartbeat sender (Heartbeat term) = do
        currentTerm <- use lCurrentTerm
        if | term > currentTerm -> stepDown sender term
           | otherwise -> logS "Ignore heartbeat of old term" >> ignore

    handleElectionTimeout = do
        -- TODO Can this be ignored? Stepdown instead?
        logS "Ignore election timeout"
        ignore

    handleHeartbeatTimeout = do
        logS "Sending heartbeats"

        resetHeartbeatTimeout

        currentTerm <- use lCurrentTerm

        broadcast $ MHeartbeat
                  $ Heartbeat currentTerm

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

        l <- use lLog

        return $ wrap $ Follower
                      $ FollowerState { _fCurrentTerm = term
                                      , _fVotedFor = Just sender
                                      , _fLog = l
                                      }


