{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleContexts,
             OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Raft.Util
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- This module contains utility functions used by implementations of
-- different modes.
-----------------------------------------------------------------------------
module Network.Kontiki.Raft.Utils where

import qualified Data.Set as Set

import Data.ByteString.Char8 ()

import Control.Monad.State.Class (get)

import Control.Lens

import Network.Kontiki.Types
import Network.Kontiki.Monad

-- | Gets the current state.
currentState :: (Functor m, Monad m, Wrapable t) => TransitionT a t m SomeState
currentState = wrap `fmap` get

-- TODO Not sure why these *' variants are required... Trickyness with
-- type-families etc. Would rather use the types exported from N.K.Monad
-- instead
type MessageHandler' t a i m = NodeId -> t -> TransitionT a i m SomeState
type TimeoutHandler' t a i m = TransitionT a i m SomeState
type Handler' a i m = Event a -> TransitionT a i m SomeState

-- | Generic handler that unwraps the `Event' and dispatches
-- appropriate `MessageHandler', `TimeoutHandler' or `Handler'.
handleGeneric :: MessageHandler' RequestVote a i m
              -> MessageHandler' RequestVoteResponse a i m
              -> MessageHandler' (AppendEntries a) a i m
              -> MessageHandler' AppendEntriesResponse a i m
              -> TimeoutHandler' ElectionTimeout a i m
              -> TimeoutHandler' HeartbeatTimeout a i m
              -> Handler' a i m
handleGeneric
    handleRequestVote
    handleRequestVoteResponse
    handleAppendEntries
    handleAppendEntriesResponse
    handleElectionTimeout
    handleHeartbeatTimeout
    event = case event of
    EMessage s m -> case m of
        MRequestVote m' -> handleRequestVote s m'
        MRequestVoteResponse m' -> handleRequestVoteResponse s m'
        MAppendEntries m' -> handleAppendEntries s m'
        MAppendEntriesResponse m' -> handleAppendEntriesResponse s m'
    EElectionTimeout -> handleElectionTimeout
    EHeartbeatTimeout -> handleHeartbeatTimeout

-- | Calculates the size of the cluster quorum, defined as:
-- @
--    (size_of_cluster `div` 2) + 1
-- @ 
quorumSize :: Monad m => TransitionT a s m Int
quorumSize = do
    nodes <- view configNodes

    return $ Set.size nodes `div` 2 + 1

-- | Checks if the current set of votes is enough
-- to achieve majority.
isMajority :: Monad m => NodeSet -> TransitionT a s m Bool
isMajority votes = do
    quorum <- quorumSize
    return $ Set.size votes >= quorum

-- | Steps down to `MFollower' mode by resetting the election timeout,
-- sending a `RequestVoteResponse' to `sender' and transitioning to
-- `MFollower' mode.
--  
-- Can't have this in Follower due to recursive imports, bummer
stepDown :: Monad m => NodeId -> Term -> TransitionT a f m SomeState
stepDown sender term = do
    logS "Stepping down to Follower state"

    resetElectionTimeout

    send sender RequestVoteResponse { rvrTerm = term
                                    , rvrVoteGranted = True
                                    }

    return $ wrap FollowerState { _fCurrentTerm = term
                                , _fVotedFor = Just sender
                                }
