{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleContexts,
             OverloadedStrings #-}

module Network.Kontiki.Raft.Utils where

import qualified Data.Set as Set

import Data.ByteString.Char8 ()

import Control.Monad.State.Class (get)

import Control.Lens

import Network.Kontiki.Types
import Network.Kontiki.Monad

currentState :: (Functor m, Monad m, Wrapable t) => TransitionT a t m SomeState
currentState = wrap `fmap` get

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

quorumSize :: Monad m => TransitionT a s m Int
quorumSize = do
    nodes <- view configNodes

    return $ Set.size nodes `div` 2 + 1

-- Can't have this in Follower due to recursive imports, bummer
stepDown :: Monad m => Term -> TransitionT a f m SomeState
stepDown term = do
    logS "Stepping down to Follower state"

    resetElectionTimeout
    resubmit

    return $ wrap $ FollowerState { _fCurrentTerm = term
                                  , _fVotedFor = Nothing
                                  }
