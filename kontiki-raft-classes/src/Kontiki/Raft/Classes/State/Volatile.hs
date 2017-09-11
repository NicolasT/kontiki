{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.State.Volatile (
      VolatileState(..)
    , VolatileFollowerState
    , VolatileCandidateState
    , VolatileLeaderState
    , Role(..)
    , Conversion(..)
    , HasCommitIndex(..)
    , HasLastApplied(..)
    , HasVotesGranted(..)
    ) where

import Data.Set (Set)

import Kontiki.Raft.Classes.Lens (Lens')

data Role = Follower | Candidate | Leader
    deriving (Show, Eq)

data Conversion (from :: Role) (to :: Role) where
    FollowerToCandidate :: Conversion 'Follower 'Candidate
    CandidateToLeader :: Conversion 'Candidate 'Leader
    AnyToFollower :: Conversion any 'Follower

class HasCommitIndex s a | s -> a where
    commitIndex :: Lens' s a

class HasLastApplied s a | s -> a where
    lastApplied :: Lens' s a

class HasVotesGranted s a | s -> a where
    votesGranted :: Lens' s a

class ( VolatileFollowerState (s 'Follower)
      , VolatileCandidateState (s 'Candidate)
      , VolatileLeaderState (s 'Leader)
      ) => VolatileState s where
    type Index s
    type Node s

    initialize :: Index s -> Index s -> s 'Follower
    convert :: Conversion from to -> s from -> s to
    dispatch :: (s 'Follower -> a)
             -> (s 'Candidate -> a)
             -> (s 'Leader -> a)
             -> s r
             -> a

class ( HasCommitIndex s (Index (Base s))
      , HasLastApplied s (Index (Base s))
      ) => VolatileFollowerState s

class ( HasCommitIndex s (Index (Base s))
      , HasLastApplied s (Index (Base s))
      , HasVotesGranted s (Set (Node (Base s)))
      ) => VolatileCandidateState s

class ( HasCommitIndex s (Index (Base s))
      , HasLastApplied s (Index (Base s))
      ) => VolatileLeaderState s

type family Base s where
    Base (s (r :: Role)) = s
