{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.State.Volatile (
      VolatileState
    ) where

import Control.Lens (lens)
-- import Data.Default (Default(def))

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Aeson (ToJSON(toJSON), (.=), object)
import Data.Text (Text)

import Test.QuickCheck (Arbitrary(arbitrary))

import qualified Kontiki.Raft.Classes.State.Volatile as K

import Kontiki.Protocol.Types (Index, Node)

data VolatileState r where
    Follower :: {-# UNPACK #-} !FollowerState -> VolatileState 'K.Follower
    Candidate :: {-# UNPACK #-} !CandidateState -> VolatileState 'K.Candidate
    Leader :: {-# UNPACK #-} !LeaderState -> VolatileState 'K.Leader

deriving instance Eq (VolatileState r)
deriving instance Show (VolatileState r)


instance K.VolatileState VolatileState where
    type Index VolatileState = Index
    type Node VolatileState = Node

    initialize ci la = Follower $ FollowerState { followerStateCommitIndex = ci
                                                , followerStateLastApplied = la
                                                }
    convert c s = case c of
        K.FollowerToCandidate -> case s of
            Follower s' -> Candidate $ CandidateState { candidateStateCommitIndex = followerStateCommitIndex s'
                                                      , candidateStateLastApplied = followerStateLastApplied s'
                                                      , candidateStateVotesGranted = Set.empty
                                                      }
        K.CandidateToLeader -> case s of
            Candidate s' -> Leader $ LeaderState { leaderStateCommitIndex = candidateStateCommitIndex s'
                                                 , leaderStateLastApplied = candidateStateLastApplied s'
                                                 }
        K.AnyToFollower -> case s of
            Follower s' -> Follower s'
            Candidate s' -> Follower $ FollowerState { followerStateCommitIndex = candidateStateCommitIndex s'
                                                     , followerStateLastApplied = candidateStateLastApplied s'
                                                     }
            Leader s' -> Follower $ FollowerState { followerStateCommitIndex = leaderStateCommitIndex s'
                                                  , followerStateLastApplied = leaderStateLastApplied s'
                                                  }
    dispatch f c l s = case s of
        Follower _ -> f s
        Candidate _ -> c s
        Leader _ -> l s

instance K.HasCommitIndex (VolatileState r) Index where
    commitIndex = lens
        (\case
            Follower s -> followerStateCommitIndex s
            Candidate s -> candidateStateCommitIndex s
            Leader s -> leaderStateCommitIndex s)
        (\s i -> case s of
            Follower s' -> Follower $ s' { followerStateCommitIndex = i }
            Candidate s' -> Candidate $ s' { candidateStateCommitIndex = i }
            Leader s' -> Leader $ s' { leaderStateCommitIndex = i })

instance K.HasLastApplied (VolatileState r) Index where
    lastApplied = lens
        (\case
            Follower s -> followerStateLastApplied s
            Candidate s -> candidateStateLastApplied s
            Leader s -> leaderStateLastApplied s)
        (\s i -> case s of
            Follower s' -> Follower $ s' { followerStateLastApplied = i }
            Candidate s' -> Candidate $ s' { candidateStateLastApplied  = i }
            Leader s' -> Leader $ s' { leaderStateLastApplied = i })

instance K.HasVotesGranted (VolatileState 'K.Candidate) (Set Node) where
    votesGranted = lens
        (\case
            Candidate s -> candidateStateVotesGranted s)
        (\s i -> case s of
            Candidate s' -> Candidate $ s' { candidateStateVotesGranted = i })

instance K.VolatileFollowerState (VolatileState 'K.Follower)
instance K.VolatileCandidateState (VolatileState 'K.Candidate)
instance K.VolatileLeaderState (VolatileState 'K.Leader)

instance ToJSON (VolatileState r) where
    toJSON = \case
        Follower s -> toJSON s
        Candidate s -> toJSON s
        Leader s -> toJSON s

instance Arbitrary (VolatileState 'K.Follower) where
    arbitrary = Follower <$> arbitrary

instance Arbitrary (VolatileState 'K.Candidate) where
    arbitrary = Candidate <$> arbitrary

instance Arbitrary (VolatileState 'K.Leader) where
    arbitrary = Leader <$> arbitrary


data FollowerState = FollowerState { followerStateCommitIndex :: {-# UNPACK #-} !Index
                                   , followerStateLastApplied :: {-# UNPACK #-} !Index
                                   }
    deriving (Eq, Show)

instance ToJSON FollowerState where
    toJSON s = object [ "role" .= ("follower" :: Text)
                      , "commitIndex" .= followerStateCommitIndex s
                      , "lastApplied" .= followerStateLastApplied s
                      ]
instance Arbitrary FollowerState where
    arbitrary = FollowerState <$> arbitrary
                              <*> arbitrary


data CandidateState = CandidateState { candidateStateCommitIndex :: {-# UNPACK #-} !Index
                                     , candidateStateLastApplied :: {-# UNPACK #-} !Index
                                     , candidateStateVotesGranted :: {-# UNPACK #-} !(Set Node)
                                     }
    deriving (Eq, Show)

instance ToJSON CandidateState where
    toJSON s = object [ "role" .= ("candidate" :: Text)
                      , "commitIndex" .= candidateStateCommitIndex s
                      , "lastApplied" .= candidateStateLastApplied s
                      , "votesGranted" .= candidateStateVotesGranted s
                      ]

instance Arbitrary CandidateState where
    arbitrary = CandidateState <$> arbitrary
                               <*> arbitrary
                               <*> arbitrary


data LeaderState = LeaderState { leaderStateCommitIndex :: {-# UNPACK #-} !Index
                               , leaderStateLastApplied :: {-# UNPACK #-} !Index
                               }
    deriving (Eq, Show)

instance ToJSON LeaderState where
    toJSON s = object [ "role" .= ("leader" :: Text)
                      , "commitIndex" .= leaderStateCommitIndex s
                      , "lastApplied" .= leaderStateLastApplied s
                      ]

instance Arbitrary LeaderState where
    arbitrary = LeaderState <$> arbitrary
                            <*> arbitrary
