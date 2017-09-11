{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kontiki.Raft.Classes.State.Test (tests) where

import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)

import Kontiki.Raft.Classes.State.Volatile (
    HasCommitIndex(commitIndex), HasLastApplied(lastApplied), HasVotesGranted(votesGranted),
    Role(Candidate, Follower, Leader), VolatileState(Index, Node))
import Kontiki.Raft.Classes.Test.Utils (PropLensConstraints, prop_lens, typeId)

tests :: forall volatileState.
         ( VolatileState volatileState
         , Typeable (volatileState 'Follower), PropLensConstraints (volatileState 'Follower) (Index volatileState)
         , Typeable (volatileState 'Candidate), PropLensConstraints (volatileState 'Candidate) (Index volatileState)
             , PropLensConstraints (volatileState 'Candidate) (Node volatileState), Ord (Node volatileState)
         , Typeable (volatileState 'Leader), PropLensConstraints (volatileState 'Leader) (Index volatileState)
         )
      => TestTree
tests = testGroup "Kontiki.Raft.Classes.State" [
      testGroup ("VolatileState 'Follower @" ++ typeId @(volatileState 'Follower)) [
          prop_lens @(volatileState 'Follower) "commitIndex" commitIndex
        , prop_lens @(volatileState 'Follower) "lastApplied" lastApplied
        ]
    , testGroup ("VolatileState 'Candidate @" ++ typeId @(volatileState 'Candidate)) [
          prop_lens @(volatileState 'Candidate) "commitIndex" commitIndex
        , prop_lens @(volatileState 'Candidate) "lastApplied" lastApplied
        , prop_lens @(volatileState 'Candidate) "votesGranted" votesGranted
        ]
    , testGroup ("VolatileState 'Leader @" ++ typeId @(volatileState 'Leader)) [
          prop_lens @(volatileState 'Leader) "commitIndex" commitIndex
        , prop_lens @(volatileState 'Leader) "lastApplied" lastApplied
        ]
    ]
