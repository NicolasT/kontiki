{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Raft.Classes.Test (tests) where

import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)

import Test.QuickCheck (Arbitrary)

import qualified Kontiki.Raft.Classes.Types as Types
import qualified Kontiki.Raft.Classes.Types.Test as Types
import Kontiki.Raft.Classes.RPC (Term)
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as AEReq
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse)
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse)
import qualified Kontiki.Raft.Classes.RPC.Test as RPC
import qualified Kontiki.Raft.Classes.State.Test as State
import Kontiki.Raft.Classes.State.Volatile (VolatileState)
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Test.Utils (PropLensConstraints)

tests :: forall index term requestVoteRequest requestVoteResponse appendEntriesRequest appendEntriesResponse volatileState.
         ( Types.Index index, Show index, Ord index, Arbitrary index, Typeable index
         , Types.Term term, Show term, Ord term, Arbitrary term, Typeable term
         , Typeable requestVoteRequest, RequestVoteRequest requestVoteRequest
         , PropLensConstraints requestVoteRequest (Term requestVoteRequest)
         , PropLensConstraints requestVoteRequest (RVReq.Node requestVoteRequest)
         , PropLensConstraints requestVoteRequest (RVReq.Index requestVoteRequest)
         , Typeable requestVoteResponse, RequestVoteResponse requestVoteResponse
         , PropLensConstraints requestVoteResponse (Term requestVoteResponse)
         , Typeable appendEntriesRequest, AppendEntriesRequest appendEntriesRequest
         , PropLensConstraints appendEntriesRequest (Term appendEntriesRequest)
         , PropLensConstraints appendEntriesRequest (AEReq.Node appendEntriesRequest)
         , PropLensConstraints appendEntriesRequest (AEReq.Index appendEntriesRequest)
         , PropLensConstraints appendEntriesRequest (AEReq.Entry appendEntriesRequest)
         , Typeable appendEntriesResponse, AppendEntriesResponse appendEntriesResponse
         , PropLensConstraints appendEntriesResponse (Term appendEntriesResponse)
         , VolatileState volatileState
         , Typeable (volatileState 'V.Follower)
         , PropLensConstraints (volatileState 'V.Follower) (V.Index volatileState)
         , Typeable (volatileState 'V.Candidate)
         , PropLensConstraints (volatileState 'V.Candidate) (V.Index volatileState)
         , PropLensConstraints (volatileState 'V.Candidate) (V.Node volatileState), Ord (V.Node volatileState)
         , Typeable (volatileState 'V.Leader)
         , PropLensConstraints (volatileState 'V.Leader) (V.Index volatileState)
         )
      => TestTree
tests = testGroup "Kontiki.Raft.Classes" [
      Types.tests @index @term
    , RPC.tests @requestVoteRequest @requestVoteResponse @appendEntriesRequest @appendEntriesResponse
    , State.tests @volatileState
    ]
