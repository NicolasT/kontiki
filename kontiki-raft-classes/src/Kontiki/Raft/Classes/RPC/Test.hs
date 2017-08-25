{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Raft.Classes.RPC.Test (tests) where

import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)

import Kontiki.Raft.Classes.RPC (Term, term)
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as AEReq
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse)
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesResponse as AEResp
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import Kontiki.Raft.Classes.Test.Utils (PropLensConstraints, prop_lens, typeId)

tests :: forall requestVoteRequest requestVoteResponse appendEntriesRequest appendEntriesResponse.
         ( Typeable requestVoteRequest, RequestVoteRequest requestVoteRequest
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
         )
      => TestTree
tests = testGroup "Kontiki.Raft.Classes.RPC" [
      testGroup ("RequestVoteRequest @" ++ typeId @requestVoteRequest) [
          prop_lens @requestVoteRequest "term" term
        , prop_lens @requestVoteRequest "candidateId" RVReq.candidateId
        , prop_lens @requestVoteRequest "lastLogIndex" RVReq.lastLogIndex
        , prop_lens @requestVoteRequest "lastLogTerm" RVReq.lastLogTerm
        ]
    , testGroup ("RequestVoteResponse @" ++ typeId @requestVoteResponse) [
          prop_lens @requestVoteResponse "term" term
        , prop_lens @requestVoteResponse "voteGranted" RVResp.voteGranted
        ]
    , testGroup ("AppendEntriesRequest @" ++ typeId @appendEntriesRequest) [
          prop_lens @appendEntriesRequest "term" term
        , prop_lens @appendEntriesRequest "leaderId" AEReq.leaderId
        , prop_lens @appendEntriesRequest "prevLogIndex" AEReq.prevLogIndex
        , prop_lens @appendEntriesRequest "prevLogTerm" AEReq.prevLogTerm
        , prop_lens @appendEntriesRequest "entries" AEReq.entries
        , prop_lens @appendEntriesRequest "leaderCommit" AEReq.leaderCommit
        ]
    , testGroup ("AppendEntriesResponse @" ++ typeId @appendEntriesResponse) [
          prop_lens @appendEntriesResponse "term" term
        , prop_lens @appendEntriesResponse "success" AEResp.success
        ]
    ]
