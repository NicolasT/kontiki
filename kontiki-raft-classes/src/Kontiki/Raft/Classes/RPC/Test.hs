{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Raft.Classes.RPC.Test (tests) where

import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)

import Kontiki.Raft.Classes.RPC (Term, term)
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import Kontiki.Raft.Classes.Test.Utils (PropLensConstraints, prop_lens, typeId)

tests :: forall requestVoteRequest requestVoteResponse.
         ( Typeable requestVoteRequest, RequestVoteRequest requestVoteRequest
         , PropLensConstraints requestVoteRequest (Term requestVoteRequest)
         , PropLensConstraints requestVoteRequest (RVReq.Node requestVoteRequest)
         , PropLensConstraints requestVoteRequest (RVReq.Index requestVoteRequest)
         , Typeable requestVoteResponse, RequestVoteResponse requestVoteResponse
         , PropLensConstraints requestVoteResponse (Term requestVoteResponse)
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
    ]
