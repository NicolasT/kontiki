{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Raft.Classes.Test (tests) where

import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)

import Test.QuickCheck (Arbitrary)

import qualified Kontiki.Raft.Classes.Types as T
import qualified Kontiki.Raft.Classes.Types.Test as KRCT
import Kontiki.Raft.Classes.RPC (Term)
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse)
import qualified Kontiki.Raft.Classes.RPC.Test as KRCR
import Kontiki.Raft.Classes.Test.Utils (PropLensConstraints)

tests :: forall index term requestVoteRequest requestVoteResponse.
         ( T.Index index, Show index, Ord index, Arbitrary index, Typeable index
         , T.Term term, Show term, Ord term, Arbitrary term, Typeable term
         , Typeable requestVoteRequest
         , Typeable requestVoteRequest, RequestVoteRequest requestVoteRequest
         , PropLensConstraints requestVoteRequest (Term requestVoteRequest)
         , PropLensConstraints requestVoteRequest (RVReq.Node requestVoteRequest)
         , PropLensConstraints requestVoteRequest (RVReq.Index requestVoteRequest)
         , Typeable requestVoteResponse, RequestVoteResponse requestVoteResponse
         , PropLensConstraints requestVoteResponse (Term requestVoteResponse)
         )
      => TestTree
tests = testGroup "Kontiki.Raft.Classes" [
      KRCT.tests @index @term
    , KRCR.tests @requestVoteRequest @requestVoteResponse
    ]
