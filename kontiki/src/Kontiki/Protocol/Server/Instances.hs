{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kontiki.Protocol.Server.Instances () where

import Control.Lens (lens)

import Data.Default (Default(def))

import Test.QuickCheck (Arbitrary(arbitrary, shrink))
import qualified Data.Text.Lazy as Text
import Data.Text.Arbitrary ()

import qualified Kontiki.Raft.Classes.Types as T

import qualified Kontiki.Raft.Classes.RPC as RPC
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesResponse as AEResp
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp

import Kontiki.Protocol.Server (Term, Index, NodeId, AppendEntriesResponse, RequestVoteRequest, RequestVoteResponse)
import qualified Kontiki.Protocol.Server as S

instance T.Term Term where
    term0 = Term 0

instance Arbitrary Term where
    arbitrary = Term <$> arbitrary
    shrink = map Term . shrink . termTerm


instance T.Index Index where
    index0 = Index 0
    succIndex = Index . succ . indexIndex

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary
    shrink = map Index . shrink . indexIndex

instance Default Index where
    def = Index def


instance Arbitrary NodeId where
    arbitrary = NodeId . Text.fromStrict <$> arbitrary
    shrink = map (NodeId . Text.fromStrict) . shrink . Text.toStrict . nodeIdNode


instance RPC.HasTerm RequestVoteRequest where
    type Term RequestVoteRequest = Term

    term = lens (Term . S.requestVoteRequestTerm) (\r t -> r { S.requestVoteRequestTerm = getTerm t })

instance RVReq.RequestVoteRequest RequestVoteRequest where
    type Node RequestVoteRequest = Node
    type Index RequestVoteRequest = Index

    candidateId = lens (Node . Text.toStrict . S.requestVoteRequestCandidateId) (\r n -> r { S.requestVoteRequestCandidateId = Text.fromStrict $ getNode n })
    lastLogIndex = lens (Index . S.requestVoteRequestLastLogIndex) (\r i -> r { S.requestVoteRequestLastLogIndex = getIndex i })
    lastLogTerm = lens (Term . S.requestVoteRequestLastLogTerm) (\r t -> r { S.requestVoteRequestLastLogTerm = getTerm t })

instance Arbitrary RequestVoteRequest where
    arbitrary = S.RequestVoteRequest <$> arbitrary
                                     <*> (Text.fromStrict <$> arbitrary)
                                     <*> arbitrary
                                     <*> arbitrary

instance Default RequestVoteRequest where
    def = S.RequestVoteRequest def (Text.empty) def def


instance RPC.HasTerm RequestVoteResponse where
    type Term RequestVoteResponse = Term

    term = lens (Term . S.requestVoteResponseTerm) (\r t -> r { S.requestVoteResponseTerm = getTerm t })

instance RVResp.RequestVoteResponse RequestVoteResponse where
    voteGranted = lens S.requestVoteResponseVoteGranted (\r g -> r { S.requestVoteResponseVoteGranted = g })

instance Default RequestVoteResponse where
    def = S.RequestVoteResponse def False

instance Arbitrary RequestVoteResponse where
    arbitrary = S.RequestVoteResponse <$> arbitrary
                                      <*> arbitrary


instance RPC.HasTerm AppendEntriesResponse where
    type Term AppendEntriesResponse = Term

    term = lens (Term . S.appendEntriesResponseTerm) (\r t -> r { S.appendEntriesResponseTerm = getTerm t })

instance AEResp.AppendEntriesResponse AppendEntriesResponse where
    success = lens S.appendEntriesResponseSuccess (\r s -> r { S.appendEntriesResponseSuccess = s })

instance Arbitrary AppendEntriesResponse where
    arbitrary = S.AppendEntriesResponse <$> arbitrary
                                        <*> arbitrary
