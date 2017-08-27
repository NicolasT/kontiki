{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kontiki.Protocol.Server.Instances () where

import Control.Lens (lens)

import Data.Default (Default(def))

import Test.QuickCheck (Arbitrary, arbitrary)
import qualified Data.Text.Lazy as Text
import Data.Text.Arbitrary ()

import qualified Kontiki.Raft.Classes.RPC as RPC
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesResponse as AEResp
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp

import Kontiki.Types (Term(Term, getTerm), Index(Index, getIndex), Node(Node, getNode))
import Kontiki.Protocol.Server (AppendEntriesResponse, RequestVoteRequest, RequestVoteResponse)
import qualified Kontiki.Protocol.Server as S

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
