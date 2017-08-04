{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Types (
      Index
    , Term
    , Node
    , VolatileState
    , RequestVoteRequest
    ) where

import Data.Word (Word64)

import GHC.Generics (Generic)

import Data.Default.Class (Default)

import Control.Lens (lens)

import qualified Kontiki.Raft.Classes.RPC as RPC
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import qualified Kontiki.Raft.Classes.State.Volatile as V
import qualified Kontiki.Raft.Classes.Types as T

newtype Index = Index Word64
    deriving (Show, Eq, Generic)

instance Default Index

instance T.Index Index where
    index0 = Index 0
    succIndex (Index i) = Index (succ i)


newtype Term = Term Word64
    deriving (Show, Eq, Generic)

instance Default Term

instance T.Term Term where
    term0 = Term 0


newtype Node = Node Int
    deriving (Show, Eq, Generic)

instance Default Node


data VolatileState = VolatileState { volatileStateCommitIndex :: Index
                                   , volatileStateLastApplied :: Index
                                   }
    deriving (Show, Eq, Generic)

instance Default VolatileState

instance V.VolatileState VolatileState where
    type Index VolatileState = Index

    commitIndex = lens volatileStateCommitIndex (\v i -> v { volatileStateCommitIndex = i })
    lastApplied = lens volatileStateLastApplied (\v l -> v { volatileStateLastApplied = l })


data RequestVoteRequest = RequestVoteRequest { requestVoteRequestTerm :: Term
                                             , requestVoteRequestCandidateId :: Node
                                             , requestVoteRequestLastLogIndex :: Index
                                             , requestVoteRequestLastLogTerm :: Term
                                             }
    deriving (Show, Eq, Generic)

instance Default RequestVoteRequest

instance RPC.HasTerm RequestVoteRequest where
    type Term RequestVoteRequest = Term

    term = lens requestVoteRequestTerm (\r t -> r { requestVoteRequestTerm = t })

instance RVReq.RequestVoteRequest RequestVoteRequest where
    type Index RequestVoteRequest = Index
    type Node RequestVoteRequest = Node

    candidateId = lens requestVoteRequestCandidateId (\r c -> r { requestVoteRequestCandidateId = c })
    lastLogIndex = lens requestVoteRequestLastLogIndex (\r i -> r { requestVoteRequestLastLogIndex = i })
    lastLogTerm = lens requestVoteRequestLastLogTerm (\r t -> r { requestVoteRequestLastLogTerm = t })
