{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Protocol.Types (
      Term(Term, getTerm)
    , Index(Index, getIndex)
    , Node(Node, getNode)
    , Entry(Entry, getEntry)
    , RequestVoteRequest
    , RequestVoteResponse
    , AppendEntriesRequest
    , AppendEntriesResponse
    ) where

import Data.Word (Word64)
import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Text.Arbitrary ()

import qualified Data.Vector as V

import Control.Lens (lens)

import Data.Default (Default(def))

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import Proto3.Suite.Class (Message, MessageField, Named)
import Proto3.Suite.Types (NestedVec(NestedVec, nestedvec))

import qualified Kontiki.Raft.Classes.Types as T
import qualified Kontiki.Raft.Classes.RPC as RPC hiding (MonadRPC(..))
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RPC
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RPC
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as RPC
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesResponse as RPC

newtype Term = Term { getTerm :: Word64 }
    deriving (Show, Eq, Ord, Generic)

instance Message Term
deriving instance MessageField Term
instance Named Term

instance Default Term

instance Arbitrary Term where
    arbitrary = Term <$> arbitrary
    shrink = map Term . shrink . getTerm

instance T.Term Term where
    term0 = Term 0


newtype Index = Index { getIndex :: Word64 }
    deriving (Show, Eq, Ord, Generic)

instance Message Index
deriving instance MessageField Index
instance Named Index

instance Default Index

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary
    shrink = map Index . shrink . getIndex

instance T.Index Index where
    index0 = Index 0
    succIndex = Index . succ . getIndex


newtype Node = Node { getNode :: Text }
    deriving (Show, Eq, Generic)

deriving instance MessageField Node
instance Named Node

instance Default Node where
    def = Node ""

instance Arbitrary Node where
    arbitrary = Node <$> arbitrary
    shrink = map Node . shrink . getNode


newtype Entry = Entry { getEntry :: Word64 }
    deriving (Show, Eq, Generic)

instance Message Entry
instance Named Entry

instance Default Entry

instance Arbitrary Entry where
    arbitrary = Entry <$> arbitrary
    shrink = map Entry . shrink . getEntry


data RequestVoteRequest = RequestVoteRequest { requestVoteRequestTerm :: {-# UNPACK #-} !Term
                                             , requestVoteRequestCandidateId :: {-# UNPACK #-} !Node
                                             , requestVoteRequestLastLogIndex :: {-# UNPACK #-} !Index
                                             , requestVoteRequestLastLogTerm :: {-# UNPACK #-} !Term
                                             }
    deriving (Show, Eq, Generic)

instance Message RequestVoteRequest
instance Named RequestVoteRequest

instance Default RequestVoteRequest

instance Arbitrary RequestVoteRequest where
    arbitrary = RequestVoteRequest <$> arbitrary
                                   <*> arbitrary
                                   <*> arbitrary
                                   <*> arbitrary

instance RPC.HasTerm RequestVoteRequest where
    type Term RequestVoteRequest = Term
    term = lens requestVoteRequestTerm (\r t -> r { requestVoteRequestTerm = t })

instance RPC.RequestVoteRequest RequestVoteRequest where
    type Node RequestVoteRequest = Node
    type Index RequestVoteRequest = Index

    candidateId = lens requestVoteRequestCandidateId (\r n -> r { requestVoteRequestCandidateId = n })
    lastLogIndex = lens requestVoteRequestLastLogIndex (\r i -> r { requestVoteRequestLastLogIndex = i })
    lastLogTerm = lens requestVoteRequestLastLogTerm (\r t -> r { requestVoteRequestLastLogTerm = t })


data RequestVoteResponse = RequestVoteResponse { requestVoteResponseTerm :: {-# UNPACK #-} !Term
                                               , requestVoteResponseVoteGranted :: {-# UNPACK #-} !Bool
                                               }
    deriving (Show, Eq, Generic)

instance Message RequestVoteResponse
instance Named RequestVoteResponse

instance Default RequestVoteResponse where
    def = RequestVoteResponse def False

instance Arbitrary RequestVoteResponse where
    arbitrary = RequestVoteResponse <$> arbitrary
                                    <*> arbitrary

instance RPC.HasTerm RequestVoteResponse where
    type Term RequestVoteResponse = Term
    term = lens requestVoteResponseTerm (\r t -> r { requestVoteResponseTerm = t })

instance RPC.RequestVoteResponse RequestVoteResponse where
    voteGranted = lens requestVoteResponseVoteGranted (\r g -> r { requestVoteResponseVoteGranted = g })


data AppendEntriesRequest = AppendEntriesRequest { appendEntriesRequestTerm :: {-# UNPACK #-} !Term
                                                 , appendEntriesRequestLeaderId :: {-# UNPACK #-} !Node
                                                 , appendEntriesRequestPrevLogIndex :: {-# UNPACK #-} !Index
                                                 , appendEntriesRequestPrevLogTerm :: {-# UNPACK #-} !Term
                                                 , appendEntriesRequestEntries :: {-# UNPACK #-} !(NestedVec Entry)
                                                 , appendEntriesRequestLeaderCommit :: {-# UNPACK #-} !Index
                                                 }
    deriving (Show, Eq, Generic)

instance Message AppendEntriesRequest
instance Named AppendEntriesRequest

instance Default AppendEntriesRequest where
    def = AppendEntriesRequest def def def def (NestedVec (V.fromList def)) def

instance Arbitrary AppendEntriesRequest where
    arbitrary = AppendEntriesRequest <$> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary

instance RPC.HasTerm AppendEntriesRequest where
    type Term AppendEntriesRequest = Term

    term = lens appendEntriesRequestTerm (\r t -> r { appendEntriesRequestTerm = t })

instance RPC.AppendEntriesRequest AppendEntriesRequest where
    type Node AppendEntriesRequest = Node
    type Index AppendEntriesRequest = Index
    type Entry AppendEntriesRequest = Entry

    leaderId = lens appendEntriesRequestLeaderId (\r l -> r { appendEntriesRequestLeaderId = l })
    prevLogIndex = lens appendEntriesRequestPrevLogIndex (\r i -> r { appendEntriesRequestPrevLogIndex = i })
    prevLogTerm = lens appendEntriesRequestPrevLogTerm (\r t -> r { appendEntriesRequestPrevLogTerm = t })
    entries = lens (V.toList . nestedvec . appendEntriesRequestEntries) (\r e -> r { appendEntriesRequestEntries = NestedVec (V.fromList e) })
    leaderCommit = lens appendEntriesRequestLeaderCommit (\r c -> r { appendEntriesRequestLeaderCommit = c })


data AppendEntriesResponse = AppendEntriesResponse { appendEntriesResponseTerm :: {-# UNPACK #-} !Term
                                                   , appendEntriesResponseSuccess :: {-# UNPACK #-} !Bool
                                                   }
    deriving (Show, Eq, Generic)

instance Message AppendEntriesResponse
instance Named AppendEntriesResponse

instance Default AppendEntriesResponse where
    def = AppendEntriesResponse def False

instance Arbitrary AppendEntriesResponse where
    arbitrary = AppendEntriesResponse <$> arbitrary
                                      <*> arbitrary

instance RPC.HasTerm AppendEntriesResponse where
    type Term AppendEntriesResponse = Term

    term = lens appendEntriesResponseTerm (\r t -> r { appendEntriesResponseTerm = t })

instance RPC.AppendEntriesResponse AppendEntriesResponse where
    success = lens appendEntriesResponseSuccess (\r s -> r { appendEntriesResponseSuccess = s })
