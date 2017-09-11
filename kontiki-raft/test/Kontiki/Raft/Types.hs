{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Types (
      Term(..)
    , Index(..)
    , Node(..)
    , RequestVoteRequest(..)
    , RequestVoteResponse(..)
    , AppendEntriesRequest(..)
    , AppendEntriesResponse(..)
    , PersistentState(..)
    , runPersistentStateT
    , VolatileState(..)
    , TimersT(..)
    , runTimersT
    ) where

import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Logger (MonadLogger)

import Control.Lens (lens)

import Data.Default (Default(def))

import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import qualified Kontiki.Raft.Classes.Types as T
import qualified Kontiki.Raft.Classes.RPC as RPC
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as AEReq
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesResponse as AEResp
import qualified Kontiki.Raft.Classes.State.Persistent as P
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Timers (MonadTimers(startElectionTimer, cancelElectionTimer, startHeartbeatTimer))

newtype Term = Term { getTerm :: Word64 }
    deriving (Show, Eq, Ord, Typeable, Generic)

instance T.Term Term where
    term0 = Term 0
    succTerm = Term . succ . getTerm

instance Arbitrary Term where
    arbitrary = Term <$> arbitrary
    shrink = map Term . shrink . getTerm

instance Default Term


newtype Index = Index { getIndex :: Word64 }
    deriving (Show, Eq, Ord, Typeable, Generic)

instance T.Index Index where
    index0 = Index 0
    succIndex = Index . succ . getIndex

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary
    shrink = map Index . shrink . getIndex

instance Default Index


newtype Node = Node { getNode :: Int }
    deriving (Show, Eq, Ord, Typeable, Generic)

instance Arbitrary Node where
    arbitrary = Node <$> arbitrary
    shrink = map Node . shrink . getNode

instance Default Node


newtype Entry = Entry { getEntry :: Int }
    deriving (Show, Eq, Typeable, Generic)

instance Arbitrary Entry where
    arbitrary = Entry <$> arbitrary
    shrink = map Entry . shrink . getEntry


data RequestVoteRequest = RequestVoteRequest { requestVoteRequestTerm :: Term
                                             , requestVoteRequestCandidateId :: Node
                                             , requestVoteRequestLastLogIndex :: Index
                                             , requestVoteRequestLastLogTerm :: Term
                                             }
    deriving (Show, Eq, Typeable, Generic)

instance RPC.HasTerm RequestVoteRequest where
    type Term RequestVoteRequest = Term

    term = lens requestVoteRequestTerm (\r t -> r { requestVoteRequestTerm = t })

instance RVReq.RequestVoteRequest RequestVoteRequest where
    type Node RequestVoteRequest = Node
    type Index RequestVoteRequest = Index

    candidateId = lens requestVoteRequestCandidateId (\r n -> r { requestVoteRequestCandidateId = n })
    lastLogIndex = lens requestVoteRequestLastLogIndex (\r i -> r { requestVoteRequestLastLogIndex = i })
    lastLogTerm = lens requestVoteRequestLastLogTerm (\r t -> r { requestVoteRequestLastLogTerm = t })

instance Arbitrary RequestVoteRequest where
    arbitrary = RequestVoteRequest <$> arbitrary
                                   <*> arbitrary
                                   <*> arbitrary
                                   <*> arbitrary


data RequestVoteResponse = RequestVoteResponse { requestVoteResponseTerm :: Term
                                               , requestVoteResponseVoteGranted :: Bool
                                               }
    deriving (Show, Eq, Typeable, Generic)

instance RPC.HasTerm RequestVoteResponse where
    type Term RequestVoteResponse = Term

    term = lens requestVoteResponseTerm (\r t -> r { requestVoteResponseTerm = t })

instance RVResp.RequestVoteResponse RequestVoteResponse where
    voteGranted = lens requestVoteResponseVoteGranted (\r v -> r { requestVoteResponseVoteGranted = v })

instance Arbitrary RequestVoteResponse where
    arbitrary = RequestVoteResponse <$> arbitrary
                                    <*> arbitrary


data AppendEntriesRequest = AppendEntriesRequest { appendEntriesRequestTerm :: Term
                                                 , appendEntriesRequestLeaderId :: Node
                                                 , appendEntriesRequestPrevLogIndex :: Index
                                                 , appendEntriesRequestPrevLogTerm :: Term
                                                 , appendEntriesRequestEntries :: [Entry]
                                                 , appendEntriesRequestLeaderCommit :: Index
                                                 }
    deriving (Show, Eq, Typeable, Generic)

instance RPC.HasTerm AppendEntriesRequest where
    type Term AppendEntriesRequest = Term

    term = lens appendEntriesRequestTerm (\r t -> r { appendEntriesRequestTerm = t })

instance AEReq.AppendEntriesRequest AppendEntriesRequest where
    type Node AppendEntriesRequest = Node
    type Index AppendEntriesRequest = Index
    type Entry AppendEntriesRequest = Entry

    leaderId = lens appendEntriesRequestLeaderId (\r l -> r { appendEntriesRequestLeaderId = l })
    prevLogIndex = lens appendEntriesRequestPrevLogIndex (\r i -> r { appendEntriesRequestPrevLogIndex = i })
    prevLogTerm = lens appendEntriesRequestPrevLogTerm (\r t -> r { appendEntriesRequestPrevLogTerm = t })
    entries = lens appendEntriesRequestEntries (\r e -> r { appendEntriesRequestEntries = e })
    leaderCommit = lens appendEntriesRequestLeaderCommit (\r c -> r { appendEntriesRequestLeaderCommit = c })

instance Arbitrary AppendEntriesRequest where
    arbitrary = AppendEntriesRequest <$> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary
                                     <*> arbitrary


data AppendEntriesResponse = AppendEntriesResponse { appendEntriesResponseTerm :: Term
                                                   , appendEntriesResponseSuccess :: Bool
                                                   }
    deriving (Show, Eq, Typeable, Generic)

instance RPC.HasTerm AppendEntriesResponse where
    type Term AppendEntriesResponse = Term

    term = lens appendEntriesResponseTerm (\r t -> r { appendEntriesResponseTerm = t })

instance AEResp.AppendEntriesResponse AppendEntriesResponse where
    success = lens appendEntriesResponseSuccess (\r s -> r { appendEntriesResponseSuccess = s })

instance Arbitrary AppendEntriesResponse where
    arbitrary = AppendEntriesResponse <$> arbitrary
                                      <*> arbitrary


data PersistentState = PersistentState { persistentStateCurrentTerm :: Term
                                       , persistentStateVotedFor :: Maybe Node
                                       }
    deriving (Show, Eq)

instance Default PersistentState where
    def = PersistentState (Term 0) Nothing

newtype PersistentStateT m a = PersistentStateT { unPersistentStateT :: StateT PersistentState m a }
    deriving (Functor, Applicative, Monad, MonadLogger, MonadTimers)

instance Monad m => P.MonadPersistentState (PersistentStateT m) where
    type Term (PersistentStateT m) = Term
    type Node (PersistentStateT m) = Node
    type Entry (PersistentStateT m) = Entry
    type Index (PersistentStateT m) = Index

    getCurrentTerm = PersistentStateT $ gets persistentStateCurrentTerm
    setCurrentTerm t = PersistentStateT $ modify (\s -> s { persistentStateCurrentTerm = t })

    getVotedFor = PersistentStateT $ gets persistentStateVotedFor
    setVotedFor v = PersistentStateT $ modify (\s -> s { persistentStateVotedFor = v })

    getLogEntry = error "Not implemented"
    setLogEntry = error "Not implemented"
    lastLogEntry = error "Not implemented"

runPersistentStateT :: PersistentState -> PersistentStateT m a -> m (a, PersistentState)
runPersistentStateT s a = runStateT (unPersistentStateT a) s


data VolatileState r where
    Follower :: Index -> Index -> VolatileState 'V.Follower
    Candidate :: Index -> Index -> Set Node -> VolatileState 'V.Candidate
    Leader :: Index -> Index -> VolatileState 'V.Leader

deriving instance Eq (VolatileState r)
deriving instance Show (VolatileState r)

instance V.VolatileState VolatileState where
    type Index VolatileState = Index
    type Node VolatileState = Node

    initialize a b = Follower a b
    convert c s = case c of
        V.FollowerToCandidate -> case s of
            Follower a b -> Candidate a b Set.empty
        V.CandidateToLeader -> case s of
            Candidate a b _ -> Leader a b
        V.AnyToFollower -> case s of
            Follower a b -> Follower a b
            Candidate a b _ -> Follower a b
            Leader a b -> Follower a b
    dispatch f c l s = case s of
        Follower _ _ -> f s
        Candidate _ _ _ -> c s
        Leader _ _ -> l s

instance V.VolatileFollowerState (VolatileState 'V.Follower)
instance V.VolatileCandidateState (VolatileState 'V.Candidate)
instance V.VolatileLeaderState (VolatileState 'V.Leader)

instance V.HasCommitIndex (VolatileState r) Index where
    commitIndex = lens
        (\case
            Follower a _ -> a
            Candidate a _ _ -> a
            Leader a _ -> a)
        (\s i -> case s of
            Follower _ a -> Follower i a
            Candidate _ a b -> Candidate i a b
            Leader _ a -> Leader i a)

instance V.HasLastApplied (VolatileState r) Index where
    lastApplied = lens
        (\case
            Follower _ a -> a
            Candidate _ a _ -> a
            Leader _ a -> a)
        (\s i -> case s of
            Follower a _ -> Follower a i
            Candidate a _ b -> Candidate a i b
            Leader a _ -> Leader a i)

instance V.HasVotesGranted (VolatileState 'V.Candidate) (Set Node) where
    votesGranted = lens
        (\case
            Candidate _ _ a -> a)
        (\s i -> case s of
            Candidate a b _ -> Candidate a b i)

instance Default (VolatileState 'V.Candidate) where
    def = Candidate def def def

instance Arbitrary (VolatileState 'V.Follower) where
    arbitrary = Follower <$> arbitrary <*> arbitrary

instance Arbitrary (VolatileState 'V.Candidate) where
    arbitrary = Candidate <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (VolatileState 'V.Leader) where
    arbitrary = Leader <$> arbitrary <*> arbitrary


newtype TimersT m a = TimersT { unTimersT :: WriterT [TimerEvent] m a }
    deriving (Functor, Applicative, Monad, MonadLogger)

data TimerEvent = StartElectionTimer
                | CancelElectionTimer
                | StartHeartbeatTimer
    deriving (Show, Eq)

instance Monad m => MonadTimers (TimersT m) where
    startElectionTimer = TimersT $ tell [StartElectionTimer]
    cancelElectionTimer = TimersT $ tell [CancelElectionTimer]
    startHeartbeatTimer = TimersT $ tell [StartHeartbeatTimer]

runTimersT :: TimersT m a -> m (a, [TimerEvent])
runTimersT = runWriterT . unTimersT
