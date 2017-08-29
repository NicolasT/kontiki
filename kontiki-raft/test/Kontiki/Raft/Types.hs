{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    deriving (Show, Eq, Typeable, Generic)

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

runPersistentStateT :: PersistentState -> PersistentStateT m a -> m (a, PersistentState)
runPersistentStateT s a = runStateT (unPersistentStateT a) s


data VolatileState = VolatileState { volatileStateCommitIndex :: Index
                                   , volatileStateLastApplied :: Index
                                   }
    deriving (Show, Eq, Typeable, Generic)

instance V.VolatileState VolatileState where
    type Index VolatileState = Index

    commitIndex = lens volatileStateCommitIndex (\r i -> r { volatileStateCommitIndex = i })
    lastApplied = lens volatileStateLastApplied (\r i -> r { volatileStateLastApplied = i })

instance Default VolatileState where
    def = VolatileState (Index 0) (Index 0)

instance Arbitrary VolatileState where
    arbitrary = VolatileState <$> arbitrary
                              <*> arbitrary


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
