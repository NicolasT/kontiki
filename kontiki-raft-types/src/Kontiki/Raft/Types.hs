{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Types (
      Index
    , Term
    , Node
    , VolatileState
    , PersistentState(runPersistentState)
    , initialPersistentState
    , RPC (runRPC)
    , RequestVoteRequest
    ) where

import Data.Word (Word64)

import GHC.Generics (Generic)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State (StateT, gets, modify)

import Data.Default.Class (Default(def))

import Control.Lens (lens)

import Control.Monad.Logger (MonadLogger, logDebugSH)

import qualified Kontiki.Raft.Classes.RPC as RPC
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import qualified Kontiki.Raft.Classes.State.Persistent as P
import qualified Kontiki.Raft.Classes.State.Volatile as V
import qualified Kontiki.Raft.Classes.Types as T

newtype Index = Index Word64
    deriving (Show, Eq, Generic)

instance Default Index

instance T.Index Index where
    index0 = Index 0
    succIndex (Index i) = Index (succ i)


newtype Term = Term Word64
    deriving (Show, Eq, Ord, Generic)

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


newtype PersistentState e m a = PersistentState { runPersistentState :: StateT (PersistentState' e) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadTrans)

data PersistentState' e = PersistentState' { persistentState'CurrentTerm :: Term
                                           , persistentState'VotedFor :: Maybe Node
                                           }
    deriving (Show, Eq, Generic)

initialPersistentState :: PersistentState' e
initialPersistentState = PersistentState' { persistentState'CurrentTerm = T.term0
                                          , persistentState'VotedFor = Nothing
                                          }

instance MonadLogger m => P.MonadPersistentState (PersistentState e m) where
    type Term (PersistentState e m) = Term
    type Node (PersistentState e m) = Node
    type Entry (PersistentState e m) = e
    type Index (PersistentState e m) = Index

    getCurrentTerm = PersistentState $ do
        t <- gets persistentState'CurrentTerm
        $(logDebugSH) ("Get current term" :: String, t)
        return t
    setCurrentTerm t = PersistentState $ do
        $(logDebugSH) ("Set current term" :: String, t)
        modify (\s -> s { persistentState'CurrentTerm = t })
    getVotedFor = PersistentState $ do
        v <- gets persistentState'VotedFor
        $(logDebugSH) ("Get voted for" :: String, v)
        return v
    setVotedFor v = PersistentState $ do
        $(logDebugSH) ("Set voted for" :: String, v)
        modify (\s -> s { persistentState'VotedFor = v })
    getLogEntry _i = error "Not implemented"
    setLogEntry _i _t _e = error "Not implemented"


instance (Monad m, RPC.MonadRPC m) => RPC.MonadRPC (PersistentState e m) where
    type Node (PersistentState e m) = RPC.Node m
    type RequestVoteRequest (PersistentState e m) = RPC.RequestVoteRequest m
    type RequestVoteResponse (PersistentState e m) = RPC.RequestVoteResponse m
    type AppendEntriesRequest (PersistentState e m) = RPC.AppendEntriesRequest m
    type AppendEntriesResponse (PersistentState e m) = RPC.AppendEntriesResponse m

    broadcastRequestVoteRequest = lift . RPC.broadcastRequestVoteRequest
    sendRequestVoteResponse n m = lift $ RPC.sendRequestVoteResponse n m
    sendAppendEntriesRequest n m = lift $ RPC.sendAppendEntriesRequest n m
    sendAppendEntriesResponse n m = lift $ RPC.sendAppendEntriesResponse n m


newtype RPC m a = RPC { runRPC :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance RPC.MonadRPC (RPC m) where
    type Node (RPC m) = Node
    type RequestVoteRequest (RPC m) = RequestVoteRequest
    type RequestVoteResponse (RPC m) = RequestVoteResponse
    type AppendEntriesRequest (RPC m) = ()
    type AppendEntriesResponse (RPC m) = ()

    broadcastRequestVoteRequest _ = error "broadcastRequestVoteRequest: Not implemented"
    sendRequestVoteResponse _ _ = error "sendRequestVoteResponse: Not implemented"
    sendAppendEntriesRequest _ _ = error "sendAppendEntriesRequest: Not implemented"
    sendAppendEntriesResponse _ _ = error "sendAppendEntriesResponse: Not implemented"


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


data RequestVoteResponse = RequestVoteResponse { requestVoteResponseTerm :: Term
                                               , requestVoteResponseVoteGranted :: Bool
                                               }
    deriving (Show, Eq, Generic)

instance Default RequestVoteResponse where
    def = RequestVoteResponse { requestVoteResponseTerm = def
                              , requestVoteResponseVoteGranted = False
                              }

instance RPC.HasTerm RequestVoteResponse where
    type Term RequestVoteResponse = Term

    term = lens requestVoteResponseTerm (\r t -> r { requestVoteResponseTerm = t })

instance RVResp.RequestVoteResponse RequestVoteResponse where
    voteGranted = lens requestVoteResponseVoteGranted (\r g -> r { requestVoteResponseVoteGranted = g })
