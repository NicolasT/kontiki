{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kontiki.Raft (
      initialState
    , S.SomeState
    , initializePersistentState
    , onRequestVoteRequest
    ) where

import Control.Monad.State.Class (MonadState(get, put))

import Control.Lens ((&), (.~))

import Data.Default.Class (Default(def))

import Control.Monad.Indexed.State (IxStateT(runIxStateT))

import Control.Monad.Logger (MonadLogger, logInfo)

import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest)
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse)
--import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
--import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(setCurrentTerm, setVotedFor))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState(commitIndex, lastApplied))
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Types (Index(index0), Term (term0))

import qualified Kontiki.Raft.AllServers as A
import qualified Kontiki.Raft.Candidate as C
import qualified Kontiki.Raft.Follower as F
import qualified Kontiki.Raft.Leader as L
import qualified Kontiki.Raft.State as S

initialState :: ( Default volatileState
                , VolatileState volatileState
                , Index (V.Index volatileState)
                )
             => S.SomeState volatileState volatileLeaderState
initialState = S.SomeState state
  where
    state = S.F volatileState
    volatileState = (def & commitIndex .~ index0
                         & lastApplied .~ index0)

initializePersistentState :: ( Monad m
                             , MonadPersistentState m
                             , MonadLogger m
                             , Term (P.Term m)
                             )
                          => m ()
initializePersistentState = do
    $(logInfo) "Initializing persistent state"
    setCurrentTerm term0
    setVotedFor Nothing

onRequestVoteRequest :: ( MonadState (S.SomeState volatileState volatileLeaderState) m
                        , MonadPersistentState m
                        , RequestVoteRequest req
                        , P.Term m ~ term
                        , RPC.Term req ~ term
                        , Ord term
                        , VolatileState volatileState
                        , Default volatileState
                        , MonadLogger m
                        , RPC.MonadRPC m
                        , RPC.Term (RPC.RequestVoteResponse m) ~ term
                        , P.Node m ~ node
                        , RPC.Node m ~ node
                        , RequestVoteResponse (RPC.RequestVoteResponse m)
                        , Default (RPC.RequestVoteResponse m)
                        , Eq node
                        )
                     => node
                     -> req
                     -> m ()
onRequestVoteRequest node req = do
    A.checkTerm req
    dispatchHandler
        (F.onRequestVoteRequest node req)
        (C.onRequestVoteRequest req)
        (L.onRequestVoteRequest req)

dispatchHandler :: MonadState (S.SomeState v vl) m
                => IxStateT m (S.State v vl 'S.Follower) (S.SomeState v vl) a
                -> IxStateT m (S.State v vl 'S.Candidate) (S.SomeState v vl) a
                -> IxStateT m (S.State v vl 'S.Leader) (S.SomeState v vl) a
                -> m a
dispatchHandler f c l = do
    (r, s') <- get >>= \case
        S.SomeState s -> case s of
            S.F _ -> runIxStateT f s
            S.C _ -> runIxStateT c s
            S.L _ _ -> runIxStateT l s
    put s'
    return r
