{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Kontiki.Raft (
      initialState
    , initializePersistentState
    , onRequestVoteRequest
    ) where

import Control.Monad.State.Class (MonadState(get, put))

import Control.Lens ((&), (.~))

import Data.Default.Class (Default(def))

import Control.Monad.Indexed.State (IxStateT(runIxStateT))

import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest)
--import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse)
--import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
--import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(setCurrentTerm, setVotedFor))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState(commitIndex, lastApplied))
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Types (Index(index0), Term (term0))

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
                             , Term (P.Term m)
                             )
                          => m ()
initializePersistentState = do
    setCurrentTerm term0
    setVotedFor Nothing

onRequestVoteRequest :: ( MonadState (S.SomeState volatileState volatileLeaderState) m
                        , RequestVoteRequest req
                        )
                     => req
                     -> m ()
onRequestVoteRequest = dispatchHandler
                        F.onRequestVoteRequest
                        C.onRequestVoteRequest
                        L.onRequestVoteRequest

dispatchHandler :: MonadState (S.SomeState v vl) m
                => (arg -> IxStateT m (S.State v vl 'S.Follower) (S.SomeState v vl) a)
                -> (arg -> IxStateT m (S.State v vl 'S.Candidate) (S.SomeState v vl) a)
                -> (arg -> IxStateT m (S.State v vl 'S.Leader) (S.SomeState v vl) a)
                -> arg
                -> m a
dispatchHandler f c l arg = do
    (r, s') <- get >>= \case
        S.SomeState s -> case s of
            S.F _ -> runIxStateT (f arg) s
            S.C _ -> runIxStateT (c arg) s
            S.L _ _ -> runIxStateT (l arg) s
    put s'
    return r
