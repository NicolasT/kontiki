{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kontiki.Raft (
      initialState
    , S.SomeState
    , S.Role(..)
    , S.role
    , S.volatileState
    , initializePersistentState
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import GHC.Stack (HasCallStack)

import Control.Monad.State.Class (MonadState(get, put))

import Control.Lens ((&), (.~))

import Data.Default.Class (Default(def))

import Control.Monad.Indexed.State (IxStateT(runIxStateT))

import Control.Monad.Logger (MonadLogger, logDebugSH, logInfo)
import Control.Monad.Reader (MonadReader)

import Data.Text (Text)

import Kontiki.Raft.Classes.Config (Config)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (MonadRPC)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse)
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(setCurrentTerm, setVotedFor))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState(commitIndex, lastApplied))
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Timers (MonadTimers)
import Kontiki.Raft.Classes.Types (Index(index0), Term (term0))

import qualified Kontiki.Raft.Internal.AllServers as A
import qualified Kontiki.Raft.Internal.Candidate as C
import qualified Kontiki.Raft.Internal.Follower as F
import qualified Kontiki.Raft.Internal.Leader as L
import qualified Kontiki.Raft.Internal.State as S

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
                        , RVReq.Node req ~ node
                        , P.Term m ~ term
                        , RPC.Term req ~ term
                        , RPC.Term resp ~ term
                        , Ord term
                        , VolatileState volatileState
                        , Default volatileState
                        , MonadLogger m
                        , P.Node m ~ node
                        , RequestVoteResponse resp
                        , Default resp
                        , Eq node
                        , MonadTimers m
                        , Show req
                        , HasCallStack
                        )
                     => req
                     -> m resp
onRequestVoteRequest req = do
    $(logDebugSH) ("onRequestVoteRequest" :: Text, req)
    A.checkTerm req
    dispatchHandler
        (F.onRequestVoteRequest req)
        (C.onRequestVoteRequest req)
        (L.onRequestVoteRequest req)

onRequestVoteResponse :: ( RequestVoteResponse resp
                         , MonadPersistentState m
                         , MonadState (S.SomeState volatileState volatileLeaderState) m
                         , MonadLogger m
                         , MonadTimers m
                         , VolatileState volatileState
                         , Default volatileState
                         , RPC.Term resp ~ term
                         , P.Term m ~ term
                         , Ord term
                         , Show resp
                         , Show node
                         , HasCallStack
                         )
                      => node
                      -> resp
                      -> m ()
onRequestVoteResponse sender resp = do
    $(logDebugSH) ("onRequestVoteResponse" :: Text, sender, resp)
    A.checkTerm resp
    dispatchHandler
        (F.onRequestVoteResponse sender resp)
        (C.onRequestVoteResponse sender resp)
        (L.onRequestVoteResponse sender resp)

onAppendEntriesRequest :: ( AppendEntriesRequest req
                          , MonadPersistentState m
                          , MonadState (S.SomeState volatileState volatileLeaderState) m
                          , MonadLogger m
                          , MonadTimers m
                          , VolatileState volatileState
                          , Default volatileState
                          , RPC.Term req ~ term
                          , P.Term m ~ term
                          , Ord term
                          , Show req
                          , RPC.Term resp ~ term
                          , AppendEntriesResponse resp
                          , Default resp
                          , HasCallStack
                          )
                       => req
                       -> m resp
onAppendEntriesRequest req = do
    $(logDebugSH) ("onAppendEntriesRequest" :: Text, req)
    A.checkTerm req
    dispatchHandler
        (F.onAppendEntriesRequest req)
        (C.onAppendEntriesRequest req)
        (L.onAppendEntriesRequest req)

onAppendEntriesResponse :: ( AppendEntriesResponse resp
                           , MonadPersistentState m
                           , MonadState (S.SomeState volatileState volatileLeaderState) m
                           , MonadLogger m
                           , MonadTimers m
                           , VolatileState volatileState
                           , Default volatileState
                           , RPC.Term resp ~ term
                           , P.Term m ~ term
                           , Ord term
                           , Show resp
                           , Show node
                           , HasCallStack
                           )
                        => node
                        -> resp
                        -> m ()
onAppendEntriesResponse sender resp = do
    $(logDebugSH) ("onAppendEntriesResponse" :: Text, sender, resp)
    A.checkTerm resp
    dispatchHandler
        (F.onAppendEntriesResponse sender resp)
        (C.onAppendEntriesResponse sender resp)
        (L.onAppendEntriesResponse sender resp)

onElectionTimeout :: ( MonadState (S.SomeState v vl) m
                     , MonadReader config m
                     , MonadRPC m
                     , MonadTimers m
                     , MonadPersistentState m
                     , MonadLogger m
                     , VolatileState v
                     , Config config
                     , Term term
                     , Index index
                     , Default v
                     , Default (RPC.RequestVoteRequest m)
                     , RequestVoteRequest (RPC.RequestVoteRequest m)
                     , RPC.Term (RPC.RequestVoteRequest m) ~ term
                     , P.Term m ~ term
                     , RVReq.Index (RPC.RequestVoteRequest m) ~ index
                     , P.Index m ~ index
                     , RVReq.Node (RPC.RequestVoteRequest m) ~ node
                     , Config.Node config ~ node
                     , Show term
                     , HasCallStack
                     )
                  => m ()
onElectionTimeout = dispatchHandler F.onElectionTimeout C.onElectionTimeout L.onElectionTimeout

onHeartbeatTimeout :: ( MonadState (S.SomeState v vl) m
                      , MonadLogger m
                      , HasCallStack
                      )
                   => m ()
onHeartbeatTimeout = dispatchHandler F.onHeartbeatTimeout C.onHeartbeatTimeout L.onHeartbeatTimeout


dispatchHandler :: ( MonadState (S.SomeState v vl) m
                   , HasCallStack
                   )
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
