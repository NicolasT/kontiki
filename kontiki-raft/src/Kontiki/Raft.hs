{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Kontiki.Raft (
      S.Some
    , initialState
    , initializePersistentState
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    , role
    , Role(..)
    ) where

import GHC.Stack (HasCallStack)

import Control.Monad.Trans.State (StateT, runStateT)

import Control.Monad.State.Class (MonadState(get, put))

import Control.Lens ((&), (.~))

import Data.Default.Class (Default(def))

import Control.Monad.Indexed ((>>>=), ireturn)
import Control.Monad.Indexed.State (IxStateT(runIxStateT), iget, iput, imodify)

import Control.Monad.Logger (MonadLogger, logDebug, logDebugSH, logInfo)
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
import Kontiki.Raft.Classes.State.Volatile (VolatileState, Role(Follower, Candidate, Leader))
import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.Timers (MonadTimers)
import Kontiki.Raft.Classes.Types (Index(index0), Term (term0))

import qualified Kontiki.Raft.Internal.AllServers as A
import qualified Kontiki.Raft.Internal.Candidate as C
import qualified Kontiki.Raft.Internal.Follower as F
import qualified Kontiki.Raft.Internal.Leader as L
import qualified Kontiki.Raft.Internal.State as S

initialState :: ( VolatileState volatileState
                , Index (V.Index volatileState)
                )
             => S.Some volatileState
initialState = S.Some (V.initialize initialCommitIndex initialLastApplied)
  where
    initialCommitIndex = index0
    initialLastApplied = index0

initializePersistentState :: ( MonadPersistentState m
                             , MonadLogger m
                             , Term (P.Term m)
                             )
                          => m ()
initializePersistentState = do
    $(logInfo) "Initializing persistent state"
    setCurrentTerm term0
    setVotedFor Nothing

onRequestVoteRequest :: forall m volatileState req resp node term.
                        ( MonadState (S.Some volatileState) m
                        , MonadPersistentState m
                        , MonadTimers m
                        , MonadLogger m
                        , VolatileState volatileState
                        , RequestVoteRequest req
                        , RequestVoteResponse resp
                        , Default resp
                        , P.Node m ~ node
                        , RVReq.Node req ~ node
                        , Eq node
                        , P.Term m ~ term
                        , RPC.Term resp ~ term
                        , RPC.Term req ~ term
                        , Ord term
                        , Show req
                        )
                     => req
                     -> m resp
onRequestVoteRequest req = do
    $(logDebugSH) ("onRequestVoteRequest" :: Text, req)
    checkTerm req
    dispatch (runIxStateT (wrap $ F.onRequestVoteRequest req))
             (runIxStateT (wrap $ C.onRequestVoteRequest req))
             (runIxStateT (L.onRequestVoteRequest req))

onRequestVoteResponse :: ( MonadState (S.Some volatileState) m
                         , MonadLogger m
                         , MonadPersistentState m
                         , MonadTimers m
                         , VolatileState volatileState
                         , V.Node volatileState ~ node
                         , Ord node
                         , RPC.Term resp ~ P.Term m
                         , Show node
                         , RequestVoteResponse resp
                         , Show resp
                         , Ord (P.Term m)
                         )
                      => node
                      -> resp
                      -> m ()
onRequestVoteResponse sender resp = do
    $(logDebugSH) ("onRequestVoteResponse" :: Text, sender, resp)
    checkTerm resp
    dispatch
        (runIxStateT (wrap $ F.onRequestVoteResponse sender resp))
        (runIxStateT $ C.onRequestVoteResponse sender resp)
        (runIxStateT $ L.onRequestVoteResponse sender resp)


onElectionTimeout :: ( RVReq.Node (RPC.RequestVoteRequest m) ~ Config.Node config
                     , MonadState (S.Some volatileState) m
                     , MonadReader config m
                     , V.Node volatileState ~ RVReq.Node (RPC.RequestVoteRequest m)
                     , P.Term m ~ RPC.Term (RPC.RequestVoteRequest m)
                     , P.Index m ~ RVReq.Index (RPC.RequestVoteRequest m)
                     , MonadLogger m
                     , VolatileState volatileState
                     , Default (RPC.RequestVoteRequest m)
                     , RequestVoteRequest (RPC.RequestVoteRequest m)
                     , Ord (Config.Node config)
                     , Term (RPC.Term (RPC.RequestVoteRequest m))
                     , MonadRPC m
                     , MonadTimers m
                     , MonadPersistentState m
                     , Config config
                     , Index (RVReq.Index (RPC.RequestVoteRequest m))
                     )
                  => m ()
onElectionTimeout = do
    $(logDebug) "onElectionTimeout"
    dispatch
        (runIxStateT (F.onElectionTimeout >>>= \() -> S.wrap))
        (runIxStateT (wrap $ C.onElectionTimeout))
        (runIxStateT L.onElectionTimeout)

onHeartbeatTimeout :: ( MonadLogger m
                      , VolatileState volatileState
                      , MonadState (S.Some volatileState) m
                      )
                   => m ()
onHeartbeatTimeout = do
    $(logDebug) "onHeartbeatTimeout"
    dispatch
        (runIxStateT (wrap F.onHeartbeatTimeout))
        (runIxStateT (wrap C.onHeartbeatTimeout))
        (runIxStateT L.onHeartbeatTimeout)



-- TODO
onAppendEntriesRequest, onAppendEntriesResponse :: a
onAppendEntriesRequest = undefined
onAppendEntriesResponse = undefined


{-
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

-}

role :: VolatileState volatileState => S.Some volatileState -> Role
role s = case s of
    S.Some s' -> V.dispatch (const Follower) (const Candidate) (const Leader) s'

checkTerm :: ( MonadPersistentState m
             , MonadTimers m
             , MonadState (S.Some volatileState) m
             , MonadLogger m
             , RPC.Term msg ~ P.Term m
             , Ord (P.Term m)
             , RPC.HasTerm msg
             , VolatileState volatileState
             )
          => msg
          -> m ()
checkTerm req = dispatch (runIxStateT $ A.checkTerm req)
                         (runIxStateT $ A.checkTerm req)
                         (runIxStateT $ A.checkTerm req)

wrap :: Monad m => IxStateT m (volatileState r) (volatileState r) a -> IxStateT m (volatileState r) (S.Some volatileState) a
wrap act = act >>>= \r -> S.wrap >>>= \() -> ireturn r

dispatch :: ( MonadState (S.Some volatileState) m
            , VolatileState volatileState
            )
         => (volatileState 'V.Follower -> m (a, S.Some volatileState))
         -> (volatileState 'V.Candidate -> m (a, S.Some volatileState))
         -> (volatileState 'V.Leader -> m (a, S.Some volatileState))
         -> m a
dispatch f c l = get >>= \case
    S.Some s -> do
        (r, s') <- V.dispatch f c l s
        put s'
        return r
