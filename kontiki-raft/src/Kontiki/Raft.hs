{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Monad.State.Class (MonadState, get, put)

import Data.Default.Class (Default)

import Control.Monad.Indexed ((>>>=), ireturn)
import Control.Monad.Indexed.State (IxStateT(runIxStateT))

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

onRequestVoteRequest :: ( MonadState (S.Some volatileState) m
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
             (runIxStateT (wrap $ L.onRequestVoteRequest req))

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
        (runIxStateT (wrap $ L.onRequestVoteResponse sender resp))


onAppendEntriesRequest :: ( MonadState (S.Some volatileState) m
                          , MonadLogger m
                          , MonadPersistentState m
                          , MonadTimers m
                          , VolatileState volatileState
                          , RPC.Term req ~ P.Term m
                          , Show req
                          , AppendEntriesRequest req
                          , Ord (P.Term m)
                          , RPC.Term resp ~ P.Term m
                          , AppendEntriesResponse resp
                          , Default resp
                          )
                       => req
                       -> m resp
onAppendEntriesRequest req = do
    $(logDebugSH) ("onAppendEntriesRequest" :: Text, req)
    checkTerm req
    dispatch (runIxStateT (F.onAppendEntriesRequest req))
             (runIxStateT (C.onAppendEntriesRequest req >>>= \r -> S.wrap >>> ireturn r))
             (runIxStateT (wrap $ L.onAppendEntriesRequest req))
  where
    a >>> b = a >>>= \() -> b

onAppendEntriesResponse :: ( MonadState (S.Some volatileState) m
                           , MonadLogger m
                           , MonadPersistentState m
                           , MonadTimers m
                           , VolatileState volatileState
                           , RPC.Term resp ~ P.Term m
                           , Show resp
                           , Show node
                           , AppendEntriesResponse resp
                           , Ord (P.Term m)
                           )
                        => node
                        -> resp
                        -> m ()
onAppendEntriesResponse sender resp = do
    $(logDebugSH) ("onAppendEntriesResponse" :: Text, sender, resp)
    checkTerm resp
    dispatch (runIxStateT (wrap $ F.onAppendEntriesResponse sender resp))
             (runIxStateT (wrap $ C.onAppendEntriesResponse sender resp))
             (runIxStateT (L.onAppendEntriesResponse sender resp))


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
        (runIxStateT F.onElectionTimeout)
        (runIxStateT C.onElectionTimeout)
        (runIxStateT (wrap L.onElectionTimeout))

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
