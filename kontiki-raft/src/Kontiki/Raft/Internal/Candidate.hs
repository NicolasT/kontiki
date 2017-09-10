{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Kontiki.Raft.Internal.Candidate (
      convertToCandidate
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import Prelude hiding ((>>), (>>=), return)
import qualified Prelude

import Data.String (fromString)

import GHC.Stack (HasCallStack)

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.), view)

import Data.Default.Class (Default, def)

import Control.Monad.Logger (MonadLogger, logDebug, logDebugSH, logInfo)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)

import Data.Text (Text)

import Kontiki.Raft.Classes.Config (Config, localNode)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (MonadRPC, broadcastRequestVoteRequest, term)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest, candidateId, lastLogIndex, lastLogTerm)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RequestVoteRequest
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (RequestVoteResponse, voteGranted)
import Kontiki.Raft.Classes.RPC.AppendEntriesRequest (AppendEntriesRequest)
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse, success)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState, getCurrentTerm, lastLogEntry, setCurrentTerm)
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState, commitIndex, lastApplied)
import Kontiki.Raft.Classes.Timers (MonadTimers, cancelElectionTimer, startElectionTimer)
import Kontiki.Raft.Classes.Types (succTerm)
import qualified Kontiki.Raft.Classes.Types as T

import {-# SOURCE #-} qualified  Kontiki.Raft.Internal.Follower as Follower
import Kontiki.Raft.Internal.State (Role(Candidate, Follower), SomeState(SomeState), State(F, C))

convertToCandidate :: forall m vs vls requestVoteRequest index term node config.
                      ( IxMonadState m
                      , Monad (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadReader config (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadRPC (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadTimers (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadPersistentState (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , MonadLogger (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , Config config
                      , VolatileState vs
                      , Default vs
                      , requestVoteRequest ~ RPC.RequestVoteRequest (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , RequestVoteRequest requestVoteRequest
                      , RequestVoteRequest.Index requestVoteRequest ~ index
                      , P.Index (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ index
                      , RPC.Term requestVoteRequest ~ term
                      , P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ term
                      , RequestVoteRequest.Node requestVoteRequest ~ node
                      , Config.Node config ~ node
                      , T.Term term
                      , T.Index index
                      , Default requestVoteRequest
                      , HasCallStack
                      , Show term
                      )
                   => m (State vs vls 'Follower) (State vs vls 'Candidate) ()
convertToCandidate = changeState >>> startElection
  where
    changeState :: m (State vs vls 'Follower) (State vs vls 'Candidate) ()
    changeState = imodify $ \case
        F s -> C $ def & commitIndex .~ s ^. commitIndex
                       & lastApplied .~ s ^. lastApplied

startElection :: forall m req config.
                 ( Monad m
                 , MonadPersistentState m
                 , MonadTimers m
                 , MonadRPC m
                 , MonadReader config m
                 , MonadLogger m
                 , Config config
                 , T.Index (P.Index m)
                 , T.Term (P.Term m)
                 , req ~ RPC.RequestVoteRequest m
                 , RequestVoteRequest req
                 , Default req
                 , RequestVoteRequest.Index req ~ P.Index m
                 , RPC.Term req ~ P.Term m
                 , Config.Node config ~ RequestVoteRequest.Node req
                 , Show (P.Term m)
                 )
              => m ()
startElection = do
    $(logInfo) "Starting election"
    incrementCurrentTerm
    voteForSelf
    resetElectionTimer
    sendRequestVoteToOtherServers
  where
    incrementCurrentTerm = do
        currentTerm <- getCurrentTerm
        let newTerm = succTerm currentTerm
        $(logDebugSH) ("New term" :: Text, newTerm)
        setCurrentTerm newTerm
    voteForSelf = do
        voteFor =<< view localNode
    resetElectionTimer = do
        cancelElectionTimer
        startElectionTimer
    sendRequestVoteToOtherServers = do
        me <- view localNode
        currentTerm <- getCurrentTerm
        (idx, term') <- maybe (T.index0, T.term0) (\(i, t, _) -> (i, t)) <$> lastLogEntry
        let req = def & term .~ currentTerm
                      & candidateId .~ me
                      & lastLogIndex .~ idx
                      & lastLogTerm .~ term'
        broadcastRequestVoteRequest req

    (>>) = (Prelude.>>)
    (>>=) = (Prelude.>>=)
    return a = Prelude.return a

voteFor :: Monad m => node -> m ()
voteFor _node = Prelude.return ()-- error "Not implemented"

onRequestVoteRequest :: forall a m vs vls resp.
                        ( IxMonadState m
                        , MonadLogger (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                        , MonadPersistentState (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                        , Default resp
                        , RequestVoteResponse resp
                        , RPC.Term resp ~ P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                        , HasCallStack
                        )
                     => a
                     -> m (State vs vls 'Candidate) (SomeState vs vls) resp
onRequestVoteRequest _ = do
    $(logDebug) "Received RequestVote request in Candidate mode, ignoring" :: m (State vs vls 'Candidate) (State vs vls 'Candidate) ()
    currentTerm <- getCurrentTerm :: m (State vs vls 'Candidate) (State vs vls 'Candidate) (RPC.Term resp)
    let msg = def & term .~ currentTerm
                  & voteGranted .~ False
    imodify SomeState
    return msg
  where
    (>>) = (>>>)
    (>>=) = flip Ix.ibind
    return a = Ix.ireturn a

onRequestVoteResponse :: ( IxMonadState m
                         , HasCallStack
                )
                      => a
                      -> m (State vs vls 'Candidate) (SomeState vs vls) ()
onRequestVoteResponse _ = error "Not implemented"

onAppendEntriesRequest :: forall m req term vs vls resp.
                          ( IxMonadState m
                          , MonadPersistentState (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                          , RPC.Term req ~ term
                          , RPC.Term resp ~ term
                          , P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ term
                          , AppendEntriesRequest req
                          , AppendEntriesResponse resp
                          , Default resp
                          , Ord term
                          , VolatileState vs
                          , Default vs
                          , MonadState (SomeState vs vls) (m (SomeState vs vls) (SomeState vs vls))
                          , MonadTimers (m (SomeState vs vls) (SomeState vs vls))
                          , HasCallStack
                          )
                       => req
                       -> m (State vs vls 'Candidate) (SomeState vs vls) resp
onAppendEntriesRequest req = do
    currentTerm <- getCurrentTerm :: m (State vs vls 'Candidate) (State vs vls 'Candidate) term
    if (currentTerm <= req ^. term)
        then do
            imodify SomeState :: m (State vs vls 'Candidate) (SomeState vs vls) ()
            Follower.convertToFollower :: m (SomeState vs vls) (SomeState vs vls) ()
            imodify (\(SomeState s) -> case s of
                F s' -> F s'
                _ -> error "Impossible") :: m (SomeState vs vls) (State vs vls 'Follower) ()
            Follower.onAppendEntriesRequest req
        else do
            let msg = def & term .~ currentTerm
                          & success .~ False
            imodify SomeState
            return msg
  where
    (>>) = (>>>)
    (>>=) = flip Ix.ibind
    return a = Ix.ireturn a
    ifThenElse b t f = case b of
        True -> t
        False -> f


onAppendEntriesResponse :: forall a m vs vls.
                           ( IxMonadState m
                           , MonadLogger (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                           , HasCallStack
                           )
                        => a
                        -> m (State vs vls 'Candidate) (SomeState vs vls) ()
onAppendEntriesResponse _ =
    ($(logDebug) "Received AppendEntries response in Candidate mode, ignoring" :: m (State vs vls 'Candidate) (State vs vls 'Candidate) ())
        >>> imodify SomeState

onElectionTimeout :: forall m vs vls req config.
                     ( IxMonadState m
                     , Monad (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadPersistentState (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadTimers (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadRPC (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadReader config (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadLogger (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , Config config
                     , T.Index (P.Index (m (State vs vls 'Candidate) (State vs vls 'Candidate)))
                     , T.Term (P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate)))
                     , req ~ RPC.RequestVoteRequest (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , RequestVoteRequest req
                     , Default req
                     , RequestVoteRequest.Index req ~ P.Index (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , RPC.Term req ~ P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , Config.Node config ~ RequestVoteRequest.Node req
                     , Show (P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate)))
                     )
                  => m (State vs vls 'Candidate) (SomeState vs vls) ()
onElectionTimeout =
    (startElection :: m (State vs vls 'Candidate) (State vs vls 'Candidate) ())
        >>> imodify SomeState

onHeartbeatTimeout :: forall m vs vls.
                      ( IxMonadState m
                      , MonadLogger (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                      , HasCallStack)
                   => m (State vs vls 'Candidate) (SomeState vs vls) ()
onHeartbeatTimeout =
    ($(logDebug) "Heartbeat timeout in Candidate mode, ignoring" :: m (State vs vls 'Candidate) (State vs vls 'Candidate) ())
        >>> imodify SomeState

(>>>) :: Ix.IxMonad m => m i j a -> m j k b -> m i k b
m >>> n = Ix.ibind (const n) m
{-# INLINE (>>>) #-}
