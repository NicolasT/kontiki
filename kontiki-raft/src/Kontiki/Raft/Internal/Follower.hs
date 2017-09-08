{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Kontiki.Raft.Internal.Follower (
      convertToFollower
    , onRequestVoteRequest
    , onRequestVoteResponse
    , onAppendEntriesRequest
    , onAppendEntriesResponse
    , onElectionTimeout
    , onHeartbeatTimeout
    ) where

import Prelude hiding ((>>=), (>>), return)
import qualified Prelude

import GHC.Stack (HasCallStack)

import Control.Monad (when)
import Data.Maybe (isNothing)

import Control.Monad.State.Class (MonadState, modify)

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.))

import Data.Default.Class (Default(def))

import Kontiki.Raft.Classes.Config (MonadConfig)
import qualified Kontiki.Raft.Classes.Config as Config
import Kontiki.Raft.Classes.RPC (HasTerm(term), MonadRPC)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (RequestVoteRequest, candidateId)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RequestVoteRequest
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (voteGranted)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(getCurrentTerm, getVotedFor, setVotedFor))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState(commitIndex, lastApplied))
import Kontiki.Raft.Classes.Timers (MonadTimers(startElectionTimer))
import qualified Kontiki.Raft.Classes.Types as T

import Kontiki.Raft.Internal.Candidate (convertToCandidate)
import Kontiki.Raft.Internal.State (Role(Candidate, Follower), State(F), SomeState(SomeState))

convertToFollower :: ( Monad m
                     , MonadState (SomeState volatileState volatileLeaderState) m
                     , VolatileState volatileState
                     , Default volatileState
                     , MonadTimers m
                     )
                  => m ()
convertToFollower = do
    modify $ \case
        SomeState s -> SomeState $ F $ def & commitIndex .~ s ^. commitIndex
                                           & lastApplied .~ s ^. lastApplied
    startElectionTimer
  where
    a >> b = (Prelude.>>) a b
    return a = Prelude.return a

onRequestVoteRequest :: forall m node req resp term vs vls.
                        ( IxMonadState m
                        , Applicative (m (State vs vls 'Follower) (State vs vls 'Follower))
                        , MonadPersistentState (m (State vs vls 'Follower) (State vs vls 'Follower))
                        , P.Node (m (State vs vls 'Follower) (State vs vls 'Follower)) ~ node
                        , P.Term (m (State vs vls 'Follower) (State vs vls 'Follower)) ~ term
                        , HasTerm req
                        , RequestVoteRequest req
                        , RPC.Term req ~ term
                        , RequestVoteRequest.Node req ~ node
                        , Ord term
                        , RVResp.RequestVoteResponse resp
                        , RPC.Term resp ~ term
                        , Default resp
                        , Eq node
                        )
                     => req
                     -> m (State vs vls 'Follower) (SomeState vs vls) resp
onRequestVoteRequest req = do
    currentTerm <- getCurrentTerm
    resp :: resp <- if req ^. term < currentTerm
    then do
        -- Reply false if term < currentTerm (§5.1)
        return $ def & term .~ currentTerm
                     & voteGranted .~ False
    else do
        vf <- getVotedFor
        let node = req ^. candidateId
        let maybeGrantVote = maybe True (== node) vf
        if maybeGrantVote
        then do
            candidateLogUpToDate <- isCandidateLogUpToDate req
            when (isNothing vf && candidateLogUpToDate) $
                setVotedFor (Just node) :: m (State vs vls 'Follower) (State vs vls 'Follower) ()

            return $ def & term .~ currentTerm
                         & voteGranted .~ candidateLogUpToDate
        else do
            return $ def & term .~ currentTerm
                         & voteGranted .~ False
    imodify SomeState
    return resp
  where
    isCandidateLogUpToDate _ = return True
    m >>= n = Ix.ibind n m
    m >> n = m >>= \_ -> n
    return a = Ix.ireturn a
    ifThenElse p a b = case p of
        True -> a
        False -> b

onRequestVoteResponse :: HasCallStack
                      => a
                      -> b
onRequestVoteResponse _ = error "Not implemented"

onAppendEntriesRequest :: HasCallStack
                       => a
                       -> b
onAppendEntriesRequest _ = error "Not implemented"

onAppendEntriesResponse :: HasCallStack
                        => a
                        -> b
onAppendEntriesResponse _ = error "Not implemented"

onElectionTimeout :: forall m vs vls requestVoteRequest index term node.
                     ( IxMonadState m
                     , Monad (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadConfig (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadRPC (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadTimers (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , MonadPersistentState (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , VolatileState vs
                     , Default vs
                     , requestVoteRequest ~ RPC.RequestVoteRequest (m (State vs vls 'Candidate) (State vs vls 'Candidate))
                     , RequestVoteRequest requestVoteRequest
                     , RequestVoteRequest.Index requestVoteRequest ~ index
                     , P.Index (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ index
                     , RPC.Term requestVoteRequest ~ term
                     , P.Term (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ term
                     , RequestVoteRequest.Node requestVoteRequest ~ node
                     , Config.Node (m (State vs vls 'Candidate) (State vs vls 'Candidate)) ~ node
                     , T.Term term
                     , T.Index index
                     , Default requestVoteRequest
                     , HasCallStack
                     )
                  => m (State vs vls 'Follower) (SomeState vs vls) ()
onElectionTimeout = convertToCandidate >>>= imodify SomeState

onHeartbeatTimeout :: HasCallStack => a
onHeartbeatTimeout = error "Not implemented"


(>>>=) :: Ix.IxMonad m => m i j a -> m j k b -> m i k b
m >>>= n = Ix.ibind (const n) m
{-# INLINE (>>>=) #-}
