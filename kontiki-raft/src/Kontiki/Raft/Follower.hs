{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Kontiki.Raft.Follower (
      convertToFollower
    , onRequestVoteRequest
    ) where

import Prelude hiding ((>>=), (>>), return)

import Control.Monad (when)
import Data.Maybe (isNothing)

import Control.Monad.State.Class (MonadState, modify)

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.))

import Data.Default.Class (Default(def))

import Kontiki.Raft.Classes.RPC (HasTerm(term), MonadRPC(sendRequestVoteResponse))
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (voteGranted)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(getCurrentTerm, getVotedFor, setVotedFor))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState(commitIndex, lastApplied))

import Kontiki.Raft.State (Role(Follower), State(F), SomeState(SomeState))

convertToFollower :: ( MonadState (SomeState volatileState volatileLeaderState) m
                     , VolatileState volatileState
                     , Default volatileState
                     )
                  => m ()
convertToFollower = modify $ \case
    SomeState s -> SomeState $ F $ def & commitIndex .~ s ^. commitIndex
                                       & lastApplied .~ s ^. lastApplied


onRequestVoteRequest :: forall m node req resp term vs vls.
                        ( IxMonadState m
                        , Applicative (m (State vs vls 'Follower) (State vs vls 'Follower))
                        , MonadPersistentState (m (State vs vls 'Follower) (State vs vls 'Follower))
                        , P.Node (m (State vs vls 'Follower) (State vs vls 'Follower)) ~ node
                        , P.Term (m (State vs vls 'Follower) (State vs vls 'Follower)) ~ term
                        , HasTerm req
                        , RPC.Term req ~ term
                        , Ord term
                        , MonadRPC (m (State vs vls 'Follower) (State vs vls 'Follower))
                        , RPC.RequestVoteResponse (m (State vs vls 'Follower) (State vs vls 'Follower)) ~ resp
                        , RPC.Node (m (State vs vls 'Follower) (State vs vls 'Follower)) ~ node
                        , RVResp.RequestVoteResponse resp
                        , RPC.Term resp ~ term
                        , Default resp
                        , Eq node
                        )
                     => node
                     -> req
                     -> m (State vs vls 'Follower) (SomeState vs vls) ()
onRequestVoteRequest node req = do
    currentTerm <- getCurrentTerm
    if req ^. term < currentTerm
    then do
        -- Reply false if term < currentTerm (§5.1)
        let resp :: resp = def & term .~ currentTerm
                               & voteGranted .~ False
        sendRequestVoteResponse node resp :: m (State vs vls 'Follower) (State vs vls 'Follower) ()
    else do
        vf <- getVotedFor
        let maybeGrantVote = maybe True (== node) vf
        if maybeGrantVote
        then do
            candidateLogUpToDate <- isCandidateLogUpToDate req
            when (isNothing vf && candidateLogUpToDate) $
                setVotedFor (Just node) :: m (State vs vls 'Follower) (State vs vls 'Follower) ()

            let resp :: resp = def & term .~ currentTerm
                                   & voteGranted .~ candidateLogUpToDate
            sendRequestVoteResponse node resp
        else do
            let resp = def & term .~ currentTerm
                           & voteGranted .~ False
            sendRequestVoteResponse node resp
    imodify SomeState
  where
    isCandidateLogUpToDate _ = return True
    m >>= n = Ix.ibind n m
    m >> n = m >>= \_ -> n
    return a = Ix.ireturn a
    ifThenElse p a b = case p of
        True -> a
        False -> b
