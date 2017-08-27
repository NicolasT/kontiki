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
    ) where

import Prelude hiding ((>>=), (>>), return)

import Control.Monad (when)
import Data.Maybe (isNothing)

import Control.Monad.State.Class (MonadState, modify)

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.))

import Data.Default.Class (Default(def))

import Kontiki.Raft.Classes.RPC (HasTerm(term))
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.RequestVoteRequest (candidateId)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteRequest as RVReq
import Kontiki.Raft.Classes.RPC.RequestVoteResponse (voteGranted)
import qualified Kontiki.Raft.Classes.RPC.RequestVoteResponse as RVResp
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(getCurrentTerm, getVotedFor, setVotedFor))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState(commitIndex, lastApplied))

import Kontiki.Raft.Internal.State (Role(Follower), State(F), SomeState(SomeState))

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
                        , RVReq.RequestVoteRequest req
                        , RPC.Term req ~ term
                        , RVReq.Node req ~ node
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
