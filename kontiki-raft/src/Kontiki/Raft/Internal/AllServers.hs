{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Internal.AllServers (
      checkTerm
    ) where

import qualified Control.Monad.Indexed as Ix
import Control.Monad.Indexed.State (IxMonadState)

import Control.Lens ((^.))

import Control.Monad.Logger (MonadLogger, logInfo)

-- import qualified Kontiki.Raft.Classes.FSM as FSM
-- import Kontiki.Raft.Classes.FSM (MonadFSM(applyEntry))
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC (HasTerm(term))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(getCurrentTerm, setCurrentTerm))
-- import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.State.Volatile (Role(Follower), VolatileState)
-- import Kontiki.Raft.Classes.Types (Index(succIndex))
import Kontiki.Raft.Classes.Timers (MonadTimers)

import Kontiki.Raft.Internal.Follower (convertToFollower)
import Kontiki.Raft.Internal.State (Some, wrap)

{-
applyToCommitIndex :: ( MonadState (Role volatileState volatileLeaderState) m
                      , VolatileState volatileState
                      , MonadPersistentState m
                      , MonadFSM m
                      , V.Index s ~ index
                      , P.Index m ~ index
                      , Ord index
                      , T.Index index
                      , P.Entry m ~ FSM.Entry m
                      )
                      => m ()
applyToCommitIndex = do
    ci <- use commitIndex
    la <- use lastApplied
    when (ci > la) $ do
        lastApplied %= succIndex
        (_, entry) <- getLogEntry la
        applyEntry entry
-}

checkTerm :: forall m m' r msg volatileState term.
             ( IxMonadState m
             , MonadPersistentState m'
             , MonadLogger m'
             , MonadTimers (m (volatileState 'Follower) (volatileState 'Follower))
             , m' ~ m (volatileState r) (volatileState r)
             , term ~ P.Term m'
             , HasTerm msg
             , RPC.Term msg ~ term
             , Ord term
             , VolatileState volatileState
             )
          => msg
          -> m (volatileState r) (Some volatileState) ()
checkTerm msg =
    (getCurrentTerm :: m' term) >>>= \ct ->
        if (msg ^. term > ct)
            then $(logInfo) "Received message with higher term, converting to follower" >>>= \() ->
                (setCurrentTerm (msg ^. term) :: m' ()) >>>= \() ->
                    convertToFollower >>>= \() ->
                        wrap
            else wrap
  where
    a >>>= b = Ix.ibind b a
