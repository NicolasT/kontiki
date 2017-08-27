{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Internal.AllServers (
      checkTerm
    ) where

import Control.Monad (when)

import Control.Monad.State.Class (MonadState)

import Control.Lens ((^.))

import Data.Default.Class (Default)

import Control.Monad.Logger (MonadLogger, logInfo)

-- import qualified Kontiki.Raft.Classes.FSM as FSM
-- import Kontiki.Raft.Classes.FSM (MonadFSM(applyEntry))
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC (HasTerm(term))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(getCurrentTerm, setCurrentTerm))
-- import qualified Kontiki.Raft.Classes.State.Volatile as V
import Kontiki.Raft.Classes.State.Volatile (VolatileState)
-- import qualified Kontiki.Raft.Classes.Types as T
-- import Kontiki.Raft.Classes.Types (Index(succIndex))

import Kontiki.Raft.Internal.Follower (convertToFollower)
import Kontiki.Raft.Internal.State (SomeState)

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

checkTerm :: ( MonadState (SomeState volatileState volatileLeaderState) m
             , MonadPersistentState m
             , HasTerm msg
             , RPC.Term msg ~ term
             , P.Term m ~ term
             , Ord term
             , VolatileState volatileState
             , Default volatileState
             , MonadLogger m
             )
          => msg
          -> m ()
checkTerm msg = do
    ct <- getCurrentTerm
    when (msg ^. term > ct) $ do
        $(logInfo) "Received message with higher term, converting to follower"
        setCurrentTerm (msg ^. term)
        convertToFollower
