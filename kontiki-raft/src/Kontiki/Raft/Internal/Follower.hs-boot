{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Internal.Follower where

import Data.Default.Class (Default)

import Control.Monad.Indexed.State (IxMonadState)

import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.RPC.AppendEntriesResponse (AppendEntriesResponse)
import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState)
import qualified Kontiki.Raft.Classes.State.Persistent as P
import Kontiki.Raft.Classes.State.Volatile (VolatileState, Role(Follower))
import Kontiki.Raft.Classes.Timers (MonadTimers)

convertToFollower :: ( IxMonadState m
                     , Monad (m (volatileState 'Follower) (volatileState 'Follower))
                     , MonadTimers (m (volatileState 'Follower) (volatileState 'Follower))
                     , VolatileState volatileState
                     )
                  => m (volatileState r) (volatileState 'Follower) ()

onAppendEntriesRequest :: ( Monad m
                          , MonadTimers m
                          , MonadPersistentState m
                          , P.Term m ~ RPC.Term resp
                          , Default resp
                          , AppendEntriesResponse resp
                          )
                       => req
                       -> m resp
