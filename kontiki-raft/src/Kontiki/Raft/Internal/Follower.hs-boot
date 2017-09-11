{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Kontiki.Raft.Internal.Follower where

import GHC.Stack (HasCallStack)

import Control.Monad.Indexed.State (IxMonadState)

import Kontiki.Raft.Classes.State.Volatile (VolatileState, Role(Follower))
import Kontiki.Raft.Classes.Timers (MonadTimers)

convertToFollower :: ( IxMonadState m
                     , MonadTimers (m (volatileState 'Follower) (volatileState 'Follower))
                     , VolatileState volatileState
                     )
                  => m (volatileState r) (volatileState 'Follower) ()

onAppendEntriesRequest :: HasCallStack
                       => req
                       -> m resp
