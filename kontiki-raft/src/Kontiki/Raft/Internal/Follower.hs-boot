{-# LANGUAGE FlexibleContexts #-}

module Kontiki.Raft.Internal.Follower where

import GHC.Stack (HasCallStack)

import Control.Monad.State.Class (MonadState)

import Data.Default.Class (Default)

import Kontiki.Raft.Classes.State.Volatile (VolatileState)
import Kontiki.Raft.Classes.Timers (MonadTimers)

import Kontiki.Raft.Internal.State (SomeState)

convertToFollower :: ( Monad m
                     , MonadState (SomeState volatileState volatileCandidateState volatileLeaderState) m
                     , VolatileState volatileState
                     , Default volatileState
                     , MonadTimers m
                     )
                  => m ()

onAppendEntriesRequest :: HasCallStack
                       => req
                       -> m resp
