{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.Timers (
      MonadTimers(..)
    ) where

import Control.Monad.Indexed.State (IxStateT)
import Control.Monad.Indexed.Trans (ilift)

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Strict as SStateT

class MonadTimers m where
    startElectionTimer :: m ()
    default startElectionTimer :: (Monad m', MonadTimers m', MonadTrans t, m ~ t m') => m ()
    startElectionTimer = lift startElectionTimer
    {-# INLINE startElectionTimer #-}
    cancelElectionTimer :: m ()
    default cancelElectionTimer :: (Monad m', MonadTimers m', MonadTrans t, m ~ t m') => m ()
    cancelElectionTimer = lift cancelElectionTimer
    {-# INLINE cancelElectionTimer #-}

    startHeartbeatTimer :: m ()
    default startHeartbeatTimer :: (Monad m', MonadTimers m', MonadTrans t, m ~ t m') => m ()
    startHeartbeatTimer = lift startHeartbeatTimer
    {-# INLINE startHeartbeatTimer #-}

instance (Monad m, MonadTimers m) => MonadTimers (ReaderT r m)
instance (Monad m, MonadTimers m) => MonadTimers (StateT s m)
instance (Monad m, MonadTimers m) => MonadTimers (SStateT.StateT s m)

instance (Monad m, MonadTimers m) => MonadTimers (IxStateT m i i) where
    startElectionTimer = ilift startElectionTimer
    cancelElectionTimer = ilift cancelElectionTimer
    startHeartbeatTimer = ilift startHeartbeatTimer
