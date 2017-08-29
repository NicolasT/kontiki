module Kontiki.Raft.Classes.Timers (
      MonadTimers(..)
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)

class MonadTimers m where
    startElectionTimer :: m ()
    cancelElectionTimer :: m ()

    startHeartbeatTimer :: m ()

instance (Monad m, MonadTimers m) => MonadTimers (ReaderT r m) where
    startElectionTimer = lift startElectionTimer
    cancelElectionTimer = lift cancelElectionTimer
    startHeartbeatTimer = lift startHeartbeatTimer

instance (Monad m, MonadTimers m) => MonadTimers (StateT s m) where
    startElectionTimer = lift startElectionTimer
    cancelElectionTimer = lift cancelElectionTimer
    startHeartbeatTimer = lift startHeartbeatTimer
