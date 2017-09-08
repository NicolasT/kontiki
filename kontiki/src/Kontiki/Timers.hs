{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Timers (
      TimersT
    , runTimersT
    , Timers
    , newTimers
    , TimeoutHandler(..)
    , readTimeout
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue

import Control.Concurrent.Suspend (Delay)
import Control.Concurrent.Timer (TimerIO, newTimer, oneShotStart, stopTimer)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)

import Katip (Katip, KatipContext)

import qualified Kontiki.Raft.Classes.Config as C
import Kontiki.Raft.Classes.RPC (MonadRPC)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.Timers (MonadTimers(startElectionTimer, cancelElectionTimer, startHeartbeatTimer))

newtype TimersT m a = TimersT { unTimersT :: ReaderT Timers m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadLogger, Katip, KatipContext,
              MonadCatch, MonadMask, MonadThrow)

instance (Monad m, MonadIO m) => MonadTimers (TimersT m) where
    startElectionTimer = TimersT $ do
        ts <- ask
        d <- liftIO $ timersMkElectionTimeout ts
        void $ liftIO $ oneShotStart (timersElectionTimer ts) (atomically $ TQueue.writeTQueue (timersQueue ts) ElectionTimeout) d
    cancelElectionTimer = TimersT $ do
        ts <- ask
        liftIO $ stopTimer $ timersElectionTimer ts
    startHeartbeatTimer = TimersT $ do
        ts <- ask
        d <- liftIO $ timersMkHeartbeatTimeout ts
        void $ liftIO $ oneShotStart (timersHeartbeatTimer ts) (atomically $ TQueue.writeTQueue (timersQueue ts) HeartbeatTimeout) d

instance (Monad m, MonadRPC m) => MonadRPC (TimersT m) where
    type Node (TimersT m) = RPC.Node m
    type RequestVoteRequest (TimersT m) = RPC.RequestVoteRequest m
    type AppendEntriesRequest (TimersT m) = RPC.AppendEntriesRequest m

instance (Monad m, C.MonadConfig m) => C.MonadConfig (TimersT m) where
    type Node (TimersT m) = C.Node m

data Timeout = ElectionTimeout
             | HeartbeatTimeout
    deriving (Show, Eq)

data TimeoutHandler m = TimeoutHandler { onElectionTimeout :: m ()
                                       , onHeartbeatTimeout :: m ()
                                       }

readTimeout :: Timers -> STM (TimeoutHandler m -> m ())
readTimeout Timers{..} = do
    t <- TQueue.readTQueue timersQueue
    return $ \TimeoutHandler{..} -> case t of
        ElectionTimeout -> onElectionTimeout
        HeartbeatTimeout -> onHeartbeatTimeout

data Timers = Timers { timersQueue :: TQueue Timeout
                     , timersMkElectionTimeout :: IO Delay
                     , timersMkHeartbeatTimeout :: IO Delay
                     , timersElectionTimer :: TimerIO
                     , timersHeartbeatTimer :: TimerIO
                     }

newTimers :: IO Delay -> IO Delay -> IO Timers
newTimers mkElectionTimeout mkHeartbeatTimeout =
    Timers <$> TQueue.newTQueueIO
           <*> pure mkElectionTimeout
           <*> pure mkHeartbeatTimeout
           <*> newTimer
           <*> newTimer

runTimersT :: Timers -> TimersT m a -> m a
runTimersT ts = flip runReaderT ts . unTimersT
