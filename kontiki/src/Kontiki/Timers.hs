{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Kontiki.Timers (
      TimersT
    , runTimersT
    , Timers
    , newTimers
    , TimeoutHandler(..)
    , readTimeout
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue

import Control.Concurrent.Suspend (Delay)
import Control.Concurrent.Timer (TimerIO, newTimer, oneShotStart, stopTimer)

import Kontiki.Raft.Classes.Timers (MonadTimers(startElectionTimer, cancelElectionTimer, startHeartbeatTimer))

newtype TimersT m a = TimersT { unTimersT :: ReaderT Timers m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

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
