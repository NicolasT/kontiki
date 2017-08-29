{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kontiki.Timers (
      TimersT
    , runTimersT
    , Timers
    , newTimers
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)

import Control.Concurrent.Suspend (Delay)
import Control.Concurrent.Timer (TimerIO, newTimer, oneShotStart, stopTimer)

import Kontiki.Raft.Classes.Timers (MonadTimers(startElectionTimer, cancelElectionTimer, startHeartbeatTimer))

newtype TimersT m a = TimersT { unTimersT :: ReaderT Timers m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance (Monad m, MonadIO m) => MonadTimers (TimersT m) where
    startElectionTimer = TimersT $ do
        ts <- ask
        d <- liftIO $ timersMkElectionTimeout ts
        void $ liftIO $ oneShotStart (timersElectionTimer ts) (timersOnElectionTimeout ts) d
    cancelElectionTimer = TimersT $ do
        ts <- ask
        liftIO $ stopTimer $ timersElectionTimer ts
    startHeartbeatTimer = TimersT $ do
        ts <- ask
        d <- liftIO $ timersMkHeartbeatTimeout ts
        void $ liftIO $ oneShotStart (timersHeartbeatTimer ts) (timersOnHeartbeatTimeout ts) d

data Timers = Timers { timersMkElectionTimeout :: IO Delay
                     , timersOnElectionTimeout :: IO ()
                     , timersMkHeartbeatTimeout :: IO Delay
                     , timersOnHeartbeatTimeout :: IO ()
                     , timersElectionTimer :: TimerIO
                     , timersHeartbeatTimer :: TimerIO
                     }

newTimers :: (IO Delay, IO ()) -> (IO Delay, IO ()) -> IO Timers
newTimers (mkElectionTimeout, onElectionTimeout) (mkHeartbeatTimeout, onHeartbeatTimeout) =
    Timers mkElectionTimeout onElectionTimeout mkHeartbeatTimeout onHeartbeatTimeout <$> newTimer <*> newTimer

runTimersT :: Timers -> TimersT m a -> m a
runTimersT ts = flip runReaderT ts . unTimersT
