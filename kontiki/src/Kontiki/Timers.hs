{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kontiki.Timers (
      TimersT
    , runTimersT
    , Timers
    , newTimers
    , TimeoutHandler(..)
    , readTimeout
    ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader.Class (MonadReader(ask, local))

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue

import Control.Concurrent.Suspend (Delay)
import Control.Concurrent.Timer (TimerIO, newTimer, oneShotStart, stopTimer)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)

import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (
    ComposeSt,
    MonadBaseControl(StM, liftBaseWith, restoreM) , defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(StT, liftWith, restoreT), defaultLiftWith, defaultRestoreT)

import Katip (Katip, KatipContext)

import Kontiki.Raft.Classes.RPC (MonadRPC)
import qualified Kontiki.Raft.Classes.RPC as RPC
import Kontiki.Raft.Classes.Timers (MonadTimers(startElectionTimer, cancelElectionTimer, startHeartbeatTimer))

newtype TimersT m a = TimersT { unTimersT :: ReaderT Timers m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadLogger, Katip, KatipContext,
              MonadCatch, MonadMask, MonadThrow, MonadBase b)

instance MonadTransControl TimersT where
    type StT TimersT a = StT (ReaderT Timers) a
    liftWith = defaultLiftWith TimersT unTimersT
    restoreT = defaultRestoreT TimersT

instance MonadBaseControl b m => MonadBaseControl b (TimersT m) where
    type StM (TimersT m) a = ComposeSt TimersT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM



instance MonadReader r m => MonadReader r (TimersT m) where
    ask = lift ask
    local = mapTimersT . local

mapTimersT :: (m a -> n b) -> TimersT m a -> TimersT n b
mapTimersT f = TimersT . mapReaderT f . unTimersT

instance (Monad m, MonadIO m) => MonadTimers (TimersT m) where
    startElectionTimer = TimersT $ do
        ts <- ask
        d <- liftIO $ timersMkElectionTimeout ts
        void $ liftIO $ oneShotStart (timersElectionTimer ts) (atomically $ TBQueue.writeTBQueue (timersQueue ts) ElectionTimeout) d
    cancelElectionTimer = TimersT $ do
        ts <- ask
        liftIO $ stopTimer $ timersElectionTimer ts
    startHeartbeatTimer = TimersT $ do
        ts <- ask
        d <- liftIO $ timersMkHeartbeatTimeout ts
        void $ liftIO $ oneShotStart (timersHeartbeatTimer ts) (atomically $ TBQueue.writeTBQueue (timersQueue ts) HeartbeatTimeout) d

instance (Monad m, MonadRPC m) => MonadRPC (TimersT m) where
    type Node (TimersT m) = RPC.Node m
    type RequestVoteRequest (TimersT m) = RPC.RequestVoteRequest m
    type AppendEntriesRequest (TimersT m) = RPC.AppendEntriesRequest m

data Timeout = ElectionTimeout
             | HeartbeatTimeout
    deriving (Show, Eq)

data TimeoutHandler m = TimeoutHandler { onElectionTimeout :: m ()
                                       , onHeartbeatTimeout :: m ()
                                       }

readTimeout :: Timers -> STM (TimeoutHandler m -> m ())
readTimeout Timers{..} = do
    t <- TBQueue.readTBQueue timersQueue
    return $ \TimeoutHandler{..} -> case t of
        ElectionTimeout -> onElectionTimeout
        HeartbeatTimeout -> onHeartbeatTimeout

data Timers = Timers { timersQueue :: TBQueue Timeout
                     , timersMkElectionTimeout :: IO Delay
                     , timersMkHeartbeatTimeout :: IO Delay
                     , timersElectionTimer :: TimerIO
                     , timersHeartbeatTimer :: TimerIO
                     }

newTimers :: IO Delay -> IO Delay -> IO Timers
newTimers mkElectionTimeout mkHeartbeatTimeout =
    Timers <$> TBQueue.newTBQueueIO 1024
           <*> pure mkElectionTimeout
           <*> pure mkHeartbeatTimeout
           <*> newTimer
           <*> newTimer

runTimersT :: Timers -> TimersT m a -> m a
runTimersT ts = flip runReaderT ts . unTimersT
