{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kontiki.Server.Monad (
      ServerT
    , runServerT
    , runInIO
    , withAsync
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(lift))

import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (
    MonadBaseControl(StM, liftBaseWith, restoreM), ComposeSt, defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(StT, liftWith, restoreT))
import Control.Concurrent.Async.Lifted.Safe (Async, Forall, Pure)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)

import Data.Text (Text)

import Katip (
    LogEnv, LogItem, Katip, KatipContext, KatipContextT, Namespace, getLogEnv, getKatipContext, getKatipNamespace, runKatipContextT)

import Control.Monad.Logger (MonadLogger)
import Control.Monad.Logger.Katip (KatipLoggingT, runKatipLoggingT)

import Control.Monad.Metrics (Metrics, MonadMetrics, getMetrics)
import Control.Monad.Trans.Metrics (MetricsT, runMetricsT)

import qualified Kontiki.Server.Async as Async

newtype ServerT m a = ServerT { unServerT :: MetricsT (KatipLoggingT KatipContext (KatipContextT m)) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO
             , MonadMetrics
             , Katip, KatipContext
             , MonadLogger
             , MonadCatch, MonadMask, MonadThrow
             , MonadBase b
             )

instance MonadTrans ServerT where
    lift = ServerT . lift . lift . lift

instance MonadTransControl ServerT where
    type StT ServerT a = StT KatipContextT (StT (KatipLoggingT KatipContext) (StT MetricsT a))
    liftWith f = ServerT $ liftWith $ \run -> liftWith $ \run' -> liftWith $ \run'' -> f $ run'' . run' . run . unServerT
    restoreT = ServerT . restoreT . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (ServerT m) where
    type StM (ServerT m) a = ComposeSt ServerT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM


runServerT :: LogItem c => Metrics -> LogEnv -> c -> Namespace -> ServerT m a -> m a
runServerT metrics logEnv logContext logNamespace =
    runKatipContextT logEnv logContext logNamespace . runKatipLoggingT . flip runMetricsT metrics . unServerT

runInIO :: MonadIO m => (a -> ServerT IO b) -> ServerT m (a -> IO b)
runInIO act = do
    metrics <- getMetrics
    (logEnv, logContext, logNamespace) <- (,,) <$> getLogEnv <*> getKatipContext <*> getKatipNamespace
    return $ runServerT metrics logEnv logContext logNamespace . act

withAsync :: ( MonadIO m
             , MonadBaseControl IO m
             , MonadMask m
             , Forall (Pure (ServerT m))
             )
          => Text
          -> ServerT m a
          -> (Async a -> ServerT m b)
          -> ServerT m b
withAsync = Async.withAsync
