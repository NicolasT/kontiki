module Data.Conduit.RollingQueue (
      RollingQueue
    , sourceRollingQueue
    , sinkRollingQueue
    , RollingQueue.new
    , RollingQueue.newIO
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Concurrent.STM (atomically)

import Data.STM.RollingQueue (RollingQueue)
import qualified Data.STM.RollingQueue as RollingQueue

import Data.Conduit

sourceRollingQueue :: MonadIO m => RollingQueue a -> Source m a
sourceRollingQueue q = loop
  where
    loop = do
        (e, _) <- liftIO $ atomically $ RollingQueue.read q
        yield e
        loop

sinkRollingQueue :: MonadIO m => RollingQueue a -> Sink a m ()
sinkRollingQueue q = awaitForever (liftIO . atomically . RollingQueue.write q)

