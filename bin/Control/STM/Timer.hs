{-# LANGUAGE RecordWildCards #-}

module Control.STM.Timer where

import Control.Monad(void)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Concurrent
import Control.Concurrent.STM

-- NOTE This implementation isn't very safe

data Timer = Timer { timerThread :: TMVar ThreadId
                   , timerTMVar :: TMVar ()
                   }

new :: STM Timer
new = do
    tid <- newEmptyTMVar
    t <- newEmptyTMVar

    return $ Timer tid t

newIO :: MonadIO m => m Timer
newIO = liftIO $ do
    tid <- newEmptyTMVarIO
    t <- newEmptyTMVarIO

    return $ Timer tid t

reset :: MonadIO m => Timer -> Int -> m ()
reset t@Timer{..} i = liftIO $ do
    cancel t

    n <- forkIO $ do
            threadDelay i
            void $ atomically $ tryPutTMVar timerTMVar ()

    atomically $ putTMVar timerThread n

cancel :: MonadIO m => Timer -> m ()
cancel Timer{..} = liftIO $ do
    tid <- atomically $ tryTakeTMVar timerThread
    case tid of
        Nothing -> return ()
        Just tid' -> killThread tid'

await :: Timer -> STM ()
await Timer{..} = takeTMVar timerTMVar
