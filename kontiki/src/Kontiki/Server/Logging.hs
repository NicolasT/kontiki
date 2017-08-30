{-# LANGUAGE LambdaCase #-}

module Kontiki.Server.Logging (
      Logger
    , mkLogger
    , withLogger
    , spawn
    ) where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.IO.Class (MonadIO)

import GHC.Conc.Sync (labelThread)

import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT)
import qualified Control.Monad.Logger as Logger

newtype Logger = Logger { loggerChan :: Chan (Loc, LogSource, LogLevel, LogStr) }

mkLogger :: IO Logger
mkLogger = Logger <$> Chan.newChan

withLogger :: MonadIO m => Logger -> LoggingT m a -> m a
withLogger l = Logger.runChanLoggingT (loggerChan l)

spawn :: Logger -> IO ThreadId
spawn l = forkIO $ do
    me <- myThreadId
    labelThread me "logger"
    Logger.runStderrLoggingT $ Logger.unChanLoggingT (loggerChan l)
