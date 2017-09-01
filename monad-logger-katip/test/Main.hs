{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- This code doesn't really test functionality, it merely asserts code
-- that's expected to compile/type-check actually does so.

module Main (main) where

import Control.Concurrent (threadDelay, yield)
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString(fromString))
import System.Environment (getProgName)
import System.IO (stderr)

import Control.Concurrent.Async.Lifted.Safe (wait, withAsync)

import Control.Monad.Logger (MonadLogger, logDebug, logError, logInfo, logInfoS, logOther)

import Katip (
    ColorStrategy(ColorIfTerminal), Severity(InfoS, ErrorS), Verbosity(V2), closeScribes, defaultScribeSettings,
    initLogEnv, logT, logTM, mkHandleScribe, registerScribe, runKatipT, runKatipContextT)

import Control.Monad.Logger.Katip (runKatipLoggingT)

logStuff :: (MonadIO m, MonadLogger m) => m ()
logStuff = do
    $(logDebug) "Debug message"
    $(logInfo) "Info message"
    liftIO yield
    $(logError) "Error message"
    $(logInfoS) "source" "This is a message with a source"
    $(logInfoS) "dotted.source" "This is another source"

examples :: (MonadIO m, MonadLogger m) => m ()
examples = do
     $(logInfoS) "db.get" "Retrieving DB rows"
     $(logOther "fatal") "Unrecoverable error encountered"

main :: IO ()
main = do
    progName <- getProgName
    handleScribe <- mkHandleScribe ColorIfTerminal stderr InfoS V2
    let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv (fromString progName) "testing"
    bracket makeLogEnv closeScribes $ \le -> do
        let initialContext = ()
            initialNamespace = "main"
        runKatipContextT le initialContext initialNamespace $ do
            $(logTM) InfoS "Standard message"
            runKatipLoggingT $ do
                logStuff
                $(logOther "fatal") "Fatal message"
                $(logTM) InfoS "Another standard message"
                $(logT) () mempty InfoS "And yet another one"
            logStuff

        runKatipT le $ do
            $(logT) () mempty InfoS "From KatipT"
            logStuff
            $(logT) () mempty ErrorS "End of KatipT"

        runKatipContextT le initialContext initialNamespace $ do
            withAsync (logStuff >> liftIO (threadDelay 500)) $ \a -> do
                $(logInfo) "Some more info"
                liftIO yield
                $(logInfo) "And yet some more"
                wait a

            examples
