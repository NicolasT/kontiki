module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Control.Monad.Logger.Katip.Test
import qualified Control.Monad.Logger.Katip.Orphans.Test

main :: IO ()
main = do
    cmlk <- Control.Monad.Logger.Katip.Test.tests
    cmlko <- Control.Monad.Logger.Katip.Orphans.Test.tests
    defaultMain $ testGroup "monad-logger-katip" [
        cmlk
      , cmlko
      ]

{-
import Control.Concurrent (threadDelay, yield)
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString(fromString))
import System.Environment (getProgName)
import System.IO (stderr)

import Control.Concurrent.Async.Lifted.Safe (wait, withAsync)

import Control.Monad.Logger (MonadLogger(monadLoggerLog), logDebug, logError, logInfo, logInfoS, logOther)

import Katip (
    ColorStrategy(ColorIfTerminal), LogEnv, LogItem, Namespace, Katip, KatipT,
    KatipContext, KatipContextT, Severity(InfoS, ErrorS), Verbosity(V2),
    closeScribes, defaultScribeSettings, initLogEnv, logT, logTM, mkHandleScribe,
    registerScribe, runKatipT, runKatipContextT)

import Control.Monad.Logger.Katip (defaultMonadLoggerLog, katipLogItem, runKatipLoggingT)
import Control.Monad.Logger.Katip.Orphans ()

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
            runKatipLoggingT @KatipContext $ do
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

        -- MyMonad doesn't have a MonadLogger instance. runKatipLoggingT
        -- to the rescue.
        runMyMonad le initialContext initialNamespace $ runKatipLoggingT @KatipContext $
            $(logInfo) "Info from MyMonad"

        -- Custom monad with MonadLoggerLog instance using
        -- defaultMonadLoggerLog
        runMyOtherMonad le $
            $(logInfo) "Info from MyOtherMonad"

        runMyMonad le () "ns" $ runKatipLoggingT @Katip examples
        runMyMonad le () "ns" $ runKatipLoggingT @KatipContext examples
        runMyOtherMonad le $ runKatipLoggingT @Katip examples
        -- runMyOtherMonad le $ runKatipLoggingT @KatipContext examples


newtype MyMonad a = MyMonad { unMyMonad :: KatipContextT IO a }
    deriving (Functor, Applicative, Monad, MonadIO, Katip, KatipContext)

runMyMonad :: LogItem c => LogEnv -> c -> Namespace -> MyMonad a -> IO a
runMyMonad le c ns = runKatipContextT le c ns . unMyMonad

newtype MyOtherMonad a = MyOtherMonad { unMyOtherMonad :: KatipT IO a }
    deriving (Functor, Applicative, Monad, MonadIO, Katip)

instance MonadLogger MyOtherMonad where
    monadLoggerLog = defaultMonadLoggerLog $ katipLogItem ()

runMyOtherMonad :: LogEnv -> MyOtherMonad a -> IO a
runMyOtherMonad le = runKatipT le . unMyOtherMonad

-}
