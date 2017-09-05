{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Control.Monad.Logger.Katip.Test (tests) where

import Control.Concurrent (yield)
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import Data.Text (Text)
import Data.Text.Lazy (pack)

import Data.Aeson (Value(Array, Number))

import Control.Concurrent.Async.Lifted.Safe (async, wait)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, around, describe, it, shouldMatchList, shouldNotReturn, shouldReturn, testSpecs)

import Katip (
    Item(_itemLoc, _itemMessage, _itemNamespace, _itemPayload), LogStr(unLogStr), KatipT, Namespace(Namespace),
    Severity(DebugS, ErrorS, InfoS, WarningS), Verbosity(V2),
    closeScribes, katipAddContext, katipAddNamespace, katipNoLogging, logMsg, runKatipT, runKatipContextT, sl)

import Control.Monad.Logger (logDebug, logDebugN, logErrorN, logInfoN, logWarnN, logOther)

import Utils (summarizeLogs, withLogEnv)

import qualified Control.Monad.Logger.Katip.Test.ShouldNotTypecheck

import Control.Monad.Logger.Katip (Katip, KatipContext, KatipLoggingT, runKatipLoggingT)

specs :: Spec
specs = around (withLogEnv DebugS V2) $
    describe "KatipLoggingT" $ do
        it "emits a warning (once) when LevelOther is used" $ \(logEnv, tvar) -> do
            runKatipT logEnv $ runKatipLoggingT @Katip $ do
                $(logOther "fatal") "Fatal issue encountered"
                $(logOther "trace") "Just tracing"
            _ <- closeScribes logEnv

            (filter (\(l, _, _, _) -> l == WarningS) <$> summarizeLogs tvar) `shouldReturn` [
                (WarningS, "monad-logger-katip doesn't support LevelOther, using ErrorS instead", Namespace ["monad-logger-katip"], mempty)
              ]

        it "emits LevelOther messages using ErrorS" $ \(logEnv, tvar) -> do
            runKatipT logEnv $ runKatipLoggingT @Katip $ do
                $(logOther "fatal") "Fatal issue encountered"
                $(logOther "trace") "Just tracing"
            _ <- closeScribes logEnv

            (map (\(l, _, _, _) -> l) <$> summarizeLogs tvar) `shouldReturn` [
                ErrorS
              , ErrorS
              ]

        it "prepends LevelOther levels to the log message" $ \(logEnv, tvar) -> do
            runKatipT logEnv $ runKatipLoggingT @Katip $ do
                $(logOther "fatal") "Fatal issue encountered"
                $(logOther "trace") "Just tracing"
            _ <- closeScribes logEnv

            (map (\(_, m, _, _) -> m) <$> summarizeLogs tvar) `shouldReturn` [
                "fatal: Fatal issue encountered"
              , "trace: Just tracing"
              ]

        let levelMap = [ ("Debug", logDebugN, DebugS)
                       , ("Info", logInfoN, InfoS)
                       , ("Warn", logWarnN, WarningS)
                       , ("Error", logErrorN, ErrorS)
                       ] :: [(String, Text -> KatipLoggingT Katip (KatipT IO) (), Severity)]
        forM_ levelMap $ \(n, act, lvl) ->
            it ("correctly maps log level " ++ n) $ \(logEnv, tvar) -> do
                runKatipT logEnv $ runKatipLoggingT @Katip $
                    act "Message"
                _ <- closeScribes logEnv

                (map (\(l, m, _, _) -> (l, m)) <$> summarizeLogs tvar) `shouldReturn` [
                    (lvl, "Message")
                  ]

        it "discards made-up source locations" $ \(logEnv, tvar) -> do
            runKatipT logEnv $ runKatipLoggingT @Katip $
                logDebugN "Message"
            _ <- closeScribes logEnv

            (_itemLoc . head) <$> readTVarIO tvar  `shouldReturn` Nothing

        it "passes along source locations" $ \(logEnv, tvar) -> do
            runKatipT logEnv $ runKatipLoggingT @Katip $
                $(logDebug) "Message"
            _ <- closeScribes logEnv

            (_itemLoc . head) <$> readTVarIO tvar `shouldNotReturn` Nothing

        it "can work in a KatipContext context" $ \(logEnv, tvar) -> do
            runKatipContextT logEnv () "main" $ runKatipLoggingT @KatipContext $
                $(logDebug) "Message"
            _ <- closeScribes logEnv

            summarizeLogs tvar `shouldReturn` [
                (DebugS, "Message", Namespace ["monad-logger-katip", "main"], mempty)
              ]

        it "has a Katip instance" $ \(logEnv, tvar) -> do
            runKatipT logEnv $ runKatipLoggingT @KatipContext $
                katipNoLogging $
                    logMsg  "ns" InfoS "Message"
            _ <- closeScribes logEnv

            summarizeLogs tvar `shouldReturn` []

        it "has a KatipContext instance" $ \(logEnv, tvar) -> do
            runKatipContextT logEnv () "main" $ runKatipLoggingT @KatipContext $
                katipAddNamespace "sub" $
                    katipAddContext (sl "subctx" ([1.0] :: [Double])) $
                        $(logDebug) "Message"
            _ <- closeScribes logEnv

            (map (\i -> (unLogStr $ _itemMessage i, _itemPayload i, _itemNamespace i)) <$> readTVarIO tvar) `shouldReturn` [
                ("Message", [("subctx", Array [Number 1.0])], Namespace ["monad-logger-katip", "main", "sub"])
              ]

        it "works with MonadBaseControl and async" $ \(logEnv, tvar) -> do
            runKatipT logEnv $ runKatipLoggingT @Katip $ do
                t <- async $ do
                    $(logDebug) "Message 1"
                    liftIO yield
                    $(logDebug) "Message 2"

                $(logDebug) "Message 3"
                liftIO yield
                $(logDebug) "Message 4"
                wait t
                $(logDebug) "Message 5"
            _ <- closeScribes logEnv

            m <- map (\(_, m, _, _) -> m) <$> summarizeLogs tvar
            m `shouldMatchList` ["Message " <> pack (show i) | i <- [1..5 :: Int]]

tests :: IO TestTree
tests = do
    cmlkts <- Control.Monad.Logger.Katip.Test.ShouldNotTypecheck.tests
    specs' <- testSpecs specs

    return $ testGroup "Control.Monad.Logger.Katip" $ cmlkts ++ specs'
