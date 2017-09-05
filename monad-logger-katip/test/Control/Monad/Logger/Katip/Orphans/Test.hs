{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Logger.Katip.Orphans.Test (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (around, describe, it, shouldReturn, testSpec)

import Control.Monad.Logger (MonadLogger, logInfo, logInfoS)

import Katip (Namespace(Namespace), Severity(DebugS, InfoS), Verbosity(V2),
    closeScribes, katipAddNamespace, runKatipT, runKatipContextT)

import Utils (summarizeLogs, withLogEnv)

import Control.Monad.Logger.Katip.Orphans ()

tests :: IO TestTree
tests = testSpec "Control.Monad.Logger.Katip.Orphans" $
    around (withLogEnv DebugS V2) $ do
        describe "KatipT" $ do
            it "has a MonadLogger instance" $ \(logEnv, _) -> do
                let act = return () :: MonadLogger m => m ()
                runKatipT logEnv act :: IO ()

            it "passes through log messages" $ \(logEnv, tvar) -> do
                runKatipT logEnv $ do
                    $(logInfo) "Demo"
                    $(logInfo) "Demo2"
                _ <- closeScribes logEnv
                summarizeLogs tvar `shouldReturn` [
                    (InfoS, "Demo", Namespace ["monad-logger-katip"], mempty)
                  , (InfoS, "Demo2", Namespace ["monad-logger-katip"], mempty)
                  ]

            it "recovers namespace extensions from log sources" $ \(logEnv, tvar) -> do
                runKatipT logEnv $ do
                    $(logInfoS) "single" "Single"
                    $(logInfoS) "multiple.levels" "Multiple"
                _ <- closeScribes logEnv
                summarizeLogs tvar `shouldReturn` [
                    (InfoS, "Single", Namespace ["monad-logger-katip", "single"], mempty)
                  , (InfoS, "Multiple", Namespace ["monad-logger-katip", "multiple", "levels"], mempty)
                  ]

        describe "KatipContextT" $ do
            it "has a MonadLogger instance" $ \(logEnv, _) -> do
                let act = return () :: MonadLogger m => m ()
                runKatipContextT logEnv () "main" act :: IO ()

            it "passes through log messages" $ \(logEnv, tvar) -> do
                runKatipContextT logEnv () "main" $ do
                    $(logInfo) "Demo"
                    $(logInfo) "Demo2"
                _ <- closeScribes logEnv
                summarizeLogs tvar `shouldReturn` [
                    (InfoS, "Demo", Namespace ["monad-logger-katip", "main"], mempty)
                  , (InfoS, "Demo2", Namespace ["monad-logger-katip", "main"], mempty)
                  ]

            it "retains namespace stacks" $ \(logEnv, tvar) -> do
                runKatipContextT logEnv () "main" $
                    katipAddNamespace "db" $ do
                        $(logInfo) "Querying database"
                        katipAddNamespace "demo" $
                            $(logInfo) "Demo"
                        $(logInfo) "Done querying"
                _ <- closeScribes logEnv
                summarizeLogs tvar `shouldReturn` [
                    (InfoS, "Querying database", Namespace ["monad-logger-katip", "main", "db"], mempty)
                  , (InfoS, "Demo", Namespace ["monad-logger-katip", "main", "db", "demo"], mempty)
                  , (InfoS, "Done querying", Namespace ["monad-logger-katip", "main", "db"], mempty)
                  ]

            it "recovers namespace extensions from log sources" $ \(logEnv, tvar) -> do
                runKatipContextT logEnv () "main" $ do
                    $(logInfoS) "single" "Single"
                    $(logInfoS) "multiple.levels" "Multiple"
                _ <- closeScribes logEnv
                summarizeLogs tvar `shouldReturn` [
                    (InfoS, "Single", Namespace ["monad-logger-katip", "main", "single"], mempty)
                  , (InfoS, "Multiple", Namespace ["monad-logger-katip", "main", "multiple", "levels"], mempty)
                  ]
