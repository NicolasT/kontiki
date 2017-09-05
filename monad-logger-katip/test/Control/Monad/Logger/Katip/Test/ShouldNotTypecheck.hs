{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}

module Control.Monad.Logger.Katip.Test.ShouldNotTypecheck (tests) where

import Control.Monad.Identity (Identity)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (around, describe, it, testSpecs)

import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Katip (KatipT, LogEnv, Severity(DebugS), Verbosity(V2), runKatipT)

import Control.Monad.Logger (logInfo)

import Utils (withLogEnv)

import Control.Monad.Logger.Katip (KatipContext, runKatipLoggingT)

tests :: IO [TestTree]
tests = testSpecs $
    around (withLogEnv DebugS V2) $
        describe "runKatipLoggingT" $
            it "requires KatipContext when ctx is KatipContext" $ \(logEnv, _) ->
                shouldNotTypecheck $
                    (runKatipT :: LogEnv -> KatipT Identity a -> Identity a) logEnv $ runKatipLoggingT @KatipContext $
                        $(logInfo) "Hello, world!"
