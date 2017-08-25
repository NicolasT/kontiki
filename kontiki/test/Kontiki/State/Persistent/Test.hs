module Kontiki.State.Persistent.Test (tests) where

import qualified Database.LevelDB.Base as L
import System.IO.Temp (withSystemTempDirectory)

import Control.Monad.Logger (runNoLoggingT)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import Test.Hspec (around, describe, it)
import Test.Hspec.Expectations (shouldBe)
import Test.QuickCheck (property)

import qualified Kontiki.Raft as K
import qualified Kontiki.Raft.Classes.Types as K
import qualified Kontiki.Raft.Classes.State.Persistent as K

import Kontiki.State.Persistent (runPersistentStateT)

withDatabase :: (L.DB -> IO ()) -> IO ()
withDatabase fn = withSystemTempDirectory "kontiki-test" $ \dir -> do
    L.withDB dir (L.defaultOptions { L.createIfMissing = True }) fn

tests :: IO TestTree
tests = testSpec "Kontiki.State.Persistent" $ do
    around withDatabase $ do
        describe "initializePersistentState" $ do
            let test act val db = do
                    r <- runNoLoggingT $ runPersistentStateT db $ do
                        K.initializePersistentState
                        act
                    r `shouldBe` val

            it "sets currentTerm to term0" $ test K.getCurrentTerm K.term0
            it "sets votedFor to Nothing" $ test K.getVotedFor Nothing

        describe "get/setCurrentTerm" $ do
            it "correctly stores new values" $ \db -> property $ \t -> do
                (t0, t', t0') <- runNoLoggingT $ runPersistentStateT db $ do
                    K.setCurrentTerm K.term0
                    t0 <- K.getCurrentTerm
                    K.setCurrentTerm t
                    t' <- K.getCurrentTerm
                    K.setCurrentTerm K.term0
                    t0' <- K.getCurrentTerm
                    return (t0, t', t0')
                t0 `shouldBe` K.term0
                t' `shouldBe` t
                t0' `shouldBe` K.term0

        describe "get/setVotedFor" $ do
            it "correctly stores new values" $ \db -> property $ \v -> do
                (v0, v', v0') <- runNoLoggingT $ runPersistentStateT db $ do
                    K.setVotedFor Nothing
                    v0 <- K.getVotedFor
                    K.setVotedFor v
                    v' <- K.getVotedFor
                    K.setVotedFor Nothing
                    v0' <- K.getVotedFor
                    return (v0, v', v0')
                v0 `shouldBe` Nothing
                v' `shouldBe` v
                v0' `shouldBe` Nothing
