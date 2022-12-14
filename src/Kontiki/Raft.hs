{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Kontiki.Raft
  ( initialize,
    tests,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Kontiki.Raft.Class (MNodeId, MTerm, MonadRaftIO, getCurrentTerm, getVotedFor, setPersistentState, term0)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Lifted (shouldBe)

initialize :: MonadRaftIO m => m ()
initialize = setPersistentState term0 Nothing

tests :: (MonadRaftIO m, MonadIO m, Show (MTerm m), Show (MNodeId m)) => (forall a. m a -> IO a) -> Spec
tests run = describe "initialize" $ do
  it "sets the persisted term to term0" $
    run $ do
      initialize
      t <- getCurrentTerm
      t `shouldBe` term0

  it "sets the persisted vote to Nothing" $ do
    run $ do
      initialize
      v <- getVotedFor
      v `shouldBe` Nothing
