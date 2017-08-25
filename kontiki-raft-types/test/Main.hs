{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (runStateT)

import Data.Default.Class (Default(def))

import Control.Monad.Logger (LoggingT, runStderrLoggingT)

import qualified Kontiki.Raft as R
import qualified Kontiki.Raft.Types as T

testOnRequestVoteRequest :: LoggingT IO ()
testOnRequestVoteRequest = do
    res <- T.runRPC $ flip runStateT T.initialPersistentState $ T.runPersistentState $ do
        R.initializePersistentState
        let s0 = R.initialState :: R.SomeState T.VolatileState Int
        liftIO $ print s0
        flip runStateT s0 $ do
            let node = def
            resp :: T.RequestVoteResponse <- R.onRequestVoteRequest node (def :: T.RequestVoteRequest)
            return resp
    liftIO $ print res

main :: IO ()
main = do
    runStderrLoggingT testOnRequestVoteRequest
