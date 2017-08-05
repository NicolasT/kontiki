module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (runStateT)

import Data.Default.Class (Default(def))

import Control.Monad.Logger (LoggingT, runStderrLoggingT)

import qualified Kontiki.Raft as R
import qualified Kontiki.Raft.Types as T

testOnRequestVoteRequest :: LoggingT IO ()
testOnRequestVoteRequest = do
    res <- flip runStateT T.initialPersistentState $ T.runPersistentState $ do
        R.initializePersistentState
        let s0 = R.initialState :: R.SomeState T.VolatileState Int
        liftIO $ print s0
        flip runStateT s0 $ do
            R.onRequestVoteRequest (def :: T.RequestVoteRequest)
    liftIO $ print res

main :: IO ()
main = do
    runStderrLoggingT testOnRequestVoteRequest
