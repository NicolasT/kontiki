{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Kontiki.Server (main) where

import Control.Concurrent (MVar, forkIO, newChan, newMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.State (runStateT)
import Control.Monad.Logger (MonadLogger, logInfo, logInfoSH, unChanLoggingT, runChanLoggingT, runStderrLoggingT)

import Data.Text (Text)

import Database.LevelDB.Base (DB)
import qualified Database.LevelDB.Base as DB

import Network.GRPC.HighLevel.Generated (GRPCMethodType(Normal), ServerRequest(ServerNormalRequest), ServerResponse(ServerNormalResponse), StatusCode(StatusOk), defaultServiceOptions)

import qualified Kontiki.Raft as K

import qualified Kontiki.Protocol.Server as S
import Kontiki.Protocol.Server.Instances ()
import Kontiki.Types (Node(Node))
import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)

nodeRequestVote :: ( MonadLogger m
                   , MonadIO m
                   )
                => DB
                -> MVar (K.SomeState VolatileState ())
                -> ServerRequest 'Normal S.RequestVoteRequest S.RequestVoteResponse
                -> m (ServerResponse 'Normal S.RequestVoteResponse)
nodeRequestVote db state (ServerNormalRequest meta req) = do
    s <- liftIO $ takeMVar state
    $(logInfoSH) ("Request" :: Text, req)
    $(logInfoSH) ("Meta" :: Text, meta)
    $(logInfoSH) ("Current state" :: Text, s)

    (resp, s') <- runPersistentStateT db $ flip runStateT s $ do
        resp <- K.onRequestVoteRequest (Node "") req
        return (ServerNormalResponse resp mempty StatusOk "")

    $(logInfoSH) ("New state" :: Text, s')
    liftIO $ putMVar state s'

    return resp

nodeAppendEntries :: ServerRequest 'Normal S.AppendEntriesRequest S.AppendEntriesResponse
                  -> IO (ServerResponse 'Normal S.AppendEntriesResponse)
nodeAppendEntries = undefined

main :: IO ()
main = do
    logs <- newChan
    _ <- forkIO $ runStderrLoggingT $ unChanLoggingT logs

    let state0 = K.initialState :: K.SomeState VolatileState ()
    state <- newMVar state0

    runChanLoggingT logs $ do
        $(logInfo) "Starting Kontiki"

        DB.withDB "/tmp/kontiki-db" DB.defaultOptions { DB.createIfMissing = True } $ \db -> do
            runPersistentStateT db $
                K.initializePersistentState

            let opts = defaultServiceOptions
                impl = S.Node { S.nodeRequestVote = \req -> runChanLoggingT logs $ nodeRequestVote db state req
                              , S.nodeAppendEntries = nodeAppendEntries
                              }
            liftIO $ S.nodeServer impl opts
