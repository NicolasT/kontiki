{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Kontiki.Server (main) where

import Control.Concurrent (Chan, MVar, forkIO, newChan, newMVar, readMVar)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.MVar.Lifted (modifyMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT, MonadLogger, logDebugSH, logInfo, logInfoSH, unChanLoggingT, runChanLoggingT, runNoLoggingT, runStderrLoggingT)

import Data.Text (Text)

import Control.Lens (view)

import Database.LevelDB.Base (DB)
import qualified Database.LevelDB.Base as DB

import Network.GRPC.HighLevel.Generated (GRPCMethodType(Normal), ServerRequest(ServerNormalRequest), ServerResponse(ServerNormalResponse), StatusCode(StatusOk), defaultServiceOptions)

import qualified System.Remote.Monitoring as EKG (forkServer, serverMetricStore)

import Control.Concurrent.Suspend (msDelay, sDelay)

import qualified Kontiki.Raft as K
import qualified Kontiki.Raft.Classes.State.Persistent as K
import qualified Kontiki.Raft.Classes.State.Volatile as K

import qualified Kontiki.Protocol.Server as S
import Kontiki.Protocol.Server.Instances ()
import qualified Kontiki.Server.EKG  as EKG (registerStats)
import Kontiki.State.Persistent (PersistentStateT, runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)
import Kontiki.Timers (Timers, TimersT, newTimers, runTimersT)
import Kontiki.Types (Node(Node, getNode), Index(getIndex), Term(getTerm))

handleRequest :: ( Monad m
                 , MonadIO m
                 , MonadBaseControl IO m
                 )
              => Chan (Loc, LogSource, LogLevel, LogStr)
              -> DB
              -> Timers
              -> MVar (K.SomeState VolatileState ())
              -> (req -> StateT (K.SomeState VolatileState ()) (PersistentStateT (TimersT (LoggingT m))) resp)
              -> req
              -> m resp
handleRequest logs db timers stateMVar handler = \req -> runChanLoggingT logs $ do
    $(logInfo) "Handling request"
    modifyMVar stateMVar $ \state -> do
        $(logDebugSH) ("Current state" :: Text, state)
        (resp, state') <- runTimersT timers $ runPersistentStateT db $ flip runStateT state $
            handler req
        $(logDebugSH) ("New state" :: Text, state')
        return (state', resp)

nodeRequestVote :: (Monad m, MonadIO m)
                => ServerRequest 'Normal S.RequestVoteRequest S.RequestVoteResponse
                -> StateT (K.SomeState VolatileState ()) (PersistentStateT (TimersT (LoggingT m))) (ServerResponse 'Normal S.RequestVoteResponse)
nodeRequestVote (ServerNormalRequest _meta req) = do
    $(logDebugSH) ("Request" :: Text, req)
    resp <- K.onRequestVoteRequest req
    $(logDebugSH) ("Response" :: Text, resp)
    return (ServerNormalResponse resp mempty StatusOk "")

nodeAppendEntries :: ServerRequest 'Normal S.AppendEntriesRequest S.AppendEntriesResponse
                  -> IO (ServerResponse 'Normal S.AppendEntriesResponse)
nodeAppendEntries = undefined

main :: IO ()
main = do
    logs <- newChan
    _ <- forkIO $ runStderrLoggingT $ unChanLoggingT logs

    ekg <- EKG.forkServer "localhost" 8000

    let state0 = K.initialState :: K.SomeState VolatileState ()
    state <- newMVar state0

    runChanLoggingT logs $ do
        $(logInfo) "Starting Kontiki"

        DB.withDB "/tmp/kontiki-db" DB.defaultOptions { DB.createIfMissing = True } $ \db -> do
            runPersistentStateT db $
                K.initializePersistentState

            liftIO $ EKG.registerStats db state (EKG.serverMetricStore ekg)

            let electionDelay = return $ sDelay 1
                onElectionTimeout = error "Not implemented"
                heartbeatDelay = return $ msDelay 200
                onHeartbeatTimeout = error "Not implemented"
            timers <- liftIO $ newTimers (electionDelay, onElectionTimeout) (heartbeatDelay, onHeartbeatTimeout)

            let opts = defaultServiceOptions
                impl = S.Node { S.nodeRequestVote = handleRequest logs db timers state nodeRequestVote
                              , S.nodeAppendEntries = nodeAppendEntries
                              }
            liftIO $ S.nodeServer impl opts
