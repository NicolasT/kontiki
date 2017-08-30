{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Server (main) where

{-
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
-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Monoid ((<>))

import GHC.Conc.Sync (labelThread)

import Network.Socket (withSocketsDo)

import qualified Data.Text as Text

import qualified Data.ByteString.Char8 as BS8

import Control.Monad.Logger (MonadLogger, logInfo)

import qualified Database.LevelDB.Base as DB

import qualified System.Remote.Monitoring as EKG

import Control.Concurrent.Suspend (msDelay, sDelay)

import qualified Kontiki.Raft as K

import Kontiki.Protocol.Server.Instances ()
import Kontiki.Server.Logging (Logger)
import qualified Kontiki.Server.Logging as Logging
import Kontiki.Server.GRPC (Server)
import qualified Kontiki.Server.GRPC as GRPC
import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)
import Kontiki.Timers (Timers, newTimers, runTimersT)

{-
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
-}

startEKG :: (MonadIO m, MonadLogger m) => m EKG.Server
startEKG = do
    let host = "localhost"
        port = 8000

    $(logInfo) $ "Running EKG at http://" <> Text.pack host <> ":" <> Text.pack (show port)
    liftIO $ do
        server <- EKG.forkServer (BS8.pack host) port
        labelThread (EKG.serverThreadId server) "ekg"
        return server


handle :: Logger -> Timers -> Server -> IO ()
handle logger timers server = DB.withDB "/tmp/kontiki-db" DB.defaultOptions { DB.createIfMissing = True } $ \db -> do
    _ <- Logging.withLogger logger $ runTimersT timers $ runPersistentStateT db $ flip runStateT state0 $ do
        GRPC.handleRequests handlers server
    return ()
  where
    state0 :: K.SomeState VolatileState ()
    state0 = K.initialState
    handlers = GRPC.RequestHandler { onRequestVote = K.onRequestVoteRequest
                                   , onAppendEntries = \_ -> return undefined
                                   }

main :: IO ()
main = withSocketsDo $ do
    logger <- Logging.mkLogger
    _logThread <- Logging.spawn logger

    Logging.withLogger logger $ do
        $(logInfo) "Starting Kontiki"
        ekg <- startEKG

        server <- liftIO GRPC.mkServer


        let electionDelay = return $ sDelay 1
            onElectionTimeout = error "Not implemented"
            heartbeatDelay = return $ msDelay 200
            onHeartbeatTimeout = error "Not implemented"
        timers <- liftIO $ newTimers (electionDelay, onElectionTimeout) (heartbeatDelay, onHeartbeatTimeout)

        stats <- liftIO $ EKG.getDistribution "kontiki.node.rpc" ekg

        _ <- liftIO $ forkIO $ handle logger timers server
        _ <- liftIO $ forkIO $ GRPC.runServer stats logger server

        liftIO $ forever $ threadDelay (1000 * 1000)
