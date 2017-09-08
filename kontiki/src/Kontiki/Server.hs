{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Server (main) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Data.Monoid ((<>))
import System.IO (stderr)

import Network.Socket (withSocketsDo)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Control.Monad.Trans.State.Strict (evalStateT)

import Control.Exception.Safe (bracket, catchAny, finally)

import Control.Concurrent.Async.Lifted.Safe (link)

import qualified Database.LevelDB.Base as DB

import Katip (
    ColorStrategy(ColorIfTerminal), Severity(DebugS, EmergencyS, InfoS, NoticeS), Verbosity(V2),
    closeScribes, defaultScribeSettings, initLogEnv, katipAddContext, logTM, mkHandleScribe,
    registerScribe, showLS, sl)

import qualified System.Metrics as EKG
import qualified Control.Monad.Metrics as Metrics

import Control.Concurrent.Suspend (msDelay, sDelay)

import qualified Kontiki.Raft as K

import Kontiki.Config (Config(Config, configNode), runConfigT)
import Kontiki.Protocol.Types (Node(Node))
import Kontiki.RPC (runRPCT)
import qualified Kontiki.Server.EKG as EKG (forkServerWith)
import qualified Kontiki.Server.GRPC as GRPC
import Kontiki.Server.Monad (ServerT, runServerT, withAsync)
import qualified Kontiki.Timers as Timers
import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)

data MainloopEvent m = Timeout (Timers.TimeoutHandler m -> m ())
                     | GRPCRequest (GRPC.RequestHandler m -> m ())

mainloop :: Config -> GRPC.Server -> Timers.Timers -> ServerT IO ()
mainloop config server timers = DB.withDB "/tmp/kontiki-db" DB.defaultOptions $ \db ->
                                                                  Timers.runTimersT timers
                                                                $ runPersistentStateT db
                                                                $ runRPCT
                                                                $ runConfigT config
                                                                $ evalStateT loop state0
  where
    loop = do
        st <- get
        katipAddContext (sl "volatileState" st) $ do
            $(logTM) DebugS "Waiting for event"

            req <- liftIO $ atomically $  Timeout <$> Timers.readTimeout timers
                                      <|> GRPCRequest <$> GRPC.readRequest server
            case req of
                Timeout fn -> fn timeoutHandlers
                GRPCRequest fn -> fn grpcHandlers

        loop
    state0 = K.initialState :: K.SomeState VolatileState ()

    grpcHandlers = GRPC.RequestHandler { GRPC.onRequestVote = onRequestVote
                                       , GRPC.onAppendEntries = onAppendEntries
                                       }
    onRequestVote req = katipAddContext (sl "requestVote" req) $ do
        $(logTM) DebugS "Handling RequestVote request"
        K.onRequestVoteRequest req
    onAppendEntries req = katipAddContext (sl "appendEntries" req) $ do
        $(logTM) DebugS "Handling AppendEntries request"
        K.onAppendEntriesRequest req

    timeoutHandlers = Timers.TimeoutHandler { Timers.onElectionTimeout = onElectionTimeout
                                            , Timers.onHeartbeatTimeout = onHeartbeatTimeout
                                            }
    onElectionTimeout = do
        $(logTM) DebugS "Handling election timeout"
        K.onElectionTimeout
    onHeartbeatTimeout = do
        $(logTM) DebugS "Handling heartbeat timeout"
        K.onHeartbeatTimeout


main' :: EKG.Store -> ServerT IO a
main' store = do
    DB.withDB "/tmp/kontiki-db" DB.defaultOptions { DB.createIfMissing = True } $ \db ->
        runPersistentStateT db K.initializePersistentState

    server <- liftIO GRPC.mkServer

    timers <- liftIO $ Timers.newTimers (return $ sDelay 1) (return $ msDelay 500)

    EKG.forkServerWith store "localhost" 8000 $ \_ekg ->
        withAsync "node" (GRPC.runServer server) $ \grpc -> do
            link grpc

            withAsync "mainloop" (mainloop config server timers) $ \ml -> do
                link ml
                sleepForever `finally` $(logTM) NoticeS "Exiting"
  where
    sleepForever = do
        liftIO $ threadDelay (1000 * 1000 {- us -})
        sleepForever
    config = Config { configNode = Node "localhost" }

main :: IO ()
main = withSocketsDo $ bracket mkLogEnv closeScribes $ \logEnv -> do
        (store, metrics) <- liftIO $ do
            store <- EKG.newStore
            EKG.registerGcMetrics store
            metrics <- Metrics.initializeWith store
            return (store, metrics)

        runServerT metrics logEnv () "main" $ do
            $(logTM) InfoS "Starting kontiki..."
            main' store `catchAny` \e -> $(logTM) EmergencyS ("Exception: " <> showLS e)
  where
    mkLogEnv = do
        env <- initLogEnv "kontiki" "production"
        scribe <- mkHandleScribe ColorIfTerminal stderr DebugS V2
        registerScribe "stderr" scribe defaultScribeSettings env
