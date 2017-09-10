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
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (evalStateT)

import System.Environment (getEnv)

import Control.Exception.Safe (bracket, catchAny, finally)

import Control.Concurrent.Async.Lifted.Safe (link)

import qualified Database.LevelDB.Base as DB

import Katip (
    ColorStrategy(ColorIfTerminal), Severity(DebugS, EmergencyS, InfoS, NoticeS), Verbosity(V2),
    closeScribes, defaultScribeSettings, initLogEnv, katipAddContext, logTM, mkHandleScribe,
    registerScribe, showLS, sl)

import qualified System.Metrics as EKG
import qualified Control.Monad.Metrics as Metrics

import System.Random (randomRIO)

import Control.Concurrent.Suspend (msDelay)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text

import qualified Data.HashMap.Strict as HashMap

import Network.GRPC.LowLevel (ClientConfig(ClientConfig), GRPC, Host(Host), Port(Port), withGRPC)

import qualified Control.Concurrent.STM.TBQueue as TBQueue

import qualified Kontiki.Raft.Classes.Timers as K
import qualified Kontiki.Raft as K

import qualified Kontiki.CLI.Config as CLI
import Kontiki.Config (Config(Config, configCluster, configLocalNode), localNode)
import Kontiki.Protocol.Types (Node(Node))
import Kontiki.RPC (runRPCT, withClients)
import qualified Kontiki.RPC as RPC
import qualified Kontiki.Server.EKG as EKG (forkServerWith)
import qualified Kontiki.Server.GRPC as GRPC
import Kontiki.Server.Monad (ServerT, runServerT, withAsync)
import qualified Kontiki.Timers as Timers
import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)

data MainloopEvent m = Timeout (Timers.TimeoutHandler m -> m ())
                     | GRPCRequest (GRPC.RequestHandler m -> m ())
                     | RPCResponse (RPC.ResponseHandler m -> m ())

mainloop :: GRPC -> Config -> GRPC.Server -> Timers.Timers -> ServerT IO ()
mainloop grpc config server timers = do
    queue <- liftIO $ TBQueue.newTBQueueIO 1024
    withClients grpc clients $ \grpcClients ->
        DB.withDB databasePath DB.defaultOptions $ \db ->
              flip runReaderT config
            $ Timers.runTimersT timers
            $ runPersistentStateT db
            $ runRPCT grpcClients queue
            $ evalStateT (K.startElectionTimer >> loop queue) state0
  where
    loop queue = do
        st <- get
        katipAddContext (sl "volatileState" st) $ do
            $(logTM) DebugS "Waiting for event"

            req <- liftIO $ atomically $  Timeout <$> Timers.readTimeout timers
                                      <|> RPCResponse <$> RPC.readResponse queue
                                      <|> GRPCRequest <$> GRPC.readRequest server
            case req of
                Timeout fn -> fn timeoutHandlers
                GRPCRequest fn -> fn grpcHandlers
                RPCResponse fn -> fn rpcHandlers

        loop queue
    state0 = K.initialState :: K.SomeState VolatileState ()
    databasePath = Text.unpack $ CLI.database $ localNode config
    clients = HashMap.fromList $ map
        (\n -> (Node (Text.toStrict $ CLI.name n), ClientConfig (host' n) (Port $ fromIntegral $ CLI.port n) [] Nothing))
        $ filter (\n -> n /= localNode config) (CLI.nodes $ configCluster config)
    host' = Host . LBS.toStrict . Text.encodeUtf8 . CLI.host

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

    rpcHandlers = RPC.ResponseHandler { RPC.onRequestVote = onRequestVoteResponse
                                      , RPC.onAppendEntries = onAppendEntriesResponse
                                      }
    onRequestVoteResponse resp = katipAddContext (sl "requestVote" resp) $ do
        $(logTM) DebugS "Handling RequestVote response"
        K.onRequestVoteResponse resp
    onAppendEntriesResponse resp = katipAddContext (sl "appendEntries" resp) $ do
        $(logTM) DebugS "Handling AppendEntries response"
        K.onAppendEntriesResponse resp


main' :: GRPC -> Config -> EKG.Store -> ServerT IO a
main' grpcToken config store = do
    server <- liftIO GRPC.mkServer

    let mkTimeout ms = do
        let q = ms `div` 4
            low = ms - q
            high = ms + q
        msDelay <$> randomRIO (low, high)

    timers <- liftIO $ Timers.newTimers (mkTimeout 1000) (mkTimeout 500)

    ekgHost <- BS8.pack <$> (liftIO $ getEnv "EKG_HOST")
    ekgPort <- read <$> (liftIO $ getEnv "EKG_PORT")

    EKG.forkServerWith store ekgHost ekgPort $ \_ekg ->
        withAsync "node" (GRPC.runServer grpcHost grpcPort server) $ \grpc -> do
            link grpc

            withAsync "mainloop" (mainloop grpcToken config server timers) $ \ml -> do
                link ml
                sleepForever `finally` $(logTM) NoticeS "Exiting"
  where
    sleepForever = do
        liftIO $ threadDelay (1000 * 1000 {- us -})
        sleepForever
    thisNode = localNode config
    grpcHost = Host $ LBS.toStrict $ Text.encodeUtf8 $ CLI.host thisNode
    grpcPort = Port $ fromIntegral $ CLI.port thisNode

main :: CLI.Config -> Node -> IO ()
main config node = withSocketsDo $ bracket mkLogEnv closeScribes $ \logEnv -> do
        (store, metrics) <- liftIO $ do
            store <- EKG.newStore
            EKG.registerGcMetrics store
            metrics <- Metrics.initializeWith store
            return (store, metrics)

        withGRPC $ \grpc ->
            runServerT metrics logEnv () "main" $ do
                $(logTM) InfoS "Starting kontiki..."
                main' grpc config' store `catchAny` \e -> $(logTM) EmergencyS ("Exception: " <> showLS e)
  where
    mkLogEnv = do
        env <- initLogEnv "kontiki" "production"
        scribe <- mkHandleScribe ColorIfTerminal stderr DebugS V2
        registerScribe "stderr" scribe defaultScribeSettings env
    config' = Config { configLocalNode = node
                     , configCluster = config
                     }
