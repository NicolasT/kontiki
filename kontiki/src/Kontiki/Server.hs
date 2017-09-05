{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Server (main) where

{-
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Monoid ((<>))

import Control.Applicative ((<|>))

import GHC.Conc.Sync (labelThread)

import Control.Concurrent.STM (atomically)

import Control.Concurrent.Async.Lifted.Safe (withAsync)

import Network.Socket (withSocketsDo)

import Control.Exception.Safe (MonadMask, SomeException, finally, withException)

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.ByteString.Char8 as BS8

import Control.Monad.Logger (MonadLogger, logInfo, logInfoSH)

import qualified Database.LevelDB.Base as DB

import qualified System.Remote.Monitoring as EKG

import Control.Concurrent.Suspend (msDelay, sDelay)

import qualified Kontiki.Raft as K

import Kontiki.Server.Logging (Logger)
import qualified Kontiki.Server.Logging as Logging
import Kontiki.Server.GRPC (Server)
import qualified Kontiki.Server.GRPC as GRPC
import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)
import Kontiki.Timers (Timers, newTimers, runTimersT)
import qualified Kontiki.Timers as Timers
-}
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
{-
_startEKG :: (MonadIO m, MonadLogger m) => m EKG.Server
_startEKG = do
    let host = "localhost"
        port = 8000

    $(logInfo) $ "Running EKG at http://" <> Text.pack host <> ":" <> Text.pack (show port)
    liftIO $ do
        server <- EKG.forkServer (BS8.pack host) port
        labelThread (EKG.serverThreadId server) "ekg"
        return server


data Event m = RPC (GRPC.RequestHandler m -> m ()) | Timeout (Timers.TimeoutHandler m -> m ())

handle :: Logger -> Timers -> Server -> IO ()
handle logger timers server = DB.withDB "/tmp/kontiki-db" DB.defaultOptions { DB.createIfMissing = True } $ \db -> do
    _ <- Logging.withLogger logger $ runTimersT timers $ runPersistentStateT db $ flip runStateT state0 $ do
        K.initializePersistentState
        let loop = do
                event <- liftIO $ atomically $  (Timeout <$> Timers.readTimeout timers)
                                            <|> (RPC <$> GRPC.readRequest server)
                case event of
                    RPC fn -> fn rpcHandlers
                    Timeout fn -> fn timeoutHandlers
                loop
        loop
    return ()
  where
    state0 :: K.SomeState VolatileState ()
    state0 = K.initialState
    rpcHandlers = GRPC.RequestHandler { onRequestVote = K.onRequestVoteRequest
                                      , onAppendEntries = \_ -> return undefined
                                      }
    timeoutHandlers = Timers.TimeoutHandler { onElectionTimeout = return ()
                                            , onHeartbeatTimeout = return ()
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
            heartbeatDelay = return $ msDelay 200
        timers <- liftIO $ newTimers electionDelay heartbeatDelay

        stats <- liftIO $ EKG.getDistribution "kontiki.node.rpc" ekg

        let serverMain :: (MonadLogger m, MonadIO m, MonadMask m) => m ()
            serverMain = (liftIO $ GRPC.runServer stats logger server) `withException` (\exc -> $(logInfoSH) ("Exception in server" :: Text, (exc :: SomeException)))
                                                                       `finally` $(logInfo) "GRPC server shut down"
            handlerMain :: (MonadLogger m, MonadIO m, MonadMask m) => m ()
            handlerMain = (liftIO $ handle logger timers server) `withException` (\exc -> $(logInfoSH) ("Exception in handler" :: Text, (exc :: SomeException)))
                                                                 `finally` $(logInfo) "Handler loop shut down"

        _ <- withAsync serverMain $ \_ ->
            withAsync handlerMain $ \_ ->
                liftIO $ forever $ threadDelay (1000 * 1000)

        $(logInfo) "Kontiki exiting..."
-}

import Control.Concurrent (myThreadId, threadDelay)
import Control.Monad (forever, unless)
import Data.Monoid ((<>))
import System.IO (stderr)

import GHC.Conc.Sync (labelThread)

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (get)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.State.Strict (evalStateT)

import Control.Exception.Base (AsyncException(ThreadKilled))
import Control.Exception.Safe (SomeException, bracket, catchAny, displayException, finally, fromException, withException)

import Control.Concurrent.Async.Lifted.Safe (Async, Forall, Pure, link, withAsync)

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Database.LevelDB.Base as DB

import Katip (
    ColorStrategy(ColorIfTerminal), KatipContext, KatipContextT, Namespace(Namespace),
    Severity(DebugS, EmergencyS, InfoS, NoticeS), Verbosity(V2),
    closeScribes, defaultScribeSettings, initLogEnv, katipAddContext, katipAddNamespace, logTM, ls, mkHandleScribe,
    registerScribe, runKatipContextT, showLS, sl)

import Control.Monad.Logger.Katip (KatipLoggingT, runKatipLoggingT)

import qualified Kontiki.Raft as K

import qualified Kontiki.Server.GRPC as GRPC
import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)

type KontikiM = KatipLoggingT KatipContext (KatipContextT IO)

withAsync' :: ( Monad m
              , MonadMask m
              , KatipContext m
              , MonadIO m
              , MonadBaseControl IO m
              , Forall (Pure m)
              )
           => Text
           -> m a
           -> (Async a -> m b)
           -> m b
withAsync' name act cont = flip withAsync cont $ do
    liftIO $ do
        tid <- myThreadId
        labelThread tid (Text.unpack name)

    katipAddNamespace ns $ flip finally logExit
                         $ flip withException logError
                         $ $(logTM) InfoS (name' <> " starting") >> act
  where
    name' = ls $ Text.toTitle name
    ns = Namespace [name]
    logExit = $(logTM) NoticeS (name' <> " quit")
    logError e = unless (isThreadKilled e) $
        $(logTM) EmergencyS $ "An exception occurred: " <> ls (displayException (e :: SomeException))
    isThreadKilled e = fromException e == Just ThreadKilled

mainloop :: KontikiM ()
mainloop = DB.withDB "/tmp/kontiki-db" DB.defaultOptions $ \db -> runPersistentStateT db $ flip evalStateT state0 $ do
    st <- get
    katipAddContext (sl "volatileState" st) $
        forever $ liftIO $ threadDelay 1000000
  where
    state0 = K.initialState :: K.SomeState VolatileState ()


main' :: KontikiM ()
main' = do
    DB.withDB "/tmp/kontiki-db" DB.defaultOptions { DB.createIfMissing = True } $ \db ->
        runPersistentStateT db K.initializePersistentState

    server <- liftIO $ GRPC.mkServer

    withAsync' "node" (liftIO $ GRPC.runServer undefined server) $ \grpc ->
        withAsync' "mainloop" mainloop $ \ml -> do
            link grpc
            link ml
            sleepForever `finally` $(logTM) NoticeS "Exiting"
  where
    sleepForever = liftIO $ forever $ threadDelay (1000 * 1000 {- us -})

main :: IO ()
main = bracket mkLogEnv closeScribes $ \logEnv ->
    runKatipContextT logEnv () "main" $ runKatipLoggingT @KatipContext $ do
        $(logTM) InfoS "Starting kontiki..."
        main' `catchAny` \e -> $(logTM) EmergencyS ("An exception occurred: " <> showLS e)
  where
    mkLogEnv = do
        env <- initLogEnv "kontiki" "production"
        scribe <- mkHandleScribe ColorIfTerminal stderr DebugS V2
        registerScribe "stderr" scribe defaultScribeSettings env
