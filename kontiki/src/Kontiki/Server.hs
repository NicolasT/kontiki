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

import qualified Data.HashMap.Lazy as Map
import qualified System.Metrics as EKG (Value(Gauge, Label), registerGroup, registerLabel)
import qualified System.Remote.Monitoring as EKG (forkServer, serverMetricStore)

import qualified Kontiki.Raft as K
import qualified Kontiki.Raft.Classes.State.Persistent as K
import qualified Kontiki.Raft.Classes.State.Volatile as K

import qualified Kontiki.Protocol.Server as S
import Kontiki.Protocol.Server.Instances ()
import Kontiki.Types (Node(Node, getNode), Index(getIndex), Term(getTerm))
import Kontiki.State.Persistent (PersistentStateT, runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)

handleRequest :: ( Monad m
                 , MonadIO m
                 , MonadBaseControl IO m
                 )
              => Chan (Loc, LogSource, LogLevel, LogStr)
              -> DB
              -> MVar (K.SomeState VolatileState ())
              -> (req -> StateT (K.SomeState VolatileState ()) (PersistentStateT (LoggingT m)) resp)
              -> req
              -> m resp
handleRequest logs db stateMVar handler = \req -> runChanLoggingT logs $ do
    $(logInfo) "Handling request"
    modifyMVar stateMVar $ \state -> do
        $(logDebugSH) ("Current state" :: Text, state)
        (resp, state') <- runPersistentStateT db $ flip runStateT state $
            handler req
        $(logDebugSH) ("New state" :: Text, state')
        return (state', resp)

nodeRequestVote :: (Monad m, MonadIO m)
                => ServerRequest 'Normal S.RequestVoteRequest S.RequestVoteResponse
                -> StateT (K.SomeState VolatileState ()) (PersistentStateT (LoggingT m)) (ServerResponse 'Normal S.RequestVoteResponse)
nodeRequestVote (ServerNormalRequest _meta req) = do
    $(logDebugSH) ("Request" :: Text, req)
    resp <- K.onRequestVoteRequest req
    $(logDebugSH) ("Response" :: Text, resp)
    return (ServerNormalResponse resp mempty StatusOk "")

nodeAppendEntries :: ServerRequest 'Normal S.AppendEntriesRequest S.AppendEntriesResponse
                  -> IO (ServerResponse 'Normal S.AppendEntriesResponse)
nodeAppendEntries = undefined


mapRole :: K.Role -> Text
mapRole = \case
    K.Follower -> "follower"
    K.Candidate -> "candidate"
    K.Leader -> "leader"

main :: IO ()
main = do
    logs <- newChan
    _ <- forkIO $ runStderrLoggingT $ unChanLoggingT logs

    ekg <- EKG.forkServer "localhost" 8000
    let store = EKG.serverMetricStore ekg

    let state0 = K.initialState :: K.SomeState VolatileState ()
    state <- newMVar state0

    EKG.registerGroup
        (Map.fromList [
              ("kontiki.node.role", EKG.Label . mapRole . K.role)
            , ("kontiki.node.commitIndex", EKG.Gauge . fromIntegral . getIndex . view K.commitIndex . K.volatileState)
            , ("kontiki.node.lastApplied", EKG.Gauge . fromIntegral . getIndex . view K.lastApplied . K.volatileState)
            ])
        (readMVar state)
        store

    runChanLoggingT logs $ do
        $(logInfo) "Starting Kontiki"

        DB.withDB "/tmp/kontiki-db" DB.defaultOptions { DB.createIfMissing = True } $ \db -> do
            runPersistentStateT db $
                K.initializePersistentState

            liftIO $ EKG.registerGroup
                (Map.fromList [
                      ("kontiki.node.votedFor", EKG.Label . maybe "" getNode . fst)
                    , ("kontiki.node.currentTerm", EKG.Gauge . fromIntegral . getTerm . snd)
                    ])
                (runNoLoggingT $ runPersistentStateT db $ do
                    vf <- K.getVotedFor
                    ct <- K.getCurrentTerm
                    return (vf, ct))
                store

            let opts = defaultServiceOptions
                impl = S.Node { S.nodeRequestVote = handleRequest logs db state nodeRequestVote
                              , S.nodeAppendEntries = nodeAppendEntries
                              }
            liftIO $ S.nodeServer impl opts
