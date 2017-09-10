{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kontiki.RPC (
      RPCT
    , runRPCT
    , Clients
    , withClients
    , ResponseHandler(..)
    , readResponse
    ) where

import Data.Monoid ((<>))
import Control.Monad (forM_, void)

import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)

import Control.Exception.Safe (bracket)

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader.Class (MonadReader(ask, local))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    ComposeSt,
    MonadBaseControl(StM, liftBaseWith, restoreM) , defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(StT, liftWith, restoreT), defaultLiftWith, defaultRestoreT)

import Control.Concurrent.Async.Lifted.Safe (Forall, Pure, async)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Control.Exception.Safe (displayException, tryAny)

import Katip (
    Katip(getLogEnv, localLogEnv), KatipContext(getKatipContext, localKatipContext, getKatipNamespace, localKatipNamespace),
    Severity(DebugS, ErrorS, WarningS), logTM, ls, showLS)

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue

import qualified Network.GRPC.HighLevel.Client as GRPC
import qualified Network.GRPC.LowLevel as GRPC
import qualified Network.GRPC.LowLevel.Client as GRPC

import Kontiki.Raft.Classes.RPC (MonadRPC(Node, RequestVoteRequest, AppendEntriesRequest, broadcastRequestVoteRequest, sendAppendEntriesRequest))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import qualified Kontiki.Raft.Classes.Timers as T

import qualified Kontiki.Protocol.GRPC.Node as Node
import qualified Kontiki.Protocol.Types as KPT

type Clients = HashMap KPT.Node (Node.Node GRPC.ClientRequest GRPC.ClientResult)

newtype RPCT m a = RPCT { unRPCT :: ReaderT (Clients, TBQueue Response) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadTrans, MonadCatch, MonadMask, MonadThrow, MonadLogger)

data Response = RequestVote {-# UNPACK #-} !KPT.RequestVoteResponse
              | AppendEntries {-# UNPACK #-} !KPT.AppendEntriesResponse

data ResponseHandler m = ResponseHandler { onRequestVote :: KPT.RequestVoteResponse -> m ()
                                         , onAppendEntries :: KPT.AppendEntriesResponse -> m ()
                                         }

readResponse :: (Monad m, MonadIO m, MonadCatch m, KatipContext m) => TBQueue Response -> STM (ResponseHandler m -> m ())
readResponse queue = do
    resp <- TBQueue.readTBQueue queue
    return $ \ResponseHandler{..} -> case resp of
        RequestVote resp' -> do
            res <- tryAny (onRequestVote resp')
            case res of
                Left exc -> $(logTM) WarningS $ "Exception in RequestVote response handler: " <> ls (displayException exc)
                Right _ -> return ()
        AppendEntries resp' -> do
            res <- tryAny(onAppendEntries resp')
            case res of
                Left exc -> $(logTM) WarningS $ "Exception in AppendEntries response handler: " <> ls (displayException exc)
                Right _ -> return ()

instance (Monad m, MonadIO m, KatipContext m, MonadBaseControl IO (ReaderT (Clients, TBQueue Response) m), Forall (Pure (ReaderT (Clients, TBQueue Response) m))) => MonadRPC (RPCT m) where
    type Node (RPCT m) = KPT.Node
    type RequestVoteRequest (RPCT m) = KPT.RequestVoteRequest
    type AppendEntriesRequest (RPCT m) = KPT.AppendEntriesRequest

    broadcastRequestVoteRequest req = RPCT $ do
        (clients, queue) <- ask
        forM_ clients $ \client -> async $ do
            $(logTM) DebugS $ "Sending request: " <> showLS req
            liftIO (Node.nodeRequestVote client (GRPC.ClientNormalRequest req 1 mempty)) >>= \case
                GRPC.ClientNormalResponse resp _ _ GRPC.StatusOk _ -> do
                    $(logTM) DebugS $ "Response: " <> showLS resp
                    liftIO $ atomically $ TBQueue.writeTBQueue queue (RequestVote resp)
                GRPC.ClientNormalResponse _ _ _ status _ -> do
                    $(logTM) ErrorS $ "Response status: " <> showLS status
                GRPC.ClientError e -> do
                    $(logTM) ErrorS $ "Client error: " <> showLS e
    sendAppendEntriesRequest n req = RPCT $ do
        (clients, queue) <- ask
        let client = HashMap.lookup n clients
        case client of
            Just client' -> void $ async $ do
                liftIO (Node.nodeAppendEntries client' (GRPC.ClientNormalRequest req 5 mempty)) >>= \case
                    GRPC.ClientNormalResponse resp _ _ GRPC.StatusOk _ -> do
                        $(logTM) DebugS $ "Response: " <> showLS resp
                        liftIO $ atomically $ TBQueue.writeTBQueue queue (AppendEntries resp)
                    GRPC.ClientNormalResponse _ _ _ status _ -> do
                        $(logTM) ErrorS $ "Response status: " <> showLS status
                    GRPC.ClientError e -> do
                        $(logTM) ErrorS $ "Client error: " <> showLS e
            Nothing ->
                $(logTM) ErrorS $ ls $ "Unknown node: " <> show n

instance MonadTransControl RPCT where
    type StT RPCT a = StT (ReaderT (Clients, TBQueue Response)) a
    liftWith = defaultLiftWith RPCT unRPCT
    restoreT = defaultRestoreT RPCT

instance MonadBaseControl b m => MonadBaseControl b (RPCT m) where
    type StM (RPCT m) a = ComposeSt RPCT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance Katip m => Katip (RPCT m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapRPCT . localLogEnv

instance KatipContext m => KatipContext (RPCT m) where
    getKatipContext = lift getKatipContext
    localKatipContext = mapRPCT . localKatipContext
    getKatipNamespace = lift getKatipNamespace
    localKatipNamespace = mapRPCT . localKatipNamespace

instance MonadReader r m => MonadReader r (RPCT m) where
    ask = lift ask
    local = mapRPCT . local

instance (Monad m, P.MonadPersistentState m) => P.MonadPersistentState (RPCT m) where
    type Term (RPCT m) = P.Term m
    type Node (RPCT m) = P.Node m
    type Entry (RPCT m) = P.Entry m
    type Index (RPCT m) = P.Index m

instance (Monad m, T.MonadTimers m) => T.MonadTimers (RPCT m)

mapRPCT :: (m a -> n b) -> RPCT m a -> RPCT n b
mapRPCT f (RPCT m) = RPCT $ mapReaderT f m

runRPCT :: Clients -> TBQueue Response -> RPCT m a -> m a
runRPCT clients queue = flip runReaderT (clients, queue) . unRPCT


withClient :: (MonadIO m, MonadMask m) => GRPC.GRPC -> GRPC.ClientConfig -> (GRPC.Client -> m a) -> m a
withClient grpc config = bracket (liftIO $ GRPC.createClient grpc config) (liftIO . GRPC.destroyClient)

withClients :: (MonadIO m, MonadMask m, KatipContext m) => GRPC.GRPC -> HashMap KPT.Node GRPC.ClientConfig -> (Clients -> m a) -> m a
withClients grpc confs cont = loop (HashMap.toList confs) HashMap.empty
  where
    loop [] clients = cont clients
    loop ((n, conf) : confs') clients = do
        let n' = KPT.getNode n
            GRPC.Host host = GRPC.clientServerHost conf
            GRPC.Port port = GRPC.clientServerPort conf
        $(logTM) DebugS $ "Connecting to " <> ls n' <> " on " <> ls host <> ":" <> showLS port
        withClient grpc conf $ \client -> do
            nc <- liftIO $ Node.nodeClient client
            loop confs' (HashMap.insert n nc clients)
