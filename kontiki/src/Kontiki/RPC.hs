{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kontiki.RPC (
      RPCT
    , runRPCT
    , Clients
    , withClients
    ) where

import Data.Monoid ((<>))
import Data.Traversable (forM)

import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)

import Control.Exception.Safe (bracket)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader.Class (MonadReader(ask, local))
import Control.Monad.Trans.Class (MonadTrans, lift)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Katip (
    Katip(getLogEnv, localLogEnv), KatipContext(getKatipContext, localKatipContext, getKatipNamespace, localKatipNamespace),
    Severity(ErrorS), logTM, ls)

import qualified Network.GRPC.HighLevel.Client as GRPC
import qualified Network.GRPC.LowLevel as GRPC
import qualified Network.GRPC.LowLevel.Client as GRPC

import Kontiki.Raft.Classes.RPC (MonadRPC(Node, RequestVoteRequest, AppendEntriesRequest, broadcastRequestVoteRequest, sendAppendEntriesRequest))
import qualified Kontiki.Raft.Classes.State.Persistent as P
import qualified Kontiki.Raft.Classes.Timers as T

import qualified Kontiki.Protocol.GRPC.Node as Node
import qualified Kontiki.Protocol.Types as KPT

type Clients = HashMap KPT.Node (Node.Node GRPC.ClientRequest GRPC.ClientResult)

newtype RPCT m a = RPCT { unRPCT :: ReaderT Clients m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadCatch, MonadMask, MonadThrow, MonadLogger)

instance (Monad m, MonadIO m, KatipContext m) => MonadRPC (RPCT m) where
    type Node (RPCT m) = KPT.Node
    type RequestVoteRequest (RPCT m) = KPT.RequestVoteRequest
    type AppendEntriesRequest (RPCT m) = KPT.AppendEntriesRequest

    broadcastRequestVoteRequest req = RPCT $ do
        clients <- ask
        _ <- forM clients $ \client -> liftIO $ do
            _res <- Node.nodeRequestVote client (GRPC.ClientNormalRequest req 5 mempty)
            print "Got something"
        error "Not implemented"
    sendAppendEntriesRequest n req = RPCT $ do
        clients <- ask
        let client = HashMap.lookup n clients
        case client of
            Just client' -> do
                _res <- liftIO $ Node.nodeAppendEntries client' (GRPC.ClientNormalRequest req 5 mempty)
                error "Not implemented"
            Nothing ->
                $(logTM) ErrorS $ ls $ "Unknown node: " <> show n


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

runRPCT :: Clients -> RPCT m a -> m a
runRPCT clients = flip runReaderT clients . unRPCT


withClient :: (MonadIO m, MonadMask m) => GRPC.GRPC -> GRPC.ClientConfig -> (GRPC.Client -> m a) -> m a
withClient grpc config = bracket (liftIO $ GRPC.createClient grpc config) (liftIO . GRPC.destroyClient)

withClients :: (MonadIO m, MonadMask m) => GRPC.GRPC -> HashMap KPT.Node GRPC.ClientConfig -> (Clients -> m a) -> m a
withClients grpc confs cont = loop (HashMap.toList confs) HashMap.empty
  where
    loop [] clients = cont clients
    loop ((n, conf) : confs') clients = withClient grpc conf $ \client -> do
        nc <- liftIO $ Node.nodeClient client
        loop confs' (HashMap.insert n nc clients)
