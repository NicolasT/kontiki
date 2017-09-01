{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kontiki.Server.GRPC (
      Server
    , mkServer
    , runServer
    , RequestHandler(..)
    , readRequest
    ) where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue
import Control.Monad.IO.Class (MonadIO, liftIO)

import System.Clock (Clock(Realtime), getTime, toNanoSecs)

import System.Metrics.Distribution (Distribution)
import qualified System.Metrics.Distribution as Distribution

import Network.GRPC.HighLevel.Generated (GRPCMethodType(Normal), ServerRequest(ServerNormalRequest), ServerResponse(ServerNormalResponse), StatusCode(StatusOk), defaultServiceOptions)

import qualified Kontiki.Protocol.GRPC.Node as Server
import qualified Kontiki.Protocol.Types as T
import Kontiki.Server.Logging (Logger)
import qualified Kontiki.Server.Logging as Logging

data Request = RequestVote {-# UNPACK #-} !T.RequestVoteRequest !(MVar T.RequestVoteResponse)
             | AppendEntries {-# UNPACK #-} !T.AppendEntriesRequest !(MVar T.AppendEntriesResponse)

data RequestHandler m = RequestHandler { onRequestVote :: T.RequestVoteRequest -> m T.RequestVoteResponse
                                       , onAppendEntries :: T.AppendEntriesRequest -> m T.AppendEntriesResponse
                                       }

readRequest :: (Monad m, MonadIO m) => Server -> STM (RequestHandler m -> m ())
readRequest Server{..} = do
    req <- TQueue.readTQueue serverQueue
    return $ \RequestHandler{..} -> case req of
        RequestVote req' mvar -> onRequestVote req' >>= liftIO . MVar.putMVar mvar
        AppendEntries req' mvar -> onAppendEntries req' >>= liftIO . MVar.putMVar mvar

newtype Server = Server { serverQueue :: TQueue Request }

mkServer :: IO Server
mkServer = Server <$> TQueue.newTQueueIO

runServer :: Distribution -> Logger -> Server -> IO ()
runServer stats logger server = Server.nodeServer impl opts
  where
    opts = defaultServiceOptions
    impl = Server.Node { Server.nodeRequestVote = handler stats logger server RequestVote
                       , Server.nodeAppendEntries = handler stats logger server AppendEntries
                       }

handler :: ( MonadIO m
           , Show req
           , Show resp
           )
        => Distribution
        -> Logger
        -> Server
        -> (req -> MVar resp -> Request)
        -> ServerRequest 'Normal req resp
        -> m (ServerResponse 'Normal resp)
handler stats logger server wrapper (ServerNormalRequest _meta req) = Logging.withLogger logger $ do
    start <- liftIO $ getTime Realtime
    res <- liftIO $ do
        resBox <- MVar.newEmptyMVar
        atomically $ TQueue.writeTQueue (serverQueue server) (wrapper req resBox)
        res <- MVar.takeMVar resBox
        return (ServerNormalResponse res mempty StatusOk "")
    liftIO $ do
        end <- liftIO $ getTime Realtime
        let diff = toNanoSecs end - toNanoSecs start
        Distribution.add stats (fromIntegral diff)
    return res
