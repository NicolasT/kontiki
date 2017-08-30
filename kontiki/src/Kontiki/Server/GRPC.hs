{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Kontiki.Server.GRPC (
      Server
    , mkServer
    , runServer
    , RequestHandler(..)
    , handleRequests
    ) where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQueue
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Text (Text)

import Control.Monad.Logger (logDebugSH)

import System.Clock (Clock(Realtime), getTime, toNanoSecs)

import System.Metrics.Distribution (Distribution)
import qualified System.Metrics.Distribution as Distribution

import Network.GRPC.HighLevel.Generated (GRPCMethodType(Normal), ServerRequest(ServerNormalRequest), ServerResponse(ServerNormalResponse), StatusCode(StatusOk), defaultServiceOptions)

import qualified Kontiki.Protocol.Server as Server
import Kontiki.Server.Logging (Logger)
import qualified Kontiki.Server.Logging as Logging

data Request = RequestVote {-# UNPACK #-} !(Server.RequestVoteRequest) !(MVar Server.RequestVoteResponse)
             | AppendEntries {-# UNPACK #-} !(Server.AppendEntriesRequest) !(MVar Server.AppendEntriesResponse)

data RequestHandler m = RequestHandler { onRequestVote :: Server.RequestVoteRequest -> m Server.RequestVoteResponse
                                       , onAppendEntries :: Server.AppendEntriesRequest -> m Server.AppendEntriesResponse
                                       }

handleRequests :: (Monad m, MonadIO m) => RequestHandler m -> Server -> m ()
handleRequests RequestHandler{..} Server{..} = handleOne
  where
    -- Note: Don't try to be 'smart'. Turning this recursive loop into
    -- something using 'Control.Monad.forever' introduces a massive space leak
    handleOne = do
        liftIO (atomically $ TQueue.readTQueue serverQueue) >>= \case
            RequestVote req mvar -> onRequestVote req >>= liftIO . MVar.putMVar mvar
            AppendEntries req mvar -> onAppendEntries req >>= liftIO . MVar.putMVar mvar
        handleOne

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
    -- $(logDebugSH) ("Request" :: Text, req)
    res <- liftIO $ do
        resBox <- MVar.newEmptyMVar
        atomically $ TQueue.writeTQueue (serverQueue server) (wrapper req resBox)
        res <- MVar.takeMVar resBox
        return (ServerNormalResponse res mempty StatusOk "")
    let (ServerNormalResponse res' _ _ _) = res
    -- $(logDebugSH) ("Response" :: Text, res')
    liftIO $ do
        end <- liftIO $ getTime Realtime
        let diff = toNanoSecs end - toNanoSecs start
        Distribution.add stats (fromIntegral diff)
    return res
