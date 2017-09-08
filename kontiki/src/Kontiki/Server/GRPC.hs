{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Monoid ((<>))

import Control.Monad.Catch (MonadMask)

import Control.Exception.Safe (Exception(displayException), SomeException, withException)

import qualified Control.Monad.Metrics as Metrics

import Network.GRPC.HighLevel.Generated (GRPCMethodType(Normal), ServerRequest(ServerNormalRequest), ServerResponse(ServerNormalResponse), ServiceOptions(logger), StatusCode(StatusOk), defaultServiceOptions)

import Data.Aeson (ToJSON)

import Katip (Severity(ErrorS, WarningS), katipAddContext, katipAddNamespace, logTM, ls, sl)

import Kontiki.Server.Monad (ServerT, runInIO)
import qualified Kontiki.Protocol.GRPC.Node as Server
import qualified Kontiki.Protocol.Types as T

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

runServer :: Server -> ServerT IO ()
runServer server = katipAddNamespace "grpc" $ do

    logger' <- runInIO ($(logTM) WarningS . ls)
    nrv <- runInIO $ handler server RequestVote
    nae <- runInIO $ handler server AppendEntries

    let opts = defaultServiceOptions { logger = logger' }
        impl = Server.Node { Server.nodeRequestVote = nrv
                           , Server.nodeAppendEntries = nae
                           }

    liftIO $ Server.nodeServer impl opts

handler :: ( MonadIO m
           , MonadMask m
           , ToJSON req
           )
        => Server
        -> (req -> MVar resp -> Request)
        -> ServerRequest 'Normal req resp
        -> ServerT m (ServerResponse 'Normal resp)
handler server wrapper (ServerNormalRequest _meta req) =
    katipAddContext (sl "request" req) $ Metrics.timed' Metrics.Milliseconds "kontiki.rpc" $
        flip withException handleException $ liftIO $ do
            resBox <- MVar.newEmptyMVar
            atomically $ TQueue.writeTQueue (serverQueue server) (wrapper req resBox)
            res <- MVar.takeMVar resBox
            return (ServerNormalResponse res mempty StatusOk "")
  where
    handleException e = $(logTM) ErrorS $ "An exception occurred: " <> ls (displayException (e :: SomeException))
