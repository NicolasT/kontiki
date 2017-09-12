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
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))

import Control.Monad.Catch (MonadCatch, MonadMask)

import Control.Exception.Safe (Exception(displayException), SomeException, tryAny, withException)

import qualified Control.Monad.Metrics as Metrics

import Network.GRPC.HighLevel.Generated (
    GRPCMethodType(Normal), Host, Port, ServerRequest(ServerNormalRequest), ServerResponse(ServerNormalResponse),
    ServiceOptions(logger, serverHost, serverPort),
    StatusCode(StatusAborted, StatusOk), StatusDetails(StatusDetails), defaultServiceOptions)

import Data.Aeson (ToJSON)

import qualified Data.ByteString.Char8 as BS8

import Data.Default (Default, def)

import Katip (KatipContext, Severity(ErrorS, WarningS), katipAddContext, katipAddNamespace, logTM, ls, sl)

import Kontiki.Server.Monad (ServerT, runInIO)
import qualified Kontiki.Protocol.GRPC.Node as Server
import qualified Kontiki.Protocol.Types as T

data Request = RequestVote {-# UNPACK #-} !T.RequestVoteRequest !(MVar (Either SomeException T.RequestVoteResponse))
             | AppendEntries {-# UNPACK #-} !T.AppendEntriesRequest !(MVar (Either SomeException T.AppendEntriesResponse))

data RequestHandler m = RequestHandler { onRequestVote :: T.RequestVoteRequest -> m T.RequestVoteResponse
                                       , onAppendEntries :: T.AppendEntriesRequest -> m T.AppendEntriesResponse
                                       }

readRequest :: (Monad m, MonadIO m, MonadCatch m, KatipContext m) => Server -> STM (RequestHandler m -> m ())
readRequest Server{..} = do
    req <- TBQueue.readTBQueue serverQueue
    return $ \RequestHandler{..} -> case req of
        RequestVote req' mvar -> do
            res <- tryAny (onRequestVote req')
            liftIO $ MVar.putMVar mvar res
            case res of
                Left exc -> $(logTM) WarningS $ "Exception in RequestVote handler: " <> ls (displayException exc)
                Right _ -> return ()
        AppendEntries req' mvar -> do
            res <- tryAny(onAppendEntries req')
            liftIO $ MVar.putMVar mvar res
            case res of
                Left exc -> $(logTM) WarningS $ "Exception in AppendEntries handler: " <> ls (displayException exc)
                Right _ -> return ()

newtype Server = Server { serverQueue :: TBQueue Request }

mkServer :: IO Server
mkServer = Server <$> TBQueue.newTBQueueIO 1024

runServer :: Host -> Port -> Server -> ServerT IO ()
runServer host port server = katipAddNamespace "grpc" $ do

    logger' <- runInIO ($(logTM) WarningS . ls)
    nrv <- runInIO $ handler server RequestVote
    nae <- runInIO $ handler server AppendEntries

    let opts = defaultServiceOptions { serverHost = host
                                     , serverPort = port
                                     , logger = logger'
                                     }
        impl = Server.Node { Server.nodeRequestVote = nrv
                           , Server.nodeAppendEntries = nae
                           , Server.nodePing = handlePing
                           }

    liftIO $ Server.nodeServer impl opts

handlePing :: ServerRequest 'Normal Server.PingRequest Server.PingResponse -> IO (ServerResponse 'Normal Server.PingResponse)
handlePing (ServerNormalRequest _meta Server.PingRequest) =
    return (ServerNormalResponse (Server.PingResponse (T.Node "localhost")) mempty StatusOk mempty)

handler :: ( MonadIO m
           , MonadMask m
           , ToJSON req
           , Default resp
           )
        => Server
        -> (req -> MVar (Either SomeException resp) -> Request)
        -> ServerRequest 'Normal req resp
        -> ServerT m (ServerResponse 'Normal resp)
handler server wrapper (ServerNormalRequest _meta req) =
    katipAddContext (sl "request" req) $ Metrics.timed' Metrics.Milliseconds "kontiki.rpc" $
        flip withException handleException $ liftIO $ do
            resBox <- MVar.newEmptyMVar
            atomically $ TBQueue.writeTBQueue (serverQueue server) (wrapper req resBox)
            res <- MVar.takeMVar resBox
            case res of
                Left err -> return (ServerNormalResponse def mempty StatusAborted (StatusDetails $ BS8.pack $ displayException err))
                Right res' -> return (ServerNormalResponse res' mempty StatusOk "")
  where
    handleException e = $(logTM) ErrorS $ "Exception: " <> ls (displayException (e :: SomeException))
