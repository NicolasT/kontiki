{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kontiki.Protocol.GRPC.Node (
      Node(..)
    , nodeServer
    , nodeClient
    , PingRequest(..)
    , PingResponse(..)
    ) where

import GHC.Generics (Generic)

import Proto3.Suite.Class (Message)

import Network.GRPC.HighLevel.Client (Client, ClientRequest, ClientResult, clientRegisterMethod, clientRequest)
import Network.GRPC.HighLevel.Generated (GRPCMethodType(Normal), MethodName(MethodName), ServerRequest, ServerResponse,
    ServiceOptions(ServiceOptions, serverHost, serverPort, useCompression, userAgentPrefix, userAgentSuffix, initialMetadata, sslConfig, logger))
import Network.GRPC.HighLevel.Server (Handler(UnaryHandler),
    ServerOptions(optNormalHandlers, optClientStreamHandlers, optServerStreamHandlers, optBiDiStreamHandlers, optServerHost, optServerPort, optUseCompression, optUserAgentPrefix, optUserAgentSuffix, optInitialMetadata, optSSLConfig, optLogger),
    convertGeneratedServerHandler, defaultOptions)
import Network.GRPC.HighLevel.Server.Unregistered (serverLoop)

import Kontiki.Protocol.Types (RequestVoteRequest, RequestVoteResponse, AppendEntriesRequest, AppendEntriesResponse)
import qualified Kontiki.Protocol.Types as T

data PingRequest = PingRequest
    deriving (Show, Eq, Generic)
instance Message PingRequest
newtype PingResponse = PingResponse T.Node
    deriving (Show, Eq, Generic)
instance Message PingResponse

data Node request response = Node { nodeRequestVote :: request 'Normal RequestVoteRequest RequestVoteResponse
                                                    -> IO (response 'Normal RequestVoteResponse)
                                  , nodeAppendEntries :: request 'Normal AppendEntriesRequest AppendEntriesResponse
                                                      -> IO (response 'Normal AppendEntriesResponse)
                                  , nodePing :: request 'Normal PingRequest PingResponse
                                             -> IO (response 'Normal PingResponse)
                                  }
    deriving (Generic)

requestVote, appendEntries, ping :: MethodName
requestVote = MethodName "/kontiki.Node/RequestVote"
appendEntries = MethodName "/kontiki.Node/AppendEntries"
ping = MethodName "/kontiki.Node/Ping"

nodeServer :: Node ServerRequest ServerResponse -> ServiceOptions -> IO ()
nodeServer Node{..} ServiceOptions{..} = serverLoop options
  where
    options = defaultOptions { optNormalHandlers = normalHandlers
                             , optClientStreamHandlers = []
                             , optServerStreamHandlers = []
                             , optBiDiStreamHandlers = []
                             , optServerHost = serverHost
                             , optServerPort = serverPort
                             , optUseCompression = useCompression
                             , optUserAgentPrefix = userAgentPrefix
                             , optUserAgentSuffix = userAgentSuffix
                             , optInitialMetadata = initialMetadata
                             , optSSLConfig = sslConfig
                             , optLogger = logger
                             }
    normalHandlers = [ UnaryHandler requestVote (convertGeneratedServerHandler nodeRequestVote)
                     , UnaryHandler appendEntries (convertGeneratedServerHandler nodeAppendEntries)
                     , UnaryHandler ping (convertGeneratedServerHandler nodePing)
                     ]

nodeClient :: Client -> IO (Node ClientRequest ClientResult)
nodeClient client = Node <$> (clientRequest client <$> clientRegisterMethod client requestVote)
                         <*> (clientRequest client <$> clientRegisterMethod client appendEntries)
                         <*> (clientRequest client <$> clientRegisterMethod client ping)
