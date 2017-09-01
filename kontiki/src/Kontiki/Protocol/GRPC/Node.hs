{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kontiki.Protocol.GRPC.Node (
      Node(..)
    , nodeServer
    , nodeClient
    ) where

import GHC.Generics (Generic)

import Network.GRPC.HighLevel.Client (Client, ClientRequest, ClientResult, clientRegisterMethod, clientRequest)
import Network.GRPC.HighLevel.Generated (GRPCMethodType(Normal), MethodName(MethodName), ServerRequest, ServerResponse,
    ServiceOptions(ServiceOptions, serverHost, serverPort, useCompression, userAgentPrefix, userAgentSuffix, initialMetadata, sslConfig, logger))
import Network.GRPC.HighLevel.Server (Handler(UnaryHandler),
    ServerOptions(optNormalHandlers, optClientStreamHandlers, optServerStreamHandlers, optBiDiStreamHandlers, optServerHost, optServerPort, optUseCompression, optUserAgentPrefix, optUserAgentSuffix, optInitialMetadata, optSSLConfig, optLogger),
    convertGeneratedServerHandler, defaultOptions, serverLoop)

import Kontiki.Protocol.Types (RequestVoteRequest, RequestVoteResponse, AppendEntriesRequest, AppendEntriesResponse)

data Node request response = Node { nodeRequestVote :: request 'Normal RequestVoteRequest RequestVoteResponse
                                                    -> IO (response 'Normal RequestVoteResponse)
                                  , nodeAppendEntries :: request 'Normal AppendEntriesRequest AppendEntriesResponse
                                                      -> IO (response 'Normal AppendEntriesResponse)
                                  }
    deriving (Generic)

requestVote, appendEntries :: MethodName
requestVote = MethodName "/kontiki.Node/RequestVote"
appendEntries = MethodName "/kontiki.Node/AppendEntries"

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
                     ]

nodeClient :: Client -> IO (Node ClientRequest ClientResult)
nodeClient client = Node <$> (clientRequest client <$> clientRegisterMethod client requestVote)
                         <*> (clientRequest client <$> clientRegisterMethod client appendEntries)
