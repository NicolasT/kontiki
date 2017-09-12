{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.DeepSeq (NFData(rnf))

import Control.Exception.Safe (SomeException, withException)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)

import Network.GRPC.LowLevel (Host(Host), Port(Port), StatusCode(StatusOk))
import Network.GRPC.LowLevel.Client (Client, ClientConfig(ClientConfig), createClient, destroyClient)
import Network.GRPC.HighLevel.Client (
    ClientRequest(ClientNormalRequest),
    ClientResult(ClientNormalResponse, ClientError))
import Network.GRPC.Unsafe (grpcInit, grpcShutdown)

import qualified Control.Monad.Metrics as Metrics

import Katip (initLogEnv)

import Criterion (Benchmark, bench, bgroup, envWithCleanup, whnfIO)
import Criterion.Main (defaultMain)

import Kontiki.Protocol.GRPC.Node (Node, PingRequest(PingRequest), nodeClient, nodePing)
import Kontiki.Server.GRPC (mkServer, runServer)
import Kontiki.Server.Monad (runServerT)

instance NFData Client where
    rnf c = c `seq` ()
instance NFData (Node ClientRequest ClientResult)

instance NFData (Async a) where
    rnf a = a `seq` ()

benchGRPC :: Benchmark
benchGRPC = envWithCleanup mkEnv cleanupEnv $ \ ~(_, _, client) -> bgroup "GRPC" [
      bench "ping" $ whnfIO $ nodePing client (ClientNormalRequest PingRequest 1 mempty)
    ]
  where
    mkEnv = do
        grpcInit
        let grpc = undefined
        let clientConfig = ClientConfig (Host "localhost") (Port 50051) [] Nothing
        client <- createClient grpc clientConfig
        node <- nodeClient client

        server <- mkServer
        metrics <- Metrics.initialize
        env <- initLogEnv "kontiki-bench" "bench"
        serverThread <- async $ withException (runServerT metrics env () "GRPC" $
            runServer "localhost" 50051 server) (\e -> print (e :: SomeException))

        -- Wait for server to run
        threadDelay (1000 * 1000)
        {-ClientNormalResponse _ _ _ StatusOk _ -}
        resp <- nodePing node (ClientNormalRequest PingRequest 1 mempty)
        case resp of
            ClientNormalResponse _ _ _ StatusOk _ -> return ()
            ClientNormalResponse _ _ _ st _ -> error (show st)
            ClientError e -> error (show e)

        return (client, serverThread, node)

    cleanupEnv (client, serverThread, _) = do
        destroyClient client
        cancel serverThread
        grpcShutdown

main :: IO ()
main = defaultMain [
      benchGRPC
    ]
