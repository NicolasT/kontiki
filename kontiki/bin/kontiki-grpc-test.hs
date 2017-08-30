{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Network.Socket (withSocketsDo)

import Data.Default (def)

import Network.GRPC.HighLevel.Generated

import System.Remote.Monitoring (forkServer)

import qualified Kontiki.Protocol.Server as Server
import Kontiki.Protocol.Server.Instances ()

main :: IO ()
main = withSocketsDo $ do
    _ <- forkServer "localhost" 8000
    let opts = defaultServiceOptions
        impl = Server.Node { Server.nodeRequestVote = handle (\req -> return def)
                           , Server.nodeAppendEntries = handle (\req -> return undefined)
                           }
    Server.nodeServer impl opts
  where
    handle :: (req -> IO resp) -> ServerRequest 'Normal req resp -> IO (ServerResponse 'Normal resp)
    handle h (ServerNormalRequest _meta req) = do
        res <- h req
        return $ ServerNormalResponse res mempty StatusOk ""
