{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Main (main) where

import Control.Monad (forever)

import Network.GRPC.HighLevel.Client
import Network.GRPC.HighLevel.Generated
import Network.GRPC.LowLevel

import Kontiki.Protocol.Server
import Kontiki.Protocol.Server.Instances ()

import Data.Default (def)

main :: IO ()
main = do
    let cfg = ClientConfig (Host "localhost") (Port 50051) [] Nothing
        req = def { requestVoteRequestCandidateId = "kontiki-client", requestVoteRequestTerm = 10 }
    withGRPC $ \g -> withClient g cfg $ \c -> do
        Node{..} <- nodeClient c
        forever $ nodeRequestVote (ClientNormalRequest req 5 mempty) >>= \case
            ClientNormalResponse _resp _ _ StatusOk _ -> return () -- print resp
            ClientNormalResponse _ _ _ st _ -> print st
            ClientError e -> print e
