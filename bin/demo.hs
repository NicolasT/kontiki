{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad

import Data.Monoid

import qualified Data.Set as Set

import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as LBS8

import Control.Concurrent
import Control.Concurrent.STM

import System.Random

import System.Log.Logger

import Network.Kontiki.Raft (Command(..), Config(..), CTimeout(..), Event(..), ETimeout(..), NodeId)
import qualified Network.Kontiki.Raft as R

nodes :: [NodeId]
nodes = ["node0", "node1", "node2"]

data Node = Node { nodeName :: NodeId
                 , nodeChannel :: TChan Event
                 , nodeElectionTID :: Maybe ThreadId
                 , nodeHeartbeatTID :: Maybe ThreadId
                 }

runNode :: Config -> [(NodeId, Node)] -> Node -> IO ()
runNode config others node0 = do
    let (state0, commands) = R.initialize config
    node <- foldM runCommand node0 commands
    loop node (state0 :: R.SomeState ())
  where
    loop node state = do
        evt <- atomically $ readTChan $ nodeChannel node

        debugM name $ name ++ " @ " ++ show state
        debugM name $ name ++ ": " ++ show evt

        let (state', commands) = R.handle config evt state
            oldN = R.stateName state
            newN = R.stateName state'

        debugM name $ name ++ " @ " ++ show state'
        debugM name $ name ++ ": " ++ show commands

        node' <- foldM runCommand node commands
        when (oldN /= newN || R.stateTerm state /= R.stateTerm state') $
            infoM name $
                name ++ ": " ++
                oldN ++ "@" ++ show (R.stateTerm state) ++
                " -> " ++
                newN ++ "@" ++ show (R.stateTerm state')

        loop node' state'

    name :: String
    name = unpack $ nodeName node0

    runCommand :: Node -> Command -> IO Node
    runCommand node command = case command of
        CBroadcast msg -> do
            forM_ others $ \(_, n) -> void $ forkIO $ do
                w <- getStdRandom (randomR (500, 3500))
                threadDelay (w * 1000)
                atomically $ writeTChan (nodeChannel n) (EMessage (nodeName node) msg)
            return node
        CSend rcpt msg -> do
            case lookup rcpt others of
                Nothing -> errorM "" $ name ++ ": unknown rcpt " ++ unpack rcpt
                Just n -> void $ forkIO $ do
                    w <- getStdRandom (randomR (500, 3500))
                    threadDelay (w * 1000)
                    atomically $ writeTChan (nodeChannel n) (EMessage (nodeName node) msg)
            return node
        CResetTimeout t -> case t of
            CTElection (m, n) -> do
                case nodeElectionTID node of
                    Nothing -> return ()
                    Just tid -> killThread tid

                w <- getStdRandom (randomR (m, n))

                tid <- forkIO $ do
                    threadDelay w
                    atomically $ writeTChan (nodeChannel node) (ETimeout ETElection)

                return node { nodeElectionTID = Just tid }

            CTHeartbeat m -> do
                case nodeHeartbeatTID node of
                    Nothing -> return ()
                    Just tid -> killThread tid

                tid <- forkIO $ do
                    threadDelay m
                    atomically $ writeTChan (nodeChannel node) (ETimeout ETHeartbeat)

                return node { nodeHeartbeatTID = Just tid}

        CLog b -> do
            infoM name $ LBS8.unpack
                       $ B.toLazyByteString
                       $ (B.byteString (nodeName node) <> B.byteString ": " <> b)
            return node

main :: IO ()
main = do
    putStrLn $ unlines [ "Note: This code simulates a very bad network with very high"
                       , "      latency and ill-suited election timeout / heartbeat"
                       , "      settings. The algorithm should work better on 'real'"
                       , "      networks!"
                       , "      Also, the current implementation is incomplete: it is"
                       , "      possible multiple nodes think they're Leader, but they"
                       , "      will be leaders for distinct terms, so only one would be"
                       , "      able to reach consensus anyway. Need to implement consensus"
                       , "      requirements for leader extensions, instead of the current"
                       , "      stupid heartbeat-based mechanism."
                       ]

    updateGlobalLogger rootLoggerName (setLevel INFO)

    nodes' <- forM nodes $ \name -> do
        chan <- newTChanIO
        return (name, Node name chan Nothing Nothing)

    forM_ nodes' $ \(name, node) -> do
        let others = filter (\(n, _) -> n /= name) nodes'
            config = Config { configNodeId = name
                            , configNodes = Set.fromList nodes
                            , configElectionTimeout = 3000 * 1000
                            , configHeartbeatTimeout =  1000 * 1000
                            }
        forkIO $ runNode config others node

    forever $ threadDelay $ 1000 * 1000 * 1000
