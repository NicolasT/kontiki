{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables,
             GADTs,
             MultiWayIf #-}
module Main (main) where

import Control.Monad (foldM, forM, void)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.IntMap as IntMap

import qualified Data.Set as Set

import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Lazy.Builder as Builder

import System.Environment (getArgs)
import System.Random (randomRIO)

import Network.Socket (SockAddr(SockAddrInet), inet_addr)

import Control.Lens hiding (Index)

import qualified Data.Binary as B

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network.UDP as CNU
import qualified Data.Streaming.Network as SN
import qualified Data.Streaming.Network.Internal as SNI

import Network.Kontiki.Raft
    ( Command(..), Config(..), Entry(..), Event(..),
      Message, NodeId, SomeState, Index, index0, succIndex, unIndex)
import qualified Network.Kontiki.Raft as Raft

import Data.Kontiki.MemLog (Log, runMemLog)
import qualified Data.Kontiki.MemLog as MemLog

import Data.STM.RollingQueue (RollingQueue)
import qualified Data.STM.RollingQueue as RollingQueue

import qualified Data.Conduit.RollingQueue as CRQ

import Control.STM.Timer (Timer)
import qualified Control.STM.Timer as Timer

type Value = (NodeId, Int)

nodes :: Map NodeId ((String, Int))
nodes = Map.fromList [ ("node0", ("127.0.0.1", 4000))
                     , ("node1", ("127.0.0.1", 4001))
                     , ("node2", ("127.0.0.1", 4002))
                     ]

config :: Config
config = Config { _configNodeId = "unknown"
                , _configNodes = Set.fromList $ Map.keys nodes
                , _configElectionTimeout = 10000 * 1000
                , _configHeartbeatTimeout = 5000 * 1000
                }

data PlumbingState = PS { psElectionTimer :: Timer
                        , psHeartbeatTimer :: Timer
                        , psMessages :: RollingQueue (Event Value)
                        , psChannels :: Map NodeId (RollingQueue (Message Value))
                        , psLog :: Log Value
                        , psCommitIndex :: Index
                        }

queueSize :: Int
queueSize = 1024

newPlumbingState :: Map NodeId (RollingQueue (Message Value)) -> IO PlumbingState
newPlumbingState channels = do
    et <- Timer.newIO
    ht <- Timer.newIO
    q <- RollingQueue.newIO queueSize

    return $ PS { psElectionTimer = et
                , psHeartbeatTimer = ht
                , psMessages = q
                , psChannels = channels
                , psLog = MemLog.empty
                , psCommitIndex = index0
                }

handleCommand :: PlumbingState -> Command Value -> IO PlumbingState
handleCommand s c = case c of
    CBroadcast m -> do
        putStrLn $ "CBroadcast: " ++ show m
        atomically $ mapM_ (flip RollingQueue.write m) (Map.elems $ psChannels s)
        return s
    CSend n m -> do
        putStrLn $ "CSend: " ++ show n ++ " -> " ++ show m
        case Map.lookup n (psChannels s) of
            Nothing -> putStrLn "Unknown node?!"
            Just q -> atomically $ RollingQueue.write q m
        return s
    CResetElectionTimeout a b -> do
        t <- randomRIO (a, b)
        putStrLn $ "Reset election timeout: " ++ show t
        Timer.reset (psElectionTimer s) t
        return s
    CResetHeartbeatTimeout a -> do
        putStrLn $ "Reset heartbeat timeout: " ++ show a
        Timer.reset (psHeartbeatTimer s) a
        return s
    CLog b -> do
        let m = LBS8.unpack $ Builder.toLazyByteString b
        putStrLn $ "Log: " ++ m
        return s
    CTruncateLog i -> do
        putStrLn $ "Truncate: " ++ show i
        let l = psLog s
            i' = fromIntegral $ unIndex i
            l' = IntMap.filterWithKey (\k _ -> k <= i') l
        return $ s { psLog = l' }
    CLogEntries es -> do
        putStrLn $ "Log entries: " ++ show es
        let l = psLog s
            l' = foldr (\e -> MemLog.insert (fromIntegral $ unIndex $ eIndex e) e) l es
        return $ s { psLog = l' }
    CSetCommitIndex i' -> do
        let i = psCommitIndex s
        putStrLn $ "New commit index, to commit: " ++ entriesToCommit i i'
        return $ s { psCommitIndex = i' }

entriesToCommit :: Index -> Index -> String
entriesToCommit prev new =
    if | new < prev  -> error "Committed entries could not be reverted"
       | new == prev -> "nothing"
       | new == next -> "entry " ++ show new
       | otherwise   -> "entries " ++ show next ++ " to " ++ show new
  where
    next = succIndex prev

handleCommands :: PlumbingState -> [Command Value] -> IO PlumbingState
handleCommands = foldM handleCommand

run :: Config -> SomeState -> PlumbingState -> IO ()
run config' s ps = do
    putStrLn "Awaiting event"
    event <- atomically $ do
                (const EElectionTimeout `fmap` Timer.await (psElectionTimer ps))
                    `orElse` (const EHeartbeatTimeout `fmap` Timer.await (psHeartbeatTimer ps))
                    `orElse` (fst `fmap` RollingQueue.read (psMessages ps))
    putStrLn $ "Got event: " ++ show (event :: Event Value)

    putStrLn $ "Input state: " ++ show s
    let (s', cs) = runMemLog (Raft.handle config' s event) (psLog ps)
    putStrLn $ "Output state: " ++ show s'
    ps' <- handleCommands ps cs

    ps'' <- case s' of
        Raft.WrapState (Raft.Leader ls) -> do
            let l = psLog ps'
                size = IntMap.size l
                (_, m) = if size /= 0
                             then IntMap.findMax l
                             else (0, Entry { eTerm = Raft.term0
                                            , eIndex = Raft.index0
                                            , eValue = (config' ^. Raft.configNodeId, 0)
                                            })
                e = Entry { eTerm = ls ^. Raft.lCurrentTerm
                          , eIndex = Raft.succIndex (eIndex m)
                          , eValue = (config' ^. Raft.configNodeId, size)
                          }
                l' = IntMap.insert (fromIntegral $ unIndex $ eIndex e) e l
            return $ ps' { psLog = l' }
        _ -> return ps'

    putStrLn $ "Log: " ++ show (map (\(k, v) -> (k, eValue v)) $ IntMap.toAscList $ psLog ps'')

    run config' s' ps''

makeChannel :: NodeId -> String -> Int -> IO (RollingQueue (Message Value))
makeChannel nn h p = do
    (sock, _) <- SN.getSocketUDP h p
    q <- RollingQueue.newIO queueSize
    addr <- inet_addr h
    let r = SockAddrInet (toEnum p) addr

    void $ forkIO $ do
        CRQ.sourceRollingQueue q $=  CL.map (\m -> (nn, m))
                                 =$= CL.map B.encode
                                 =$= CL.map LBS.toStrict
                                 =$= CL.map (\m -> CNU.Message { CNU.msgData = m
                                                               , CNU.msgSender = r
                                                               })
                                 $$  CNU.sinkToSocket sock

    return q

main :: IO ()
main = do
    [self] <- map BS8.pack `fmap` getArgs

    let config' = config { _configNodeId = self }

    let s0 = Raft.initialState
        (s, cs) = Raft.restore config' s0
        others = Map.filterWithKey (\k _ -> k /= self) nodes

    chans <- forM (Map.toList others) $ \(n, (h, p)) -> makeChannel self h p >>= \c -> return (n, c)

    ps <- newPlumbingState (Map.fromList chans)

    let (_, p) = (Map.!) nodes self
    sock <- SN.bindPortUDP (toEnum p) SNI.HostIPv4
    void $ forkIO $ do
        CNU.sourceSocket sock 4096 $=  CL.map (B.decode . LBS.fromStrict . CNU.msgData)
                                   =$= CL.map (uncurry EMessage)
                                   $$  CRQ.sinkRollingQueue (psMessages ps)

    -- TODO Handle resubmit
    ps' <- handleCommands ps cs

    run config' s ps'
