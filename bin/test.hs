{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Raft
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- This module has all the unit tests as well as cluster simulation tests.
-----------------------------------------------------------------------------
module Main where

import           Control.Applicative
import           Control.Lens                         hiding (Index, elements,
                                                       within)
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.State            (StateT, evalStateT,
                                                       execStateT, get)
import           Data.Binary                          (Binary, decode, encode)
import qualified Data.IntMap                          as IntMap (filterWithKey)
import           Data.Kontiki.MemLog                  as MemLog (Log, empty,
                                                                 insert,
                                                                 runMemLog)
import qualified Data.List                            as List (all, filter,
                                                               head, length,
                                                               map, null, tail)
import qualified Data.Map                             as Map (Map (), adjust,
                                                              assocs, elems,
                                                              fromSet, keys,
                                                              notMember, size)
import qualified Data.Set                             as Set (fromList, toList)
import           Network.Kontiki.Raft                 hiding (setCommitIndex,
                                                       truncateLog)

import           Data.Maybe                           (catMaybes, listToMaybe,
                                                       mapMaybe)
import           System.IO.Unsafe                     (unsafePerformIO)
import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic              (PropertyM, monadic, pick,
                                                       run)
main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
      testGroup "Serialization" [
          testGroup "Messages" [
              testProperty "Message Int" (prop_serialization :: Message Int -> Bool)
            , testProperty "RequestVote" (prop_serialization :: RequestVote -> Bool)
            , testProperty "RequestVoteResponse" (prop_serialization :: RequestVoteResponse -> Bool)
            , testProperty "AppendEntries Int" (prop_serialization :: AppendEntries Int -> Bool)
            , testProperty "AppendEntriesResponse" (prop_serialization :: AppendEntriesResponse -> Bool)
            ]
        , testGroup "State" [
              testProperty "SomeState" (prop_serialization :: SomeState -> Bool)
            , testProperty "Follower" (prop_serialization :: Follower -> Bool)
            , testProperty "Candidate" (prop_serialization :: Candidate -> Bool)
            , testProperty "Leader" (prop_serialization :: Leader -> Bool)
            ]
        , testProperty "Entry Int" (prop_serialization :: Entry Int -> Bool)
        ]
      , testProperty "handle" prop_handle
      , testGroup "Cluster simulations" [
              testProperty "prop_leaderElectionFromCold" prop_leaderElectionFromCold
      ]
    ]

prop_serialization :: (Eq a, Binary a) => a -> Bool
prop_serialization a = decode (encode a) == a

-- A stub MonadLog which, well... has no log at all
newtype Stub a = Stub { unStub :: Identity a }
  deriving ( Functor
           , Monad
           )

instance MonadLog Stub Int where
    logEntry _ = return Nothing
    logLastEntry = return Nothing

-- Make sure there are no obvious 'error' cases in the FSM
prop_handle :: Config -> SomeState -> Event Int -> Bool
prop_handle cfg s event =
    let (state', commands) = runIdentity $ unStub $ handle cfg s event in
    state' `seq` commands `seq` True

-----------------------------------------------------------------------------
-- Cluster simulations.
-----------------------------------------------------------------------------

-- | 1 minute
timeoutMicros :: Int
timeoutMicros = fromInteger $ ((10::Integer) ^ (6::Integer)) * 60

-- | Runs `leaderElectionFromCold' property.
prop_leaderElectionFromCold :: Property
prop_leaderElectionFromCold = within timeoutMicros . simulation $ kickstartSimulation >> waitForLeader

-----------------------------------------------------------------------------
-- Cluster simulator.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Data structure definitions.
-----------------------------------------------------------------------------

-- | Underlying type of log entries
type EventType = Int

-- | Possible events in the simulation
data SimulationEvent
      -- | Indicates that this node should be killed
    = KillNode NodeId

      -- | Indicates that this node should be brought back
    | ResurrectNode NodeId

      -- | Indicates heartbeat timeout for this node
    | HeartbeatTimeout NodeId

      -- | Indicates election timeout for this node
    | ElectionTimeout NodeId

      -- | Indicates that the queue of this node should be executed
    | ExecuteQueue NodeId
  deriving (Show, Eq)

-- | Possible status of a node
data NodeStatus = Up   -- ^ Indicates that the node is online
                | Down -- ^ Indicates that the node is offline
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary NodeStatus where
    arbitrary = arbitraryBoundedEnum

-- | State of a single node in the simulation
data NodeState = NodeState {

    -- | Config of this node
    _nsConfig :: Config

    -- | Status of this node
  , _nsStatus :: NodeStatus

    -- | State of the state machine
  , _nsState  :: SomeState

    -- | Log of this node
  , _nsLog    :: MemLog.Log EventType

    -- | Commit index of this node
  , _nsCommitIndex :: Index

    -- | Queue of unprocessed events
  , _nsQueue  :: [Event EventType]

    -- | If true, the election timeout was set
  , _nsElectionTimeout :: Bool

    -- | If true, the heartbeat timeout was set
  , _nsHeartbeatTimeout :: Bool
  } deriving (Show, Eq)

instance Arbitrary NodeState where
    arbitrary = do
        ns <- arbitraryNodeSet
        nid <- elements $ Set.toList ns
        arbitraryNodeState nid ns


-- | `Gen' for `NodeState' (the log is empty).
arbitraryNodeState :: NodeId -> NodeSet -> Gen NodeState
arbitraryNodeState nid ns = NodeState <$>
                                          (Config nid ns <$> arbitrary
                                                         <*> arbitrary)
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> pure MemLog.empty
                                      <*> pure index0
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary

-- | `Gen' for `NodeSet'.
arbitraryNodeSet :: Gen NodeSet
arbitraryNodeSet = Set.fromList <$> listOf1 arbitraryBS

-- | State of the cluster
type ClusterState = Map.Map NodeId NodeState

-- | `Gen' for `ClusterState'.
arbitraryClusterState :: Gen ClusterState
arbitraryClusterState = arbitrary

instance Arbitrary ClusterState where
    arbitrary = do
        ns <- arbitraryNodeSet
        return $ initialCluster ns

-- | Simulation monad.
type SState m = StateT ClusterState m

-----------------------------------------------------------------------------
-- Cluster management functions.
-----------------------------------------------------------------------------

-- | Creates initial `ClusterState' from this `NodeSet'.
--
-- The _nsElectionTimeout is set to True for all nodes
-- otherwise we cannot start the simulation correctly.
initialCluster :: NodeSet -> ClusterState
initialCluster ns = Map.fromSet toEntries ns

  where config node = Config { _configNodeId = node
                             , _configNodes = ns
                             , _configElectionTimeout = 0
                             , _configHeartbeatTimeout = 0
                             }
        toEntries node = NodeState { _nsConfig = config node
                                   , _nsStatus = Up
                                   , _nsState  = initialState
                                   , _nsLog    = MemLog.empty
                                   , _nsCommitIndex = index0
                                   , _nsQueue  = []
                                   , _nsHeartbeatTimeout = False
                                   , _nsElectionTimeout  = True
                                   }

-- | Checks if `sGen' applied to the state produces the same result
-- as evaluating `csGen' on the same initial state.
withValid :: (NodeId -> SState Identity a) -> (NodeState -> NodeState) -> Property
withValid sGen csGen = do
    cs <- arbitraryClusterState
    nodeId <- oneof [suchThat arbitraryBS $ flip Map.notMember cs, elements $ Map.keys cs]
    let lhs = Map.adjust csGen nodeId cs
        rhs = runIdentity $ execStateT (sGen nodeId) cs
    property $ lhs == rhs

-- | Changes the `_nStatus' to `Down', clears the `_nQueue'
-- and switches off timeouts.
killNode :: NodeState -> NodeState
killNode ns = ns { _nsStatus = Down
                 , _nsQueue = []
                 , _nsElectionTimeout = False
                 , _nsHeartbeatTimeout = False
                 }

-- | Resets the node status to `Up', state to `initialState`
-- clears the queue and resets timeouts.
resurrect :: NodeState -> NodeState
resurrect ns = ns { _nsStatus = Up
                  , _nsState = initialState
                  , _nsQueue = []
                  , _nsElectionTimeout = True
                  , _nsHeartbeatTimeout = False
                  }

-- | Appends `EHeartbeatTimeout' to queue if node has setup the timeout
-- and resets the timeout if so.
appendHearbeatTimeout :: NodeState -> NodeState
appendHearbeatTimeout ns = if _nsHeartbeatTimeout ns
                           then ns { _nsQueue = _nsQueue ns ++ [EHeartbeatTimeout]
                                   , _nsHeartbeatTimeout = False
                                   }
                           else ns

-- | Appends `EElectionTimeout' to queue if node has setup the timeout
-- and resets the timeout if so.
appendElectionTimeout :: NodeState -> NodeState
appendElectionTimeout ns = if _nsElectionTimeout ns
                           then ns { _nsQueue = _nsQueue ns ++ [EElectionTimeout]
                                   , _nsElectionTimeout = False
                                   }
                           else ns

-- | Appends the events to this `NodeId' s queue if this node is up.
appendEvents :: [Event EventType] -> NodeState -> NodeState
appendEvents toAdd ns | _nsStatus ns == Up = ns {_nsQueue = _nsQueue ns ++ toAdd}
                      | otherwise          = ns

-- | Truncates the log to a particular index.
truncateLog :: Index -> NodeState -> NodeState
truncateLog i ns = let l  = _nsLog ns
                       i' = fromIntegral $ unIndex i
                       l' = IntMap.filterWithKey (\k _ -> k <= i') l
                   in ns {_nsLog = l'}

-- | Appends entries to the log.
appendEntries :: [Entry EventType] -> NodeState -> NodeState
appendEntries es ns = let l = _nsLog ns
                          l' = foldr (\e -> MemLog.insert (fromIntegral $ unIndex $ eIndex e) e) l es
                      in ns { _nsLog = l' }

-- | Sets the `_nsCOmmitIndex' to `i'.
setCommitIndex :: Index -> NodeState -> NodeState
setCommitIndex i ns = ns { _nsCommitIndex = i }

-- | Executes a single command that was generated by this `NodeId'
-- This function will append any resulting `Event' s (depending on `Command' type)
-- to the `_nsQueue' s of the appropriate nodes.
handleCommand :: Monad m => NodeId -> Command EventType -> SState m ()
handleCommand nodeId c = case c of
    CBroadcast m -> each  %= appendEvents [EMessage nodeId m]
    CSend n m -> at n %= fmap (appendEvents [EMessage nodeId m])
    CResetElectionTimeout _ _ -> at nodeId %= fmap (\ns -> ns {_nsElectionTimeout = True})
    CResetHeartbeatTimeout _  -> at nodeId %= fmap (\ns -> ns {_nsHeartbeatTimeout = True})
    CLog _ -> return () -- Ignore for now
    CTruncateLog i -> at nodeId %= fmap (truncateLog i)
    CLogEntries es -> at nodeId %= fmap (appendEntries es)
    CSetCommitIndex i -> at nodeId %= fmap (setCommitIndex i)

-- | Executes the incoming event for this `NodeId'.
handleEvent :: Monad m => NodeId -> Event EventType -> SState m [Command EventType]
handleEvent nid event = do
    ns' <- use $ at nid
    case ns' of
        Just ns -> do
            let config = _nsConfig ns
                s      = _nsState ns
                theLog = _nsLog ns
                (newState, cmds) = MemLog.runMemLog (handle config s event) theLog
            at nid .= Just ns {_nsState = newState }
            return cmds
        Nothing -> return []

-- | Runs a single step of the simulation
-- by executing all events in this this `NodeId ' queue
-- executing resulting commands and distributing events.
executeQueue :: Monad m => NodeId -> SState m ()
executeQueue nid = do
    ns' <- use $ at nid
    case ns' of
        Just ns -> unless (List.null (_nsQueue ns)) $ do
           let queue = _nsQueue ns
               event = List.head queue

           -- Drop the event from the queue
           at nid .= Just ns { _nsQueue = List.tail queue }

           -- Run the simulation
           handleEvent nid event >>= mapM_ (handleCommand nid)

           -- Do it again until we exhaust the queue
           executeQueue nid
        Nothing -> return ()

-- | Applies the simulation event to the particular node state.
step :: Monad m => SimulationEvent -> SState m ()
step (KillNode nid) = at nid %= fmap killNode
step (ResurrectNode nid) = at nid %= fmap resurrect
step (HeartbeatTimeout nid) = at nid %= fmap appendHearbeatTimeout
step (ElectionTimeout nid) = at nid %= fmap appendElectionTimeout
step (ExecuteQueue nid) = executeQueue nid

-- | Checks if `NodeState' is in this `Mode'.
inMode :: Mode -> NodeState -> Bool
inMode m ns = (mode . _nsState) ns == m

-- | Unwraps the underlying `LeaderState'.
unwrapLeader :: SomeState -> Maybe LeaderState
unwrapLeader (WrapState (Leader s)) = Just s
unwrapLeader _                      = Nothing

-- | Unwraps the underlying `FollowerState'.
unwrapFollower :: SomeState -> Maybe FollowerState
unwrapFollower (WrapState (Follower s)) = Just s
unwrapFollower _                        = Nothing

-- | Checks if at least the quorum of nodes is up
-- and that all those nodes follow a single leader.
isLeaderElected :: ClusterState -> Bool
isLeaderElected nodes =
    let aliveNodes    = List.filter (\v -> _nsStatus v == Up) . Map.elems $ nodes
        aliveCount    = List.length aliveNodes
        leaders       = mapMaybe (unwrapLeader . _nsState) aliveNodes
        followers     = mapMaybe (unwrapFollower . _nsState) aliveNodes
        leaderTerms   = map _lCurrentTerm leaders
        leaderId      = listToMaybe . List.map (_configNodeId . _nsConfig) . List.filter (inMode MLeader) $ aliveNodes
        isQuorum      = aliveCount >= (Map.size nodes `div` 2 + 1)
    in
    -- Check that a quorum of nodes is alive, we have ONLY one leader
    -- and the rest are followers of this leader
    List.length leaders == 1 &&
    List.length followers == aliveCount - 1 &&
    isQuorum &&
    List.all (\f -> _fCurrentTerm f == head leaderTerms && _fVotedFor f == leaderId) followers

-----------------------------------------------------------------------------
-- Cluster model simulations.
-----------------------------------------------------------------------------

-- | Generates a cluster between 3 and 7 nodes
manageableCluster :: Gen ClusterState
manageableCluster = suchThat arbitraryClusterState between3And7
  where between3And7 cs = Map.size cs > 3 && Map.size cs < 8

-- | Runs the simulation until the condition is met,
-- keeping the number of simulations run.
simulateUntil :: (ClusterState -> Bool) -> PropertyM (StateT (Int, ClusterState) IO) () -> PropertyM (StateT (Int, ClusterState) IO) ()
simulateUntil cond a = do
   clusterState <- run $ zoom _2 $ use id
   unless (cond clusterState) $ a >> simulateUntil cond a

-- | Turns a simulation into a `Property' starting
-- from `manageableCluster'.
--
-- Using `unsafePerformIO' is apparently the only sane way to get
-- this working: I got this from the way `monadicIO' is written
-- in `Test.QuickCheck.Monadic'.
simulation :: PropertyM (StateT (Int, ClusterState) IO) () -> Property
simulation a = do
    cs <- manageableCluster
    monadic (unsafePerformIO `fmap` (`evalStateT` (1, cs))) a

-- | Runs a single step of the simulation by selecting a random `SimulationEvent'
-- and simulating the cluster.
simulationStep :: PropertyM (StateT (Int, ClusterState) IO) ()
simulationStep = do
    (i, cs) <- run get
    let elementsM :: Maybe [NodeId] -> (Int, NodeId -> SimulationEvent) -> Maybe (Int, Gen SimulationEvent)
        elementsM m (f, c)  = fmap (\ns -> (f, c <$> elements ns)) m
        toJust []           = Nothing
        toJust xs           = Just xs
        filteredJust :: (NodeState -> Bool) -> Maybe [NodeId]
        filteredJust p      = toJust . over each fst .  List.filter (p . snd) . Map.assocs $ cs
        aliveNodes          = filteredJust (\v -> _nsStatus v == Up)
        deadNodes           = filteredJust (\v -> _nsStatus v == Down)
        withHearbeatSet     = filteredJust _nsHeartbeatTimeout
        withElectionTimeout = filteredJust _nsElectionTimeout
        withNonEmptyQueue   = filteredJust (\v -> not . null . _nsQueue $ v)


    -- The probabilities are not equal,
    -- as a dying node is a relatively infrequent
    -- occurrence under normal operation.
    simulationCmd <- pick $ frequency $ catMaybes [
            elementsM aliveNodes (1, KillNode)
          , elementsM deadNodes (1, ResurrectNode)
          , elementsM withHearbeatSet (20, HeartbeatTimeout)
          , elementsM withElectionTimeout (20, ElectionTimeout)
          , elementsM withNonEmptyQueue (100, ExecuteQueue)
        ]
    --run . lift . putStrLn $ "Running: " ++ show i ++ " cmd: " ++ show simulationCmd
    run $ zoom _2 $ step simulationCmd
    run $ _1 += 1

-----------------------------------------------------------------------------
-- Reusable simulations.
-----------------------------------------------------------------------------

-- | Start the simulation by randomly selecting a node that
-- will have the initial `ElectionTimeout' otherwise it will take forever to start.
kickstartSimulation :: PropertyM (StateT (Int, ClusterState) IO) ()
kickstartSimulation = do
     nodeId <- (run . zoom _2 . use $ to Map.keys) >>= pick . elements
     let simulationCmd = ElectionTimeout nodeId
     --run . lift . putStrLn $ "Running: 0, cmd: " ++ show simulationCmd
     run $ zoom _2 $ step simulationCmd

-- | Runs the simulation until there is a stable
-- leader elected by the quorum.
--
-- This function can be used to put the cluster in a state
-- where there is a stable leader for further testing.
waitForLeader :: PropertyM (StateT (Int, ClusterState) IO) ()
waitForLeader = simulateUntil isLeaderElected simulationStep

