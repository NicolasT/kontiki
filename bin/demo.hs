{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import qualified Data.IntMap          as IntMap
import           Data.Kontiki.MemLog
import qualified Data.Set             as Set
import           Network.Kontiki.Raft

type Value = String

config :: Config
config = Config { _configNodeId = "node0"
                , _configNodes = Set.fromList ["node0", "node1"]
                , _configElectionTimeout = 10
                , _configHeartbeatTimeout = 2
                }

main :: IO ()
main = print $ runMemLog (handle config initialState EHeartbeatTimeout) (IntMap.empty :: Log Value)
