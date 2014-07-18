{-# LANGUAGE OverloadedStrings,
             GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses #-}

module Main (main) where

import Control.Applicative

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.Set as Set

import Control.Monad.Reader

import Network.Kontiki.Raft

type Log a = IntMap (Entry a)
newtype MemLog a r = MemLog { unMemLog :: Reader (Log a) r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Log a)
           )

instance MonadLog (MemLog a) a where
    logEntry i = IntMap.lookup (fromIntegral $ unIndex i) `fmap` ask
    logLastEntry = do
        l <- ask
        return $ if IntMap.null l
                    then Nothing
                    else Just $ snd $ IntMap.findMax l

runMemLog :: MemLog a r -> Log a -> r
runMemLog = runReader . unMemLog

type Value = String

config :: Config
config = Config { _configNodeId = "node0"
                , _configNodes = Set.fromList ["node0", "node1"]
                , _configElectionTimeout = 10
                , _configHeartbeatTimeout = 2
                }

main :: IO ()
main = print $ runMemLog (handle config initialState EHeartbeatTimeout) (IntMap.empty :: Log Value)
