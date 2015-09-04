{-# LANGUAGE FlexibleInstances,
             MultiParamTypeClasses,
             GeneralizedNewtypeDeriving #-}

module Data.Kontiki.MemLog (
      Log
    , MemLog
    , runMemLog
    , IntMap.empty
    , IntMap.insert
    ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Control.Monad.Reader (MonadReader, Reader, ask, runReader)

import Network.Kontiki.Raft (Entry, MonadLog(..), unIndex)

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


