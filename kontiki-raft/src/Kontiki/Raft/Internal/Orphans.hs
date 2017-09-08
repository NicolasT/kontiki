{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kontiki.Raft.Internal.Orphans () where

import Control.Monad.Indexed.State (IxStateT)
import Control.Monad.Indexed.Trans (ilift)

import Control.Monad.Logger (MonadLogger(monadLoggerLog))

instance MonadLogger m => MonadLogger (IxStateT m i i) where
    monadLoggerLog loc src lvl msg = ilift $ monadLoggerLog loc src lvl msg
