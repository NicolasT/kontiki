{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.Config (
      MonadConfig(..)
    ) where

import Control.Monad.Trans.Class (MonadTrans, lift)

import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict

import Control.Monad.Indexed.State (IxStateT)
import Control.Monad.Indexed.Trans (ilift)

class MonadConfig m where
    type Node m

    localNode :: m (Node m)
    default localNode :: (Monad m', MonadConfig m', MonadTrans t, m ~ t m') => m (Node m')
    localNode = lift localNode

instance (Monad m, MonadConfig m) => MonadConfig (Lazy.StateT s m) where
    type Node (Lazy.StateT s m) = Node m

instance (Monad m, MonadConfig m) => MonadConfig (Strict.StateT s m) where
    type Node (Strict.StateT s m) = Node m

instance (Monad m, MonadConfig m) => MonadConfig (IxStateT m i i) where
    type Node (IxStateT m i i) = Node m

    localNode = ilift localNode
