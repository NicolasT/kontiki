{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Monad
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- This module defines the abstract `MonadLog' type for an in-memory
-- log of `Entry'ies that a kontiki cluster aims to keep in sync across nodes. 
-----------------------------------------------------------------------------
module Network.Kontiki.Log where

import Network.Kontiki.Types

-- | Log of `Entry'ies.
class MonadLog m a | m -> a where
    -- | Gets the `Entry' at this `Index', 
    -- or `Nothing' if there is no entry for this `Index'.
    logEntry :: Index -> m (Maybe (Entry a))
    
    -- | Gets the `Entry' with the highest `Index' in this `MonadLog'
    -- or `Nothing' if this `MonadLog' is empty.
    logLastEntry :: m (Maybe (Entry a))
