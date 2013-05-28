{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies #-}
module Network.Kontiki.Log where

import Network.Kontiki.Types

class MonadLog m a | m -> a where
    logEntry :: Index -> m (Maybe (Entry a))
    logLastEntry :: m (Maybe (Entry a))
