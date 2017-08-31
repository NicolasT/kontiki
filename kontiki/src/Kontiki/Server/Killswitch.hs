module Kontiki.Server.Killswitch (
      mkKillswitch
    , mkKillswitchIO
    , trigger
    , await
    ) where

import Control.Concurrent.STM (STM, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar)

newtype Killswitch = Killswitch { getKillswitch :: TVar Bool }

mkKillswitch :: STM Killswitch
mkKillswitch = Killswitch <$> newTVar False

mkKillswitchIO :: IO Killswitch
mkKillswitchIO = Killswitch <$> newTVarIO False

trigger :: Killswitch -> STM ()
trigger = flip writeTVar True . getKillswitch

await :: Killswitch -> STM ()
await k = (\b -> if b then return () else retry) =<< readTVar (getKillswitch k)
