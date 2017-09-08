{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Metrics (
      MetricsT
    , runMetricsT
    , mapMetricsT
    ) where

import Control.Monad.Trans.Reader (ReaderT(ReaderT), mapReaderT, runReaderT)

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Morph (MFunctor)
import Control.Monad.Primitive (PrimMonad(PrimState, primitive))
import Control.Monad.Reader.Class (MonadReader(ask, local, reader))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    MonadBaseControl(StM, liftBaseWith, restoreM), defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(StT, liftWith, restoreT), defaultLiftWith, defaultRestoreT,
    ComposeSt)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Zip (MonadZip)
import Katip (Katip, KatipContext)

import Control.Monad.Metrics (Metrics, MonadMetrics(getMetrics))

newtype MetricsT m a = MetricsT { unMetricsT :: ReaderT Metrics m a }
    deriving (
        Functor, Monad,
        Alternative,
        Katip,
        KatipContext,
        MFunctor,
        MonadBase b,
        MonadCatch,
        MonadCont,
        MonadError e,
        MonadFail,
        MonadFix,
        MonadIO,
        MonadLogger,
        MonadMask,
        MonadPlus,
        MonadState s,
        MonadThrow,
        MonadTrans,
        MonadWriter w,
        MonadZip)

-- Work around https://ghc.haskell.org/trac/ghc/ticket/12804
-- See also https://hub.darcs.net/ross/transformers/issue/33
instance Applicative m => Applicative (MetricsT m) where
    pure a = MetricsT $ ReaderT $ const (pure a)
    {-# INLINE pure #-}
    MetricsT f <*> MetricsT v = MetricsT $ ReaderT $ \r -> runReaderT f r <*> runReaderT v r
    {-# INLINE (<*>) #-}
    MetricsT u *> MetricsT v = MetricsT $ ReaderT $ \r -> runReaderT u r *> runReaderT v r
    {-# INLINE (*>) #-}
    MetricsT u <* MetricsT v = MetricsT $ ReaderT $ \r -> runReaderT u r <* runReaderT v r
    {-# INLINE (<*) #-}

instance MonadReader r m => MonadReader r (MetricsT m) where
    ask = lift ask
    local = mapMetricsT . local
    reader = lift . reader

deriving instance MonadResource m => MonadResource (MetricsT m)
deriving instance MonadRWS r w s m => MonadRWS r w s (MetricsT m)

instance MonadBaseControl b m => MonadBaseControl b (MetricsT m) where
    type StM (MetricsT m) a = ComposeSt MetricsT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadTransControl MetricsT where
    type StT MetricsT a = StT (ReaderT Metrics) a
    liftWith = defaultLiftWith MetricsT unMetricsT
    restoreT = defaultRestoreT MetricsT

instance PrimMonad m => PrimMonad (MetricsT m) where
    type PrimState (MetricsT m) = PrimState (ReaderT Metrics m)
    primitive = lift . primitive

instance Monad m => MonadMetrics (MetricsT m) where
    getMetrics = MetricsT ask
    {-# INLINE getMetrics #-}

runMetricsT :: MetricsT m a -> Metrics -> m a
runMetricsT = runReaderT . unMetricsT

mapMetricsT :: (m a -> n b) -> MetricsT m a -> MetricsT n b
mapMetricsT f = MetricsT . mapReaderT f . unMetricsT
