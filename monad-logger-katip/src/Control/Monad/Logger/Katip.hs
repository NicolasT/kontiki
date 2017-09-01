{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Control.Monad.Logger.Katip
-- Copyright: (C) 2017 Nicolas Trangez
-- License: Apache (see the file LICENSE)
--
-- Maintainer: ikke@nicolast.be
-- Stability: alpha
-- Portability: DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances
--
-- Linking "Control.Monad.Logger" code with "Katip".
--
-- This module provides a monad transformer, 'KatipLoggingT', which
-- provides a 'MonadLogger' instance whose effects are passed to some
-- surrounding 'Katip' context.
--
-- It also provides orphan 'MonadLogger' instances for 'KatipT' and
-- 'KatipContextT' using the same mechanism.
--
-- /Note:/ "Katip" has no support for 'LevelOther'. If a message is logged
-- through 'MonadLogger' using the 'LevelOther' level, a warning message is
-- printed to 'stderr' (once), and the corresponding log messages are also
-- written to 'stderr', formatted using 'defaultLogStr', i.e. not through the
-- "Katip" infrastructure.

module Control.Monad.Logger.Katip (
    -- * The KatipLoggingT monad transformer
      KatipLoggingT(KatipLoggingT)
    , runKatipLoggingT
    , mapKatipLoggingT
    ) where

import Control.Monad (MonadPlus, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative (Alternative)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl(StM, liftBaseWith, restoreM), MonadTransControl(StT, liftWith, restoreT))
import Control.Monad.Trans.Identity (IdentityT(IdentityT, runIdentityT), mapIdentityT)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Zip (MonadZip)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Data.ByteString.Char8 as BS

import Control.Monad.Logger (
    Loc, LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError, LevelOther), LogSource, MonadLogger(monadLoggerLog), ToLogStr,
    defaultLogStr, toLogStr)

import System.Log.FastLogger (fromLogStr)

import Katip (
    Katip(getLogEnv, localLogEnv), KatipT,
    KatipContext(getKatipContext, localKatipContext, getKatipNamespace, localKatipNamespace), KatipContextT,
    Namespace(Namespace), Severity(DebugS, InfoS, WarningS, ErrorS), logItem, logStr)

-- Using IdentityT gives us lots of instances 'for free'

-- | A monad transformer which provides a 'MonadLogger' implementation through 'Katip'.
newtype KatipLoggingT m a = KatipLoggingT { unKatipLoggingT :: IdentityT m a }
    deriving (
        Functor, Applicative, Monad,
        Alternative,
        Foldable,
        MonadBase b,
        MonadCatch,
        MonadCont,
        MonadError e,
        MonadFix,
        MonadIO,
        MonadMask,
        MonadPlus,
        MonadReader r,
        MonadRWS r w s,
        MonadState s,
        MonadThrow,
        MonadTrans,
        MonadWriter w,
        MonadZip,
        Traversable)

instance MonadBaseControl b m => MonadBaseControl b (KatipLoggingT m) where
    type StM (KatipLoggingT m) a = StM m a
    liftBaseWith f = KatipLoggingT $ liftBaseWith $ \runInBase -> f $ runInBase . unKatipLoggingT
    restoreM = KatipLoggingT . IdentityT . restoreM

instance MonadTransControl KatipLoggingT where
    type StT KatipLoggingT a = a
    liftWith f = KatipLoggingT $ IdentityT $ f $ runIdentityT . unKatipLoggingT
    restoreT = KatipLoggingT . IdentityT

instance (MonadIO m, Katip m) => Katip (KatipLoggingT m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapKatipLoggingT . localLogEnv

instance KatipContext m => KatipContext (KatipLoggingT m) where
    getKatipContext = lift getKatipContext
    localKatipContext = mapKatipLoggingT . localKatipContext
    getKatipNamespace = lift getKatipNamespace
    localKatipNamespace = mapKatipLoggingT . localKatipNamespace

-- | Lift a unary operation to the new monad.
mapKatipLoggingT :: (m a -> n b) -> KatipLoggingT m a -> KatipLoggingT n b
mapKatipLoggingT f = KatipLoggingT . mapIdentityT f . unKatipLoggingT
{-# INLINE mapKatipLoggingT #-}

-- | Run a 'KatipLoggingT', rendering all 'MonadLogger' effects through 'Katip'.
runKatipLoggingT :: Katip m => KatipLoggingT m a -> m a
runKatipLoggingT = runIdentityT . unKatipLoggingT
{-# INLINE runKatipLoggingT #-}


-- | Cell tracking whether the 'LevelOther' warning has been emitted before
emitLevelOtherWarning :: IORef Bool
emitLevelOtherWarning = unsafePerformIO $ newIORef True
{-# NOINLINE emitLevelOtherWarning #-}

defaultMonadLoggerLog :: (MonadIO m, Katip m, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> m ()
defaultMonadLoggerLog loc src level msg = case mapLogLevel level of
    Just sev -> logItem () namespace (Just loc) sev (logStr $ fromLogStr $ toLogStr msg)
    Nothing -> liftIO $ do
        emit <- readIORef emitLevelOtherWarning
        when emit $ do
            writeIORef emitLevelOtherWarning False
            Text.hPutStrLn stderr $ Text.pack "Warning: monad-logger-katip doesn't support LevelOther"
        BS.hPutStr stderr $ fromLogStr $ defaultLogStr loc src level (toLogStr msg)
  where
    namespace = Namespace (if Text.null src then [] else [src])
    mapLogLevel :: LogLevel -> Maybe Severity
    mapLogLevel l = case l of
        LevelDebug -> Just DebugS
        LevelInfo -> Just InfoS
        LevelWarn -> Just WarningS
        LevelError -> Just ErrorS
        LevelOther _ -> Nothing


instance (MonadIO m, Katip m) => MonadLogger (KatipLoggingT m) where
    monadLoggerLog = defaultMonadLoggerLog

instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = defaultMonadLoggerLog

instance MonadIO m => MonadLogger (KatipContextT m) where
    monadLoggerLog = defaultMonadLoggerLog
