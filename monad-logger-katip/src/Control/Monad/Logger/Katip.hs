{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:      Control.Monad.Logger.Katip
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell,  TypeFamilies, UndecidableInstances
--
-- Linking 'MonadLogger' code with "Katip".
--
-- This module provides a monad transformer, 'KatipLoggingT', which
-- provides a 'MonadLogger' instance whose effects are passed to some
-- surrounding 'Katip' context.
--
-- When the 'LogSource' feature of 'MonadLogger' is used, this string is
-- split on dot characters, and used as "Katip" 'Namespace':
--
-- >>> $(logInfoS) "db.get" "Retrieving DB rows"
-- [2017-09-01 20:51:02][monad-logger-katip-test.db.get][Info][localhost][23952][ThreadId 1][main:Main test/Main.hs:38:8] Retrieving DB rows
--
-- == Handling of 'LevelOther'
-- "Katip" has no support for 'LevelOther'. If a message is logged through
-- 'MonadLogger' using the 'LevelOther' level, a warning message is emitted
-- once (through "Katip"), and the corresponding log messages are logged
-- (also through "Katip") using the 'ErrorS' severity. Furthermore, the
-- original level text (as passed to 'LevelOther') is prepended to the log
-- message, separated with a colon:
--
-- >>> $(logOther "fatal") "Unrecoverable error encountered"
-- [2017-09-01 20:51:02][monad-logger-katip-test][Warning][localhost][23952][ThreadId 1] monad-logger-katip doesn't support LevelOther, using ErrorS instead
-- [2017-09-01 20:51:02][monad-logger-katip-test][Error][locahost][23952][ThreadId 1][main:Main test/Main.hs:39:8] fatal: Unrecoverable error encountered

module Control.Monad.Logger.Katip (
    -- * The KatipLoggingT monad transformer
      KatipLoggingT(KatipLoggingT)
    , runKatipLoggingT
    , mapKatipLoggingT
    -- * Utilities to implement a 'MonadLogger' instance for you own 'Katip' or 'KatipContext'
    , Handler
    , defaultMonadLoggerLog
    , katipLogItem
    , katipContextLogItem
    ) where

import Control.Monad (MonadPlus, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import System.IO.Unsafe (unsafePerformIO)

import Control.Applicative (Alternative)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph (MFunctor)
import Control.Monad.Primitive (PrimMonad(PrimState, primitive))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    MonadBaseControl(StM, liftBaseWith, restoreM), defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(StT, liftWith, restoreT), defaultLiftWith, defaultRestoreT)
import Control.Monad.Trans.Identity (IdentityT(IdentityT, runIdentityT), mapIdentityT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Zip (MonadZip)
import Data.Conduit.Lazy (MonadActive)
import Data.Functor.Classes (Eq1, Ord1, Read1, Show1)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad.Logger (
    Loc, LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError, LevelOther), LogSource, MonadLogger(monadLoggerLog), ToLogStr,
    defaultLoc, toLogStr)

import System.Log.FastLogger (fromLogStr)

import Katip (
    Katip(getLogEnv, localLogEnv), KatipContext(getKatipContext, localKatipContext, getKatipNamespace, localKatipNamespace),
    LogItem, LogStr, Namespace(Namespace), Severity(DebugS, InfoS, WarningS, ErrorS), katipAddNamespace, logItem, logItemM, logStr)

import Control.Monad.Logger.Katip.Utils (location)

-- Using IdentityT gives us lots of instances 'for free'

-- | A monad transformer which provides a 'MonadLogger' implementation through 'Katip'.
newtype KatipLoggingT m a = KatipLoggingT { unKatipLoggingT :: IdentityT m a }
    deriving (
        Eq, Ord, Read, Show, Eq1, Ord1, Read1, Show1,
        Functor, Applicative, Monad,
        Alternative,
        Foldable,
        MFunctor,
        MonadActive,
        MonadBase b,
        MonadCatch,
        MonadCont,
        MonadError e,
        MonadFail,
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

deriving instance MonadResource m => MonadResource (KatipLoggingT m)

instance MonadBaseControl b m => MonadBaseControl b (KatipLoggingT m) where
    type StM (KatipLoggingT m) a = StM (IdentityT m) a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadTransControl KatipLoggingT where
    type StT KatipLoggingT a = StT IdentityT a
    liftWith = defaultLiftWith KatipLoggingT unKatipLoggingT
    restoreT = defaultRestoreT KatipLoggingT

instance PrimMonad m => PrimMonad (KatipLoggingT m) where
    type PrimState (KatipLoggingT m) = PrimState (IdentityT m)
    primitive = lift . primitive

instance Katip m => Katip (KatipLoggingT m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapKatipLoggingT . localLogEnv

instance KatipContext m => KatipContext (KatipLoggingT m) where
    getKatipContext = lift getKatipContext
    localKatipContext = mapKatipLoggingT . localKatipContext
    getKatipNamespace = lift getKatipNamespace
    localKatipNamespace = mapKatipLoggingT . localKatipNamespace

instance Katip m => MonadLogger (KatipLoggingT m) where
    monadLoggerLog = defaultMonadLoggerLog $ katipLogItem ()
    {-# INLINE monadLoggerLog #-}

-- | Lift a unary operation to the new monad.
mapKatipLoggingT :: (m a -> n b) -> KatipLoggingT m a -> KatipLoggingT n b
mapKatipLoggingT f = KatipLoggingT . mapIdentityT f . unKatipLoggingT
{-# INLINE mapKatipLoggingT #-}

-- | Run a 'KatipLoggingT', rendering all 'MonadLogger' effects through 'Katip'.
runKatipLoggingT :: Katip m => KatipLoggingT m a -> m a
runKatipLoggingT = runIdentityT . unKatipLoggingT
{-# INLINE runKatipLoggingT #-}

-- | Type of handler actions passed to 'defaultMonadLoggerLog'.
type Handler m = Maybe Namespace -> Maybe Loc -> Severity -> LogStr -> m ()

-- | Default implementation for 'monadLoggerLog'.
--
-- The first argument is a handler for generated log messages. In general,
-- you can simply use 'katipLogItem' or 'katipContextLogItem', depending on
-- the level of functionality your monad provides.
defaultMonadLoggerLog :: (MonadIO m, ToLogStr msg)
                      => Handler m -- ^ Handler for generated log messages
                      -> Loc
                      -> LogSource
                      -> LogLevel
                      -> msg
                      -> m ()
defaultMonadLoggerLog logItem' loc src level msg = case mapLogLevel level of
    Right level' -> logItem' src' loc' level' msg'
    Left level' -> logLevelOther logItem' src' loc' level' msg'
  where
    src' | Text.null src = Nothing -- Short-circuit common case
         | otherwise = Just $ Namespace $ Text.splitOn dot src
    -- Don't pass loc if it's defaultLoc (i.e. not really passed to monadLoggerLog)
    -- Common case is to have a location (when using the TH splices)
    loc' = if loc /= defaultLoc then Just loc else Nothing
    msg' = logStr $ fromLogStr $ toLogStr msg
    mapLogLevel :: LogLevel -> Either Text Severity
    mapLogLevel l = case l of
        LevelDebug -> Right DebugS
        LevelInfo -> Right InfoS
        LevelWarn -> Right WarningS
        LevelError -> Right ErrorS
        LevelOther lvl -> Left lvl
    dot = Text.singleton '.'
{-# INLINE defaultMonadLoggerLog #-}


-- | Cell tracking whether the 'LevelOther' warning has been emitted before
emitLevelOtherWarning :: IORef Bool
emitLevelOtherWarning = unsafePerformIO $ newIORef True
{-# NOINLINE emitLevelOtherWarning #-}

-- Kept out of 'defaultMonadLoggerLog' so that one can be inlined
logLevelOther :: MonadIO m
              => Handler m
              -> Maybe Namespace
              -> Maybe Loc
              -> Text
              -> LogStr
              -> m ()
logLevelOther logItem' src loc level msg = do
    emit <- liftIO $ do
        emit <- readIORef emitLevelOtherWarning
        when emit $
            writeIORef emitLevelOtherWarning False
        return emit
    when emit $
        logItem' Nothing (Just $(location)) WarningS warning
    logItem' src loc ErrorS $ mconcat [logStr level, logStr ": ", msg]
  where
    warning = logStr "monad-logger-katip doesn't support LevelOther, using ErrorS instead"


-- | Handler for 'defaultMonadLoggerLog' which logs messages in a 'Katip' environment using 'logItem'.
katipLogItem :: (Katip m, LogItem i)
             => i -- ^ 'LogItem' passed to 'logItem'
             -> Handler m
katipLogItem i = logItem i . fromMaybe emptyNamespace
  where
    emptyNamespace = Namespace []
{-# INLINE katipLogItem #-}

-- | Handler for 'defaultMonadLoggerLog' which logs messages in a 'KatipContext' environment using 'logItemM'.
katipContextLogItem :: KatipContext m => Handler m
katipContextLogItem n = case n of
    Nothing -> logItemM
    Just n' -> \l s m -> katipAddNamespace n' $ logItemM l s m
{-# INLINE katipContextLogItem #-}
