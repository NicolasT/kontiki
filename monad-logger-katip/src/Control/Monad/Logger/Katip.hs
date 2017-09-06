{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
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
-- Portability: DeriveTraversable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, PolyKinds, Rank2Types, StandaloneDeriving, TemplateHaskell,  TypeFamilies, UndecidableInstances
--
-- Linking 'MonadLogger' code with "Katip".
--
-- This module provides a monad transformer, 'KatipLoggingT', which
-- provides a 'MonadLogger' instance whose effects are passed to some
-- surrounding 'Katip' or 'KatipContext' context.
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
    -- ** Re-exports to use with 'runKatipLoggingT' and 'TypeApplications'
    , Katip
    , KatipContext
    -- * Utilities to implement a 'MonadLogger' instance for you own 'Katip' or 'KatipContext'
    , LogItem
    , MonadLoggerLog
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
    MonadTransControl(StT, liftWith, restoreT), defaultLiftWith, defaultRestoreT,
    ComposeSt)
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
    LogStr, Namespace(Namespace), Severity(DebugS, InfoS, WarningS, ErrorS), katipAddNamespace, logItem, logItemM, logStr)
import qualified Katip
import Katip.Core (getLocTH)

-- Using IdentityT gives us lots of instances 'for free'
-- Rules for instances: anything which is in 'base', or any of the
-- transitive dependencies of 'katip' or 'monad-logger' (on which we
-- depend anyway) goes. Introducing new dependencies is to be avoided.

-- | A monad transformer which provides a 'MonadLogger' implementation through 'Katip' or 'KatipContext'.
newtype KatipLoggingT (ctx :: k) m a = KatipLoggingT { unKatipLoggingT :: IdentityT m a }
    deriving (
        Eq1, Ord1, Read1, Show1,
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

deriving instance (Eq1 m, Eq a) => Eq (KatipLoggingT ctx m a)
deriving instance (Ord1 m, Ord a) => Ord (KatipLoggingT ctx m a)
deriving instance (Read1 m, Read a) => Read (KatipLoggingT ctx m a)
deriving instance (Show1 m, Show a) => Show (KatipLoggingT ctx m a)

deriving instance MonadResource m => MonadResource (KatipLoggingT ctx m)

instance MonadBaseControl b m => MonadBaseControl b (KatipLoggingT ctx m) where
    type StM (KatipLoggingT ctx m) a = ComposeSt (KatipLoggingT ctx) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadTransControl (KatipLoggingT ctx) where
    type StT (KatipLoggingT ctx) a = StT IdentityT a
    liftWith = defaultLiftWith KatipLoggingT unKatipLoggingT
    restoreT = defaultRestoreT KatipLoggingT

instance PrimMonad m => PrimMonad (KatipLoggingT ctx m) where
    type PrimState (KatipLoggingT ctx m) = PrimState (IdentityT m)
    primitive = lift . primitive

instance Katip m => Katip (KatipLoggingT ctx m) where
    getLogEnv = lift getLogEnv
    localLogEnv = mapKatipLoggingT . localLogEnv

instance KatipContext m => KatipContext (KatipLoggingT ctx m) where
    getKatipContext = lift getKatipContext
    localKatipContext = mapKatipLoggingT . localKatipContext
    getKatipNamespace = lift getKatipNamespace
    localKatipNamespace = mapKatipLoggingT . localKatipNamespace

instance Katip m => MonadLogger (KatipLoggingT Katip m) where
    monadLoggerLog = defaultMonadLoggerLog $ katipLogItem ()
    {-# INLINE monadLoggerLog #-}

instance KatipContext m => MonadLogger (KatipLoggingT KatipContext m) where
    monadLoggerLog = defaultMonadLoggerLog katipContextLogItem
    {-# INLINE monadLoggerLog #-}

-- | Lift a unary operation to the new monad.
mapKatipLoggingT :: (m a -> n b) -> KatipLoggingT ctx m a -> KatipLoggingT ctx n b
mapKatipLoggingT f = KatipLoggingT . mapIdentityT f . unKatipLoggingT
{-# INLINE mapKatipLoggingT #-}

-- | Run a 'KatipLoggingT' 'ctx' 'm', in turn rendering all 'MonadLogger' effects through 'ctx' in 'm'.
--
-- The "Katip" packages provides two levels of functionality: 'Katip' and
-- 'KatipContext', where the latter is more powerful than the first. Users
-- of the 'KatipLoggingT' transformer should be able to use it in both
-- kinds of environments.
--
-- Instead of duplicating all functionality, 'KatipLoggingT' is
-- parametrized over a context (the 'ctx' variable), and two instances of
-- 'MonadLogger' are provided:
--
-- @
-- instance Katip m => MonadLogger (KatipLoggingT Katip m)
-- instance KatipContext m => MonadLogger (KatipLoggingT KatipContext m)
-- @
--
-- where the first uses 'defaultMonadLoggerLog' 'katipLogItem' (i.e.
-- 'logItem'), the second 'defaultMonadLoggerLog' 'katipContextLogItem'
-- (i.e. 'logItemM').
--
-- Now whenever a 'KatipLoggingT' is found, one can select which
-- implementation to use by passing the desired 'ctx' to
-- 'runKatipLoggingT'. Say we have
--
-- @
-- myLoggingAction :: MonadLogger m => m ()
-- @
--
-- then we get
--
-- @
-- runKatipLoggingT myLoggingAction :: MonadLogger (KatipLoggingT ctx m) => m ()
-- @
--
-- To evaluate this action, we need to fix 'ctx', e.g. using
-- 'TypeApplication':
--
-- >>> runKatipLoggingT @Katip myLoggingAction :: Katip m => m ()
-- >>> runKatipLoggingT @KatipContext myLoggingAction :: KatipContext m => m ()
--
-- /Note:/ In the above, the type signatures are for documentation purposes
-- only, they don't need to be written in actual code.
--
-- Depending on the functionality available in the environment, we can use
-- one or the other.
runKatipLoggingT :: forall ctx m a. KatipLoggingT ctx m a -> m a
runKatipLoggingT = runIdentityT . unKatipLoggingT
{-# INLINE runKatipLoggingT #-}

-- | Type of handler actions passed to 'defaultMonadLoggerLog'.
type LogItem m = Maybe Namespace -> Maybe Loc -> Severity -> LogStr -> m ()

-- | The type of 'monadLoggerLog'.
type MonadLoggerLog m = forall msg. ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> m ()

-- | Default implementation for 'monadLoggerLog'.
--
-- The first argument is a handler for generated log messages. In general,
-- you can simply use 'katipLogItem' or 'katipContextLogItem', depending on
-- the level of functionality your monad provides.
defaultMonadLoggerLog :: MonadIO m
                      => LogItem m -- ^ Handler for generated log messages
                      -> MonadLoggerLog m
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
              => LogItem m
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
        logItem' Nothing (Just $(getLocTH)) WarningS warning
    logItem' src loc ErrorS $ mconcat [logStr level, logStr ": ", msg]
  where
    warning = logStr "monad-logger-katip doesn't support LevelOther, using ErrorS instead"


-- | Handler for 'defaultMonadLoggerLog' which logs messages in a 'Katip' environment using 'logItem'.
katipLogItem :: (Katip m, Katip.LogItem i)
             => i -- ^ 'Katip.LogItem' passed to 'logItem'
             -> LogItem m
katipLogItem i = logItem i . fromMaybe emptyNamespace
  where
    emptyNamespace = Namespace []
{-# INLINE katipLogItem #-}

-- | Handler for 'defaultMonadLoggerLog' which logs messages in a 'KatipContext' environment using 'logItemM'.
katipContextLogItem :: KatipContext m => LogItem m
katipContextLogItem n = case n of
    Nothing -> logItemM
    Just n' -> \l s m -> katipAddNamespace n' $ logItemM l s m
{-# INLINE katipContextLogItem #-}
