-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.State
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable (although the MTL instances aren't!)
--
----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Control.Monad.Indexed.State
  ( IxMonadState(..)
  , imodify
  , igets
  , IxStateT(..)
  , IxState(..)
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Data.Bifunctor
import Control.Monad.Indexed
import Control.Monad.Indexed.Trans
import Control.Monad.Indexed.Fix
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Error.Class

class IxMonad m => IxMonadState m where
  iget :: m i i i
  iput :: j -> m i j ()

imodify :: IxMonadState m => (i -> j) -> m i j ()
imodify f = iget >>>= iput . f

igets :: IxMonadState m => (i -> a) -> m i i a
igets f = iget >>>= ireturn . f

-- Indexed State Monad

newtype IxState i j a = IxState { runIxState :: i -> (a, j) }

instance Functor (IxState i j) where
  fmap = imap

instance IxFunctor IxState where
  imap f m = IxState (first f . runIxState m)

instance IxPointed IxState where
  ireturn = IxState . (,)

instance IxApplicative IxState where
  iap = iapIxMonad

instance IxMonad IxState where
  ibind f m = IxState $ \s1 -> let (a,s2) = runIxState m s1 in runIxState (f a) s2

instance IxMonadState IxState where
  iget = IxState (\x -> (x,x))
  iput x = IxState (\_ -> ((),x))

instance Bifunctor (IxState i) where
  bimap f g m = IxState $ bimap g f . runIxState m

instance Monad (IxState i i) where
  return = ireturn
  m >>= k = ibind k m

instance Applicative (IxState i i) where
  pure = ireturn
  (<*>) = iap

instance MonadState i (IxState i i) where
  get = iget
  put = iput

instance MonadFix (IxState i i) where
    mfix = imfix

instance IxMonadFix IxState where
    imfix f = IxState $ \s -> let (a, s') = runIxState (f a) s in (a, s')


-- Indexed State Monad Transformer

newtype IxStateT m i j a = IxStateT { runIxStateT :: i -> m (a, j) }

instance Monad m => Functor (IxStateT m i j) where
  fmap = imap

instance Monad m => IxFunctor (IxStateT m) where
  imap f m = IxStateT $ \s -> runIxStateT m s >>= \(x,s') -> return (f x, s')

instance Monad m => IxPointed (IxStateT m) where
      ireturn a = IxStateT $ \s -> return (a, s)

instance Monad m => IxApplicative (IxStateT m) where
     iap = iapIxMonad

instance Monad m => IxMonad (IxStateT m) where
      ibind k m = IxStateT $ \s -> runIxStateT m s >>= \ ~(a, s') -> runIxStateT (k a) s'

instance Monad m => Bifunctor (IxStateT m i) where
  bimap f g m = IxStateT $ liftM (bimap g f) . runIxStateT m

instance Monad m => IxMonadState (IxStateT m) where
  iget   = IxStateT $ \s -> return (s, s)
  iput s = IxStateT $ \_ -> return ((), s)

instance MonadPlus m => IxMonadZero (IxStateT m) where
  imzero = IxStateT $ const mzero

instance MonadPlus m => IxMonadPlus (IxStateT m) where
  m `implus` n = IxStateT $ \s -> runIxStateT m s `mplus` runIxStateT n s

instance MonadFix m => IxMonadFix (IxStateT m) where
  imfix f = IxStateT $ \s -> mfix $ \ ~(a, _) -> runIxStateT (f a) s

instance MonadFix m => MonadFix (IxStateT m i i) where
  mfix = imfix

instance Monad m => Monad (IxStateT m i i) where
  return = ireturn
  m >>= k = ibind k m

instance Monad m => Applicative (IxStateT m i i) where
  pure = ireturn
  (<*>) = iap

instance Monad m => MonadState i (IxStateT m i i) where
  get = iget
  put = iput

instance IxMonadTrans IxStateT where
  ilift m = IxStateT $ \s -> m >>= \a -> return (a, s)

instance MonadIO m => MonadIO (IxStateT m i i) where
  liftIO = ilift . liftIO

instance MonadReader r m => MonadReader r (IxStateT m i i) where
  ask = ilift ask
  local f m = IxStateT (local f . runIxStateT m)

instance MonadCont m => MonadCont (IxStateT m i i) where
  callCC f = IxStateT $ \s -> callCC $ \k -> runIxStateT (f (\a -> IxStateT $ \s' -> k (a,s'))) s

instance MonadError e m => MonadError e (IxStateT m i i) where
  throwError = ilift . throwError
  m `catchError` h = IxStateT $ \s -> runIxStateT m s `catchError` \e -> runIxStateT (h e) s

instance MonadWriter w m => MonadWriter w (IxStateT m i i) where
  tell = ilift . tell
  listen m = IxStateT $ \s -> do
    ~((a,s'),w) <- listen (runIxStateT m s)
    return ((a,w),s')
  pass m = IxStateT $ \s -> pass $ do
    ~((a,f),s') <- runIxStateT m s
    return ((a,s'),f)
