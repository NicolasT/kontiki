-------------------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Indexed.Cont
-- Copyright   : 2008 Edward Kmett, Dan Doel
-- License     : BSD
--
-- Maintainer  : Reiner Pope <reiner.pope@gmail.com>
-- Stability   : experimental
-- Portability : rank-2 Types required for correctness of shift, but they can be removed
-------------------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Control.Monad.Indexed.Cont
  ( IxMonadCont(reset, shift)
  , IxContT(IxContT, runIxContT)
  , runIxContT_
  , IxCont(IxCont)
  , runIxCont
  , runIxCont_
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Data.Pointed
import qualified Control.Monad.Cont as Cont
import Control.Monad.Identity
import Control.Monad.Indexed
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Indexed.Trans

class IxMonad m => IxMonadCont m where
  reset :: m a o o -> m r r a
  shift :: ((forall i. a -> m i i o) -> m r j j) -> m r o a
-- shift :: ((a -> m i i o) -> m r j j) -> m r o a

newtype IxContT m r o a = IxContT { runIxContT :: (a -> m o) -> m r }

runIxContT_ :: Monad m => IxContT m r a a -> m r
runIxContT_ m = runIxContT m return

instance IxFunctor (IxContT m) where
  imap f m = IxContT $ \c -> runIxContT m (c . f)

instance IxPointed (IxContT m) where
  ireturn a = IxContT ($a)

instance IxApplicative (IxContT m) where
  iap = iapIxMonad

instance IxMonad (IxContT m) where
  ibind f c = IxContT $ \k -> runIxContT c $ \a -> runIxContT (f a) k

instance Monad m => IxMonadCont (IxContT m) where
  reset e = IxContT $ \k -> runIxContT e return >>= k
  shift e = IxContT $ \k -> e (\a -> IxContT (\k' -> k a >>= k')) `runIxContT` return

callCC :: Monad m => ((forall i b. a -> IxContT m o i b) -> IxContT m r o a) -> IxContT m r o a
callCC f = shift (\k -> f (adapt k) >>>= k)
  where
    -- Both 'shift' and 'callCC' capture the current continuation up to the
    -- containing 'reset'; but where 'shift' continuations "return" the
    -- value "return"ed by the containing 'reset'-delimited computation,
    -- 'callCC' continuations never "return" but instead cause the
    -- containing 'reset' to "return" the captured continuation's result.
    --
    -- @adapt k x@ converts a 'shift'-style continuation to a
    -- 'callCC'-style continuation by using its "return"ed value directly
    -- as the final result.
    --
    -- @escape x@ ignores its continuation and provides x directly as the
    -- result.
    adapt k x = k x >>>= escape
    escape x = IxContT (\_k -> return x)

instance Functor (IxContT m i j) where
  fmap = imap

instance Pointed (IxContT m i i) where
  point = ireturn

instance Applicative (IxContT m i i) where
  pure = ireturn
  (<*>) = iap

instance Monad (IxContT m i i) where
  return = ireturn
  m >>= k = ibind k m

instance Monad m => Cont.MonadCont (IxContT m i i) where
  -- GHC < 7.10 needs some hand-holding to specialize the 'forall' in the
  -- continuation type.  Otherwise we'd just have:
  --   callCC = callCC
  callCC f = callCC (\k -> f k)

instance IxMonadTrans IxContT where
  ilift m = IxContT (m >>=)

instance MonadReader e m => MonadReader e (IxContT m i i) where
  ask = ilift ask
  local f m = IxContT $ \c -> do
    r <- ask
    local f (runIxContT m (local (const r) . c))

instance MonadState e m => MonadState e (IxContT m i i) where
  get = ilift get
  put = ilift . put

instance MonadIO m => MonadIO (IxContT m i i) where
  liftIO = ilift . liftIO

newtype IxCont r o a = IxCont (IxContT Identity r o a)
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, IxMonadCont)


runIxCont :: IxCont r o a -> (a -> o) -> r
runIxCont (IxCont k) f = runIdentity $ runIxContT k (return . f)

runIxCont_ :: IxCont r a a -> r
runIxCont_ m = runIxCont m id

instance Cont.MonadCont (IxCont i i) where
  callCC f = IxCont (callCC (\q -> unwrapIxCont (f (IxCont . q))))
    where unwrapIxCont (IxCont x) = x

instance Functor (IxCont i j) where
  fmap = imap

instance Pointed (IxCont i i) where
  point = ireturn

instance Applicative (IxCont i i) where
  pure = ireturn
  (<*>) = iap

instance Monad (IxCont i i) where
  return = ireturn
  m >>= k = ibind k m

