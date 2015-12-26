{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Network.Kontiki.HandleSpec where

import           Control.Monad.Identity
import           Network.Kontiki.Raft
import           Test.Hspec
import           Test.QuickCheck

handleSpec :: Spec
handleSpec = do
    it "FSM has no obvious error cases" $ property prop_handle

-- A stub MonadLog which, well... has no log at all
newtype Stub a = Stub { unStub :: Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           )

instance MonadLog Stub Int where
    logEntry _ = return Nothing
    logLastEntry = return Nothing

-- Make sure there are no obvious 'error' cases in the FSM
prop_handle :: Config -> SomeState -> Event Int -> Bool
prop_handle cfg state event =
    let (state', commands) = runIdentity $ unStub $ handle cfg state event in
    state' `seq` commands `seq` True
