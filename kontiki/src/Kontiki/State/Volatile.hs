{-# LANGUAGE TypeFamilies #-}

module Kontiki.State.Volatile (
      VolatileState
    ) where

import Control.Lens (lens)
import Data.Default (Default(def))

import Test.QuickCheck (Arbitrary(arbitrary))

import qualified Kontiki.Raft.Classes.State.Volatile as K

import Kontiki.Protocol.Server (Index)

data VolatileState = VolatileState { volatileStateCommitIndex :: {-# UNPACK #-} !Index
                                   , volatileStateLastApplied :: {-# UNPACK #-} !Index
                                   }
    deriving (Eq, Show)

instance K.VolatileState VolatileState where
    type Index VolatileState = Index

    commitIndex = lens volatileStateCommitIndex (\s i -> s { volatileStateCommitIndex = i })
    lastApplied = lens volatileStateLastApplied (\s i -> s { volatileStateLastApplied = i })

instance Default VolatileState where
    def = VolatileState def def

instance Arbitrary VolatileState where
    arbitrary = VolatileState <$> arbitrary
                              <*> arbitrary
