{-# LANGUAGE DeriveGeneric #-}

module Kontiki.Types (
      Index(..)
    , Term(..)
    , Node(..)
    ) where

import Data.Word (Word64)
import Data.Text.Arbitrary ()
import Data.Text.Lazy (Text, fromStrict, toStrict)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Default (Default(def))

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

import qualified Kontiki.Raft.Classes.Types as K

newtype Term = Term { getTerm :: Word64 }
    deriving (Eq, Ord, Show, Generic)

instance Binary Term

instance K.Term Term where
    term0 = Term 0

instance Arbitrary Term where
    arbitrary = Term <$> arbitrary
    shrink = map Term . shrink . getTerm


newtype Index = Index { getIndex :: Word64 }
    deriving (Eq, Ord, Show, Generic)

instance Binary Index

instance K.Index Index where
    index0 = Index 0
    succIndex = Index . succ . getIndex

instance Arbitrary Index where
    arbitrary = Index <$> arbitrary
    shrink = map Index . shrink . getIndex

instance Default Index where
    def = Index def


newtype Node = Node { getNode :: Text }
    deriving (Eq, Show, Generic)

instance Binary Node

instance Arbitrary Node where
    arbitrary = (Node . fromStrict) <$> arbitrary
    shrink = map (Node . fromStrict) . shrink . toStrict . getNode
