module Kontiki
  ( Term,
    Index,
    NodeId (..),
  )
where

import Data.UUID (UUID, fromWords, toWords)
import Data.Word (Word64)
import qualified Kontiki.Raft.Class as K
import Test.QuickCheck (Arbitrary (..))

newtype Term = Term {unTerm :: Word64}
  deriving (Show, Eq, Ord)

instance K.Term Term where
  term0 = Term 0
  succTerm (Term t) = Term (succ t)

instance Arbitrary Term where
  arbitrary = Term <$> arbitrary
  shrink (Term i) = [Term j | j <- shrink i]

newtype Index = Index {unIndex :: Word64}
  deriving (Show, Eq, Ord)

instance K.Index Index where
  index0 = Index 0
  succIndex (Index i) = Index (succ i)

instance Arbitrary Index where
  arbitrary = Index <$> arbitrary
  shrink (Index i) = [Index j | j <- shrink i]

newtype NodeId = NodeId {unNodeId :: UUID}
  deriving (Show, Eq, Ord)

instance Arbitrary NodeId where
  arbitrary = (\(a, b, c, d) -> NodeId (fromWords a b c d)) <$> arbitrary
  shrink (NodeId i) = [NodeId (fromWords a b c d) | (a, b, c, d) <- shrink (toWords i)]
