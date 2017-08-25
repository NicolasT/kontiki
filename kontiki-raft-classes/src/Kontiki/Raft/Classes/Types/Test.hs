{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Raft.Classes.Types.Test (tests) where

import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary)

import Kontiki.Raft.Classes.Test.Utils (typeId)
import qualified Kontiki.Raft.Classes.Types as T

tests :: forall index term.
         ( T.Index index
         , Show index
         , Ord index
         , Arbitrary index
         , Typeable index
         , T.Term term
         , Show term
         , Ord term
         , Arbitrary term
         , Typeable term
         )
      => TestTree
tests = testGroup "Kontiki.Raft.Classes.Types" [
      testGroup ("Term @" ++ typeId @term) [
          testProperty "∀ term. term0 ≤ term" $ \t -> T.term0 <= (t :: term)
        ]
    , testGroup ("Index @" ++ typeId @index) [
          testProperty "∀ index. index0 ≤ index" $ \i -> T.index0 <= (i :: index)
        , testProperty "∀ index. index < succIndex index" $ \i -> (i :: index) < T.succIndex i
        ]
    ]
