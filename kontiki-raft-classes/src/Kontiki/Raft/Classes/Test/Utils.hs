{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kontiki.Raft.Classes.Test.Utils (
      typeId
    , PropLensConstraints
    , prop_lens
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep, typeRepTyCon, tyConModule, tyConName)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck (Arbitrary)

import Kontiki.Raft.Classes.Lens (Lens', set, view)

typeId :: forall a. Typeable a => String
typeId = concat [tyConModule con, ".", tyConName con]
  where
    con = typeRepTyCon $ typeRep (Proxy :: Proxy a)


type PropLensConstraints s v = (Arbitrary s, Eq s, Show s, Arbitrary v, Eq v, Show v)

prop_lens :: forall s v.
             ( Arbitrary s, Eq s, Show s
             , Arbitrary v, Eq v, Show v
             )
          => String
          -> Lens' s v
          -> TestTree
prop_lens n l = testGroup ("prop_lens " ++ n) [
      testProperty (unwords ["view", n, "(set", n, "v s) ≡ v"]) $ \s v -> view l (set l v s) == v
    , testProperty (unwords ["set", n, "(view", n, "s) s ≡ s"]) $ \s -> set l (view l s) s == s
    , testProperty (unwords ["set", n, "v' (set", n, "v s) ≡ set", n, "v' s"]) $ \s v v' -> set l v' (set l v s) == set l v' s
    ]
