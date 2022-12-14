{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Class
  ( Term (..),
    termLaws,
    Index (..),
    indexLaws,
    MonadRaftIO (..),
    tests,
  )
where

import Data.Proxy (Proxy)
import Test.Hspec (Spec, context, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, property)
import Test.QuickCheck.Classes.Base (Laws (..), eqLaws, ordLaws)

class Ord t => Term t where
  term0 :: t
  succTerm :: t -> t

termLaws :: forall a proxy. (Term a, Arbitrary a, Show a) => proxy a -> Laws
termLaws _ =
  Laws
    "Term"
    [ ("∀t. term0 <= t", property $ \t -> term0 @a <= t),
      ("∀t. succTerm t > t", property $ \t -> succTerm @a t > t)
    ]

class Ord i => Index i where
  index0 :: i
  succIndex :: i -> i

indexLaws :: forall a proxy. (Index a, Arbitrary a, Show a) => proxy a -> Laws
indexLaws _ =
  Laws
    "Index"
    [ ("∀i. index0 <= i", property $ \i -> index0 @a <= i),
      ("∀i. succIndex i > i", property $ \i -> succIndex @a i > i)
    ]

class (Monad m, Term (MTerm m), Index (MIndex m), Ord (MNodeId m)) => MonadRaftIO m where
  type MTerm m
  type MIndex m
  type MNodeId m

  getCurrentTerm :: m (MTerm m)
  getVotedFor :: m (Maybe (MNodeId m))

  setPersistentState :: MTerm m -> Maybe (MNodeId m) -> m ()

tests :: (Term term, Arbitrary term, Show term, Index index, Arbitrary index, Show index) => Proxy term -> Proxy index -> Spec
tests term index = do
  describe "Term" $ lawsCheckOne term [eqLaws, ordLaws, termLaws]
  describe "Index" $ lawsCheckOne index [eqLaws, ordLaws, indexLaws]
  where
    lawsCheckOne p ls = sequence_ [lawsCheck (mkLaws p) | mkLaws <- ls]
    lawsCheck (Laws c ps) = context c $ sequence_ [uncurry prop p | p <- ps]
