{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Graph.Test.Arbitrary
-- Copyright   :  (c) Andrey Mokhov 2016-2017
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  experimental
--
-- This module defines generators and orphan Arbitrary instances for various
-- graph data types.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Arbitrary (
    -- * Generators of arbitrary graph instances
    arbitraryGraph, arbitraryRelation
  ) where

import qualified Data.Set as Set
import Test.QuickCheck

import Algebra.Graph
import qualified Algebra.Graph.Data as Data
import Algebra.Graph.Data hiding (Graph)
import Algebra.Graph.Relation.Internal
import Algebra.Graph.AdjacencyMap.Internal

-- | Generate an arbitrary 'Graph' value of a specified size.
arbitraryGraph :: (Graph g, Arbitrary (Vertex g)) => Gen g
arbitraryGraph = sized expr
  where
    expr 0 = return empty
    expr 1 = vertex <$> arbitrary
    expr n = do
        left <- choose (0, n)
        oneof [ overlay <$> (expr left) <*> (expr $ n - left)
              , connect <$> (expr left) <*> (expr $ n - left) ]

instance Arbitrary a => Arbitrary (Data.Graph a) where
    arbitrary = arbitraryGraph

    shrink Empty         = []
    shrink (Vertex    _) = [Empty]
    shrink (Overlay x y) = [Empty, x, y]
                        ++ [Overlay x' y' | (x', y') <- shrink (x, y) ]
    shrink (Connect x y) = [Empty, x, y, Overlay x y]
                        ++ [Connect x' y' | (x', y') <- shrink (x, y) ]

-- | Generate an arbitrary 'Relation'.
arbitraryRelation :: (Arbitrary a, Ord a) => Gen (Relation a)
arbitraryRelation = do
    r <- arbitrary
    let (xs, ys) = unzip $ Set.toAscList r
    return $ Relation (Set.fromList $ xs ++ ys) r

-- TODO: Implement a custom shrink method.
instance (Arbitrary a, Ord a) => Arbitrary (Relation a) where
    arbitrary = arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (ReflexiveRelation a) where
    arbitrary = ReflexiveRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (SymmetricRelation a) where
    arbitrary = SymmetricRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (TransitiveRelation a) where
    arbitrary = TransitiveRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (PreorderRelation a) where
    arbitrary = PreorderRelation <$> arbitraryRelation

deriving instance (Arbitrary a, Ord a) => Arbitrary (AdjacencyMap a)
