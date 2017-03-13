{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Arbitrary
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Generators and orphan Arbitrary instances for various graph data types.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Arbitrary (
    -- * Generators of arbitrary graph instances
    arbitraryGraph, arbitraryRelation, arbitraryAdjacencyMap, arbitraryIntAdjacencyMap
  ) where

import Test.QuickCheck

import Algebra.Graph hiding (edges)
import Algebra.Graph.AdjacencyMap.Internal hiding (edges)
import Algebra.Graph.Data hiding (Graph)
import Algebra.Graph.IntAdjacencyMap.Internal (IntAdjacencyMap)
import Algebra.Graph.Relation.Internal

import qualified Algebra.Graph.Data                     as Data
import qualified Algebra.Graph.IntAdjacencyMap.Internal as Int
import qualified Data.Set                               as Set

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
arbitraryRelation = edges <$> arbitrary

-- | Generate an arbitrary 'AdjacencyMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryAdjacencyMap :: (Arbitrary a, Ord a) => Gen (AdjacencyMap a)
arbitraryAdjacencyMap = fromAdjacencyList <$> arbitrary

-- | Generate an arbitrary 'IntAdjacencyMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryIntAdjacencyMap :: Gen IntAdjacencyMap
arbitraryIntAdjacencyMap = Int.fromAdjacencyList <$> arbitrary

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

instance (Arbitrary a, Ord a) => Arbitrary (AdjacencyMap a) where
    arbitrary = arbitraryAdjacencyMap

instance Arbitrary IntAdjacencyMap where
    arbitrary = arbitraryIntAdjacencyMap
