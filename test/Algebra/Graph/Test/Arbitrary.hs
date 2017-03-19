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

import Algebra.Graph
import Algebra.Graph.AdjacencyMap.Internal (AdjacencyMap (..))
import Algebra.Graph.Fold.Internal (Fold (..))
import Algebra.Graph.IntAdjacencyMap.Internal (IntAdjacencyMap (..))
import Algebra.Graph.Relation.Internal (Relation (..))

import qualified Algebra.Graph.Class                    as C
import qualified Algebra.Graph.AdjacencyMap.Internal    as AdjacencyMap
import qualified Algebra.Graph.IntAdjacencyMap.Internal as IntAdjacencyMap
import qualified Algebra.Graph.Relation.Internal        as Relation

-- | Generate an arbitrary 'Graph' value of a specified size.
arbitraryGraph :: (C.Graph g, Arbitrary (C.Vertex g)) => Gen g
arbitraryGraph = sized expr
  where
    expr 0 = return C.empty
    expr 1 = C.vertex <$> arbitrary
    expr n = do
        left <- choose (0, n)
        oneof [ C.overlay <$> (expr left) <*> (expr $ n - left)
              , C.connect <$> (expr left) <*> (expr $ n - left) ]

instance Arbitrary a => Arbitrary (Graph a) where
    arbitrary = arbitraryGraph

    shrink Empty         = []
    shrink (Vertex    _) = [Empty]
    shrink (Overlay x y) = [Empty, x, y]
                        ++ [Overlay x' y' | (x', y') <- shrink (x, y) ]
    shrink (Connect x y) = [Empty, x, y, Overlay x y]
                        ++ [Connect x' y' | (x', y') <- shrink (x, y) ]

-- | Generate an arbitrary 'Relation'.
arbitraryRelation :: (Arbitrary a, Ord a) => Gen (Relation a)
arbitraryRelation = Relation.fromAdjacencyList <$> arbitrary

-- | Generate an arbitrary 'AdjacencyMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryAdjacencyMap :: (Arbitrary a, Ord a) => Gen (AdjacencyMap a)
arbitraryAdjacencyMap = AdjacencyMap.fromAdjacencyList <$> arbitrary

-- | Generate an arbitrary 'IntAdjacencyMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryIntAdjacencyMap :: Gen IntAdjacencyMap
arbitraryIntAdjacencyMap = IntAdjacencyMap.fromAdjacencyList <$> arbitrary

-- TODO: Implement a custom shrink method.
instance (Arbitrary a, Ord a) => Arbitrary (Relation a) where
    arbitrary = arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Relation.ReflexiveRelation a) where
    arbitrary = Relation.ReflexiveRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Relation.SymmetricRelation a) where
    arbitrary = Relation.SymmetricRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Relation.TransitiveRelation a) where
    arbitrary = Relation.TransitiveRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Relation.PreorderRelation a) where
    arbitrary = Relation.PreorderRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (AdjacencyMap a) where
    arbitrary = arbitraryAdjacencyMap

instance Arbitrary IntAdjacencyMap where
    arbitrary = arbitraryIntAdjacencyMap

instance Arbitrary a => Arbitrary (Fold a) where
    arbitrary = arbitraryGraph
