{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Arbitrary
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Generators and orphan Arbitrary instances for various data types.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Arbitrary (
    -- * Generators of arbitrary graph instances
    arbitraryGraph, arbitraryRelation, arbitraryAdjacencyMap, arbitraryAdjacencyIntMap
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad
import Data.Tree
import Test.QuickCheck

import Algebra.Graph
import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.Export
import Algebra.Graph.Fold (Fold)
import Algebra.Graph.AdjacencyIntMap.Internal
import Algebra.Graph.Relation.Internal
import Algebra.Graph.Relation.InternalDerived

import qualified Algebra.Graph.AdjacencyMap    as AdjacencyMap
import qualified Algebra.Graph.Class           as C
import qualified Algebra.Graph.AdjacencyIntMap as AdjacencyIntMap
import qualified Algebra.Graph.NonEmpty        as NE
import qualified Algebra.Graph.Relation        as Relation

-- | Generate an arbitrary 'Graph' value of a specified size.
arbitraryGraph :: (C.Graph g, Arbitrary (C.Vertex g)) => Gen g
arbitraryGraph = sized expr
  where
    expr 0 = return C.empty
    expr 1 = C.vertex <$> arbitrary
    expr n = do
        left <- choose (0, n)
        oneof [ C.overlay <$> expr left <*> expr (n - left)
              , C.connect <$> expr left <*> expr (n - left) ]

instance Arbitrary a => Arbitrary (Graph a) where
    arbitrary = arbitraryGraph

    shrink Empty         = []
    shrink (Vertex    _) = [Empty]
    shrink (Overlay x y) = [Empty, x, y]
                        ++ [Overlay x' y' | (x', y') <- shrink (x, y) ]
    shrink (Connect x y) = [Empty, x, y, Overlay x y]
                        ++ [Connect x' y' | (x', y') <- shrink (x, y) ]

-- | Generate an arbitrary 'NonEmptyGraph' value of a specified size.
arbitraryNonEmptyGraph :: Arbitrary a => Gen (NE.NonEmptyGraph a)
arbitraryNonEmptyGraph = sized expr
  where
    expr 0 = NE.vertex <$> arbitrary -- can't generate non-empty graph of size 0
    expr 1 = NE.vertex <$> arbitrary
    expr n = do
        left <- choose (1, n)
        oneof [ NE.overlay <$> expr left <*> expr (n - left)
              , NE.connect <$> expr left <*> expr (n - left) ]

instance Arbitrary a => Arbitrary (NE.NonEmptyGraph a) where
    arbitrary = arbitraryNonEmptyGraph

    shrink (NE.Vertex    _) = []
    shrink (NE.Overlay x y) = [x, y]
                           ++ [NE.Overlay x' y' | (x', y') <- shrink (x, y) ]
    shrink (NE.Connect x y) = [x, y, NE.Overlay x y]
                           ++ [NE.Connect x' y' | (x', y') <- shrink (x, y) ]

-- | Generate an arbitrary 'Relation'.
arbitraryRelation :: (Arbitrary a, Ord a) => Gen (Relation a)
arbitraryRelation = Relation.stars <$> arbitrary

-- | Generate an arbitrary 'AdjacencyMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryAdjacencyMap :: (Arbitrary a, Ord a) => Gen (AdjacencyMap a)
arbitraryAdjacencyMap = AdjacencyMap.stars <$> arbitrary

-- | Generate an arbitrary 'AdjacencyIntMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryAdjacencyIntMap :: Gen AdjacencyIntMap
arbitraryAdjacencyIntMap = AdjacencyIntMap.stars <$> arbitrary

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

instance Arbitrary AdjacencyIntMap where
    arbitrary = arbitraryAdjacencyIntMap

instance Arbitrary a => Arbitrary (Fold a) where
    arbitrary = arbitraryGraph

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized go
      where
        go 0 = do
            root <- arbitrary
            return $ Node root []
        go n = do
            subTrees <- choose (0, n - 1)
            let subSize = (n - 1) `div` subTrees
            root     <- arbitrary
            children <- replicateM subTrees (go subSize)
            return $ Node root children

instance Arbitrary s => Arbitrary (Doc s) where
    arbitrary = (mconcat . map literal) <$> arbitrary
