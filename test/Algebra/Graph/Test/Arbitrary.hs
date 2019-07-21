{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Arbitrary
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Generators and orphan Arbitrary instances for various data types.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Arbitrary (
    -- * Generators of arbitrary graph instances
    arbitraryGraph, arbitraryRelation, arbitraryAdjacencyMap,
    ) where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes)
import Data.Tree
import Test.QuickCheck

import Algebra.Graph
import Algebra.Graph.Export
import Algebra.Graph.Label

import qualified Algebra.Graph.Acyclic.AdjacencyMap   as AAM
import qualified Algebra.Graph.AdjacencyIntMap        as AIM
import qualified Algebra.Graph.AdjacencyMap           as AM
import qualified Algebra.Graph.Bipartite.AdjacencyMap as BAM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as NAM
import qualified Algebra.Graph.Class                  as C
import qualified Algebra.Graph.Labelled               as LG
import qualified Algebra.Graph.Labelled.AdjacencyMap  as LAM
import qualified Algebra.Graph.NonEmpty               as NonEmpty
import qualified Algebra.Graph.Relation               as Relation
import qualified Algebra.Graph.Relation.Preorder      as Preorder
import qualified Algebra.Graph.Relation.Reflexive     as Reflexive
import qualified Algebra.Graph.Relation.Symmetric     as Symmetric
import qualified Algebra.Graph.Relation.Transitive    as Transitive

-- | Generate an arbitrary 'C.Graph' value of a specified size.
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

-- An Arbitrary instance for Acyclic.AdjacencyMap
instance (Ord a, Arbitrary a) => Arbitrary (AAM.AdjacencyMap a) where
    arbitrary = AAM.toAcyclicOrd <$> arbitrary

    shrink g = shrinkVertices ++ shrinkEdges
      where
        shrinkVertices =
          let vertices = AAM.vertexList g
          in [ AAM.removeVertex x g | x <- vertices ]

        shrinkEdges =
          let edges = AAM.edgeList g
          in [ AAM.removeEdge x y g | (x, y) <- edges ]

-- | Generate an arbitrary 'NonEmpty.Graph' value of a specified size.
arbitraryNonEmptyGraph :: Arbitrary a => Gen (NonEmpty.Graph a)
arbitraryNonEmptyGraph = sized expr
  where
    expr 0 = NonEmpty.vertex <$> arbitrary -- can't generate non-empty graph of size 0
    expr 1 = NonEmpty.vertex <$> arbitrary
    expr n = do
        left <- choose (1, n)
        oneof [ NonEmpty.overlay <$> expr left <*> expr (n - left)
              , NonEmpty.connect <$> expr left <*> expr (n - left) ]

instance Arbitrary a => Arbitrary (NonEmpty.Graph a) where
    arbitrary = arbitraryNonEmptyGraph

    shrink (NonEmpty.Vertex    _) = []
    shrink (NonEmpty.Overlay x y) = [x, y]
        ++ [NonEmpty.Overlay x' y' | (x', y') <- shrink (x, y) ]
    shrink (NonEmpty.Connect x y) = [x, y, NonEmpty.Overlay x y]
        ++ [NonEmpty.Connect x' y' | (x', y') <- shrink (x, y) ]

-- | Generate an arbitrary 'Relation'.
arbitraryRelation :: (Arbitrary a, Ord a) => Gen (Relation.Relation a)
arbitraryRelation = Relation.stars <$> arbitrary

-- TODO: Implement a custom shrink method.
instance (Arbitrary a, Ord a) => Arbitrary (Relation.Relation a) where
    arbitrary = arbitraryRelation

    shrink g = shrinkVertices ++ shrinkEdges
      where
         shrinkVertices =
           let vertices = Relation.vertexList g
           in  [ Relation.removeVertex v g | v <- vertices ]

         shrinkEdges =
           let edges = Relation.edgeList g
           in  [ Relation.removeEdge v w g | (v, w) <- edges ]

-- TODO: Simplify.
instance (Arbitrary a, Ord a) => Arbitrary (Reflexive.ReflexiveRelation a) where
    arbitrary = Reflexive.fromRelation . Relation.reflexiveClosure
        <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Symmetric.Relation a) where
    arbitrary = Symmetric.toSymmetric <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Transitive.TransitiveRelation a) where
    arbitrary = Transitive.fromRelation . Relation.transitiveClosure
        <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Preorder.PreorderRelation a) where
    arbitrary = Preorder.fromRelation . Relation.closure
        <$> arbitraryRelation

-- | Generate an arbitrary 'AdjacencyMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryAdjacencyMap :: (Arbitrary a, Ord a) => Gen (AM.AdjacencyMap a)
arbitraryAdjacencyMap = AM.stars <$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (AM.AdjacencyMap a) where
    arbitrary = arbitraryAdjacencyMap

    shrink g = shrinkVertices ++ shrinkEdges
      where
         shrinkVertices = [ AM.removeVertex v g | v <- AM.vertexList g ]
         shrinkEdges    = [ AM.removeEdge v w g | (v, w) <- AM.edgeList g ]

-- | Generate an arbitrary non-empty 'NAM.AdjacencyMap'. It is guaranteed that
-- the resulting adjacency map is 'consistent'.
arbitraryNonEmptyAdjacencyMap :: (Arbitrary a, Ord a) => Gen (NAM.AdjacencyMap a)
arbitraryNonEmptyAdjacencyMap = NAM.stars1 <$> nonEmpty
  where
    nonEmpty = do
        xs <- arbitrary
        case xs of
            [] -> do
                x <- arbitrary
                return ((x, []) :| []) -- There must be at least one vertex
            (x:xs) -> return (x :| xs)

instance (Arbitrary a, Ord a) => Arbitrary (NAM.AdjacencyMap a) where
    arbitrary = arbitraryNonEmptyAdjacencyMap

    shrink g = shrinkVertices ++ shrinkEdges
      where
         shrinkVertices =
           let vertices = toList $ NAM.vertexList1 g
           in catMaybes [ NAM.removeVertex1 v g | v <- vertices ]

         shrinkEdges =
           let edges = NAM.edgeList g
           in  [ NAM.removeEdge v w g | (v, w) <- edges ]

instance Arbitrary AIM.AdjacencyIntMap where
    arbitrary = AIM.stars <$> arbitrary

    shrink g = shrinkVertices ++ shrinkEdges
      where
         shrinkVertices = [ AIM.removeVertex x g | x <- AIM.vertexList g ]
         shrinkEdges    = [ AIM.removeEdge x y g | (x, y) <- AIM.edgeList g ]

-- | Generate an arbitrary labelled 'LAM.AdjacencyMap'. It is guaranteed
-- that the resulting adjacency map is 'consistent'.
arbitraryLabelledAdjacencyMap :: (Arbitrary a, Ord a, Eq e, Arbitrary e, Monoid e) => Gen (LAM.AdjacencyMap e a)
arbitraryLabelledAdjacencyMap = LAM.fromAdjacencyMaps <$> arbitrary

instance (Arbitrary a, Ord a, Eq e, Arbitrary e, Monoid e) => Arbitrary (LAM.AdjacencyMap e a) where
    arbitrary = arbitraryLabelledAdjacencyMap

    shrink g = shrinkVertices ++ shrinkEdges
      where
         shrinkVertices =
           let vertices = LAM.vertexList g
           in  [ LAM.removeVertex v g | v <- vertices ]

         shrinkEdges =
           let edges = LAM.edgeList g
           in  [ LAM.removeEdge v w g | (_, v, w) <- edges ]

-- | Generate an arbitrary labelled 'LAM.Graph' value of a specified size.
arbitraryLabelledGraph :: (Arbitrary a, Arbitrary e) => Gen (LG.Graph e a)
arbitraryLabelledGraph = sized expr
  where
    expr 0 = return LG.empty
    expr 1 = LG.vertex <$> arbitrary
    expr n = do
        label <- arbitrary
        left  <- choose (0, n)
        LG.connect label <$> expr left <*> expr (n - left)

instance (Arbitrary a, Arbitrary e, Monoid e) => Arbitrary (LG.Graph e a) where
    arbitrary = arbitraryLabelledGraph

    shrink LG.Empty           = []
    shrink (LG.Vertex      _) = [LG.Empty]
    shrink (LG.Connect e x y) = [LG.Empty, x, y, LG.Connect mempty x y]
                             ++ [LG.Connect e x' y' | (x', y') <- shrink (x, y) ]

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

    shrink (Node r fs) = [Node r fs' | fs' <- shrink fs]

-- TODO: Implement a custom shrink method.
instance Arbitrary s => Arbitrary (Doc s) where
    arbitrary = mconcat . map literal <$> arbitrary

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Distance a) where
    arbitrary = (\x -> if x < 0 then distance infinite else distance (unsafeFinite x)) <$> arbitrary

instance (Arbitrary a, Arbitrary b, Ord a, Ord b) => Arbitrary (BAM.AdjacencyMap a b) where
    arbitrary = BAM.toBipartite <$> arbitrary
    shrink = map BAM.toBipartite . shrink . BAM.fromBipartite
