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
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes)
import Data.Tree
import Test.QuickCheck

import Algebra.Graph
import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.AdjacencyIntMap.Internal
import Algebra.Graph.Export
import Algebra.Graph.Fold (Fold)
import Algebra.Graph.Label
import Algebra.Graph.Relation.InternalDerived
import Algebra.Graph.Relation.Symmetric.Internal

import qualified Algebra.Graph.AdjacencyIntMap       as AdjacencyIntMap
import qualified Algebra.Graph.AdjacencyMap          as AdjacencyMap
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NAM
import qualified Algebra.Graph.Class                 as C
import qualified Algebra.Graph.Fold                  as Fold
import qualified Algebra.Graph.Labelled              as LG
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import qualified Algebra.Graph.NonEmpty              as NonEmpty
import qualified Algebra.Graph.Relation              as Relation
import qualified Algebra.Graph.Relation.Symmetric    as Symmetric

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

instance (Eq a, Ord a, Arbitrary a) => Arbitrary (Fold a) where
    arbitrary = arbitraryGraph

    shrink g = oneLessVertex ++ oneLessEdge
      where
         oneLessVertex =
           let vertices = Fold.vertexList g
           in  [ Fold.removeVertex v g | v <- vertices ]

         oneLessEdge =
           let edges = Fold.edgeList g
           in  [ Fold.removeEdge v w g | (v, w) <- edges ]


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

    shrink g = oneLessVertex ++ oneLessEdge
      where
         oneLessVertex =
           let vertices = Relation.vertexList g
           in  [ Relation.removeVertex v g | v <- vertices ]

         oneLessEdge =
           let edges = Relation.edgeList g
           in  [ Relation.removeEdge v w g | (v, w) <- edges ]


instance (Arbitrary a, Ord a) => Arbitrary (ReflexiveRelation a) where
    arbitrary = ReflexiveRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (Symmetric.Relation a) where
    arbitrary = SR . Relation.symmetricClosure <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (TransitiveRelation a) where
    arbitrary = TransitiveRelation <$> arbitraryRelation

instance (Arbitrary a, Ord a) => Arbitrary (PreorderRelation a) where
    arbitrary = PreorderRelation <$> arbitraryRelation

-- | Generate an arbitrary 'AdjacencyMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryAdjacencyMap :: (Arbitrary a, Ord a) => Gen (AdjacencyMap a)
arbitraryAdjacencyMap = AdjacencyMap.stars <$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (AdjacencyMap a) where
    arbitrary = arbitraryAdjacencyMap

    shrink g = oneLessVertex ++ oneLessEdge
      where
         oneLessVertex =
           let vertices = AdjacencyMap.vertexList g
           in  [ AdjacencyMap.removeVertex v g | v <- vertices ]

         oneLessEdge =
           let edges = AdjacencyMap.edgeList g
           in  [ AdjacencyMap.removeEdge v w g | (v, w) <- edges ]

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

    shrink g = oneLessVertex ++ oneLessEdge
      where
         oneLessVertex =
           let vertices = toList $ NAM.vertexList1 g
           in catMaybes [ NAM.removeVertex1 v g | v <- vertices ]

         oneLessEdge =
           let edges = NAM.edgeList g
           in  [ NAM.removeEdge v w g | (v, w) <- edges ]

-- | Generate an arbitrary 'AdjacencyIntMap'. It is guaranteed that the
-- resulting adjacency map is 'consistent'.
arbitraryAdjacencyIntMap :: Gen AdjacencyIntMap
arbitraryAdjacencyIntMap = AdjacencyIntMap.stars <$> arbitrary

instance Arbitrary AdjacencyIntMap where
    arbitrary = arbitraryAdjacencyIntMap

    shrink g = oneLessVertex ++ oneLessEdge
      where
         oneLessVertex =
           let vertices = AdjacencyIntMap.vertexList g
           in  [ AdjacencyIntMap.removeVertex v g | v <- vertices ]

         oneLessEdge =
           let edges = AdjacencyIntMap.edgeList g
           in  [ AdjacencyIntMap.removeEdge v w g | (v, w) <- edges ]

-- | Generate an arbitrary labelled 'LAM.AdjacencyMap'. It is guaranteed
-- that the resulting adjacency map is 'consistent'.
arbitraryLabelledAdjacencyMap :: (Arbitrary a, Ord a, Eq e, Arbitrary e, Monoid e) => Gen (LAM.AdjacencyMap e a)
arbitraryLabelledAdjacencyMap = LAM.fromAdjacencyMaps <$> arbitrary

instance (Arbitrary a, Ord a, Eq e, Arbitrary e, Monoid e) => Arbitrary (LAM.AdjacencyMap e a) where
    arbitrary = arbitraryLabelledAdjacencyMap

    shrink g = oneLessVertex ++ oneLessEdge
      where
         oneLessVertex =
           let vertices = LAM.vertexList g
           in  [ LAM.removeVertex v g | v <- vertices ]

         oneLessEdge =
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
