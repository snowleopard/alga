-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.IntAdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.IntAdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.IntAdjacencyMap (
    -- * Testsuite
    testIntAdjacencyMap
  ) where

import Algebra.Graph.IntAdjacencyMap
import Algebra.Graph.IntAdjacencyMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

import qualified Data.Graph  as KL
import qualified Data.IntSet as IntSet

t :: Testsuite
t = testsuite "IntAdjacencyMap." empty

testIntAdjacencyMap :: IO ()
testIntAdjacencyMap = do
    putStrLn "\n============ IntAdjacencyMap ============"
    test "Axioms of graphs" $ (axioms :: GraphTestsuite IntAdjacencyMap)

    test "Consistency of arbitraryAdjacencyMap" $ \m ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs)

    testShow              t
    testBasicPrimitives   t
    testFromAdjacencyList t
    testIsSubgraphOf      t
    testProperties        t
    testAdjacencyList     t
    testPostIntSet        t
    testGraphFamilies     t
    testTransformations   t
    testDfsForest         t
    testDfsForestFrom     t
    testTopSort           t
    testIsTopSort         t

    putStrLn "\n============ IntAdjacencyMap.Internal.GraphKL ============"
    test "map (fromVertexKL h) (vertices $ toGraphKL h) == IntSet.toAscList (vertexIntSet g)"
      $ \g -> let h = mkGraphKL (adjacencyMap g) in
        map (fromVertexKL h) (KL.vertices $ toGraphKL h) == IntSet.toAscList (vertexIntSet g)

    test "map (\\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (edges $ toGraphKL h) == edgeList g"
      $ \g -> let h = mkGraphKL (adjacencyMap g) in
        map (\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (KL.edges $ toGraphKL h) == edgeList g
