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
    testTopSort           t
    testIsTopSort         t

    putStrLn "\n============ IntAdjacencyMap.GraphKL ============"
    test "map (getVertex h) (vertices $ getGraph h) == IntSet.toAscList (vertexIntSet g)"
      $ \g -> let h = graphKL g in
        map (getVertex h) (KL.vertices $ getGraph h) == IntSet.toAscList (vertexIntSet g)

    test "map (\\(x, y) -> (getVertex h x, getVertex h y)) (edges $ getGraph h) == edgeList g"
      $ \g -> let h = graphKL g in
        map (\(x, y) -> (getVertex h x, getVertex h y)) (KL.edges $ getGraph h) == edgeList g

    test "fromGraphKL . graphKL == id" $ \x ->
        (fromGraphKL . graphKL) x == x
