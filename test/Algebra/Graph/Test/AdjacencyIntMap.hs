-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.AdjacencyIntMap
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.AdjacencyIntMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.AdjacencyIntMap (
    -- * Testsuite
    testAdjacencyIntMap
    ) where

import Algebra.Graph.AdjacencyIntMap
import Algebra.Graph.Test
import Algebra.Graph.Test.API (Mono (..), adjacencyIntMapAPI)
import Algebra.Graph.Test.Generic

import qualified Algebra.Graph.AdjacencyMap as AdjacencyMap

t :: TestsuiteInt (Mono AdjacencyIntMap)
t = ("AdjacencyIntMap.", adjacencyIntMapAPI)

testAdjacencyIntMap :: IO ()
testAdjacencyIntMap = do
    putStrLn "\n============ AdjacencyIntMap ============"
    test "Axioms of graphs" (axioms :: GraphTestsuite AdjacencyIntMap)

    putStrLn $ "\n============ AdjacencyIntMap.fromAdjacencyMap ============"
    test "fromAdjacencyMap == stars . AdjacencyMap.adjacencyList" $ \x ->
          fromAdjacencyMap x == (stars . AdjacencyMap.adjacencyList) x

    testConsistent           t
    testShow                 t
    testBasicPrimitives      t
    testFromAdjacencyIntSets t
    testIsSubgraphOf         t
    testToGraph              t
    testGraphFamilies        t
    testTransformations      t
    testRelational           t
    testBfsForest            t
    testBfsForestFrom        t
    testBfs                  t
    testDfsForest            t
    testDfsForestFrom        t
    testDfs                  t
    testReachable            t
    testTopSort              t
    testIsAcyclic            t
    testIsDfsForestOf        t
    testIsTopSortOf          t
