-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Labelled.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Labelled.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Labelled.AdjacencyMap (
    -- * Testsuite
    testLabelledAdjacencyMap
  ) where

import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Labelled.AdjacencyMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

t :: Testsuite
t = testsuite "Labelled.AdjacencyMap." (empty :: AdjacencyMap Int Bool)

type LAI = AdjacencyMap Int Bool

testLabelledAdjacencyMap :: IO ()
testLabelledAdjacencyMap = do
    putStrLn "\n============ Labelled.AdjacencyMap ============"
    test "Axioms of graphs" (axioms :: GraphTestsuite LAI)

    test "Consistency of arbitraryLabelledAdjacencyMap" $ \(m :: LAI) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencySets xs :: LAI)

    testShow              t
    testBasicPrimitives   t
    testFromAdjacencySets t
    testIsSubgraphOf      t
    testToGraph           t
    testGraphFamilies     t
    testTransformations   t
    testDfsForest         t
    testDfsForestFrom     t
    testDfs               t
    testReachable         t
    testTopSort           t
    testIsTopSortOf       t
    testIsAcyclic         t
