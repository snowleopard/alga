-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.AdjacencyIntMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
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
import Algebra.Graph.AdjacencyIntMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

t :: Testsuite
t = testsuite "AdjacencyIntMap." empty

testAdjacencyIntMap :: IO ()
testAdjacencyIntMap = do
    putStrLn "\n============ AdjacencyIntMap ============"
    test "Axioms of graphs" (axioms :: GraphTestsuite AdjacencyIntMap)

    test "Consistency of arbitraryAdjacencyMap" $ \m ->
        consistent m

    testShow                 t
    testBasicPrimitives      t
    testFromAdjacencyIntSets t
    testIsSubgraphOf         t
    testToGraph              t
    testGraphFamilies        t
    testTransformations      t
    testDfsForest            t
    testDfsForestFrom        t
    testDfs                  t
    testTopSort              t
    testIsTopSort            t
