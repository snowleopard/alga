-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.IntAdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
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

t :: Testsuite
t = testsuite "IntAdjacencyMap." empty

testIntAdjacencyMap :: IO ()
testIntAdjacencyMap = do
    putStrLn "\n============ IntAdjacencyMap ============"
    test "Axioms of graphs" (axioms :: GraphTestsuite IntAdjacencyMap)

    test "Consistency of arbitraryAdjacencyMap" $ \m ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs)

    testShow                 t
    testBasicPrimitives      t
    testFromAdjacencyList    t
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
