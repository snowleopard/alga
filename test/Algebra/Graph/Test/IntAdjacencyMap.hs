-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.IntAdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'IntAdjacencyMap'.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.IntAdjacencyMap (
    -- * Testsuite
    testIntAdjacencyMap
  ) where

import Algebra.Graph.Class hiding (edges)
import Algebra.Graph.IntAdjacencyMap.Internal
import Algebra.Graph.Test

testIntAdjacencyMap :: IO ()
testIntAdjacencyMap = do
    putStrLn "\n============ IntAdjacencyMap ============"
    test "Axioms of graphs" $ (axioms :: GraphTestsuite IntAdjacencyMap)

    test "Consistency of arbitraryIntAdjacencyMap" $ \m ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs)

    test "Overlay of fromAdjacencyList" $ \xs ys ->
        fromAdjacencyList xs `overlay` fromAdjacencyList ys == fromAdjacencyList (xs ++ ys)

    test "Inverse of adjacencyList" $ \m ->
        fromAdjacencyList (adjacencyList m) == m

    test "Weak inverse of edges" $ \xs ->
        edgeList (edges xs) == nubOrd (sort xs)
