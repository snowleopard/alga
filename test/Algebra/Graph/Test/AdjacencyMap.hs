-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'AdjacencyMap'.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.AdjacencyMap (
    -- * Testsuite
    testAdjacencyMap
  ) where

import Algebra.Graph.Classes
import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.Test

testAdjacencyMap :: IO ()
testAdjacencyMap = do
    putStrLn "\n============ AdjacencyMap ============"
    quickCheck (axioms :: GraphTestsuite (AdjacencyMap Int))

    test "Consistency of arbitraryAdjacencyMap" $ \(m :: AdjacencyMap Int) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \(xs :: [(Int, [Int])]) ->
        consistent (fromAdjacencyList xs)

    test "Overlay of fromAdjacencyList" $ \(xs :: [(Int, [Int])]) ys ->
        fromAdjacencyList xs `overlay` fromAdjacencyList ys == fromAdjacencyList (xs ++ ys)

    test "Inverse of adjacencyList" $ \(m :: AdjacencyMap Int) ->
        fromAdjacencyList (adjacencyList m) == m

    test "Weak inverse of edges" $ \(xs :: [(Int, Int)]) ->
        edgeList (edges xs) == nubOrd (sort xs)
