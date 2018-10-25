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

import Data.Monoid

import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Labelled.AdjacencyMap.Internal
import Algebra.Graph.Test

type LAI = AdjacencyMap Any Int

testLabelledAdjacencyMap :: IO ()
testLabelledAdjacencyMap = do
    putStrLn "\n============ Labelled.AdjacencyMap ============"
    test "Consistency of arbitraryLabelledAdjacencyMap" $ \(m :: LAI) ->
        consistent m

    test "Consistency of fromAdjacencyMaps" $ \xs ->
        consistent (fromAdjacencyMaps xs :: LAI)

    putStrLn "\n============ Labelled.AdjacencyMap.replaceEdge ============"

    test "replaceEdge e x y m == overlay (removeEdge x y m) (edge e x y)" $ \(e :: Sum Int) (x :: Int) (y :: Int) m ->
        replaceEdge e x y m == overlay (removeEdge x y m) (edge e x y)

    test "edgeLabel x y (replaceEdge e x y m) == e" $ \(e :: Sum Int) (x :: Int) (y :: Int) m ->
          edgeLabel x y (replaceEdge e x y m) == e

    test "replaceEdge e2 x y (edge e1 x y) == edge e2 x y" $ \(e1 :: Sum Int) (e2 :: Sum Int) (x :: Int) (y :: Int) ->
          replaceEdge e2 x y (edge e1 x y) == edge e2 x y