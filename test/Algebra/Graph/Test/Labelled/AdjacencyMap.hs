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
