-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Undirected
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Undirected".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Undirected (
    -- * Testsuite
    testUndirected
    ) where

import Algebra.Graph.Undirected
import Algebra.Graph.Test
import Algebra.Graph.Test.API (toIntAPI, undirectedGraphAPI)
import Algebra.Graph.Test.Generic

tPoly :: Testsuite Graph Ord
tPoly = ("Graph.Undirected.", undirectedGraphAPI)

t :: TestsuiteInt Graph
t = fmap toIntAPI tPoly

type G = Graph Int

testUndirected :: IO ()
testUndirected = do
    putStrLn "\n============ Graph.Undirected ============"
    test "Axioms of undirected graphs" $
        size10 (undirectedAxioms :: GraphTestsuite G)

    testConsistent    t
    testSymmetricShow t

    testSymmetricBasicPrimitives t
    testSymmetricIsSubgraphOf    t
    testSymmetricGraphFamilies   t
    testSymmetricTransformations t
    testInduceJust               tPoly

