-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Undirected
-- Copyright  : (c) Andrey Mokhov 2016-2022
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

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Undirected as U

tPoly :: Testsuite Graph Ord
tPoly = ("Graph.Undirected.", undirectedGraphAPI)

t :: TestsuiteInt Graph
t = fmap toIntAPI tPoly

type G = Graph Int
type UGI = U.Graph Int
type AGI = G.Graph Int

testUndirected :: IO ()
testUndirected = do
    putStrLn "\n============ Graph.Undirected ============"
    test "Axioms of undirected graphs" $ size10 $ undirectedAxioms @G

    testSymmetricShow t

    putStrLn $ "\n============ Graph.Undirected.toUndirected ============"
    test "toUndirected (edge 1 2)         == edge 1 2" $
          toUndirected (G.edge 1 2)       == edge 1 (2 :: Int)

    test "toUndirected . fromUndirected   == id" $ \(x :: G) ->
          (toUndirected . fromUndirected) x == id x

    test "vertexCount      . toUndirected == vertexCount" $ \(x :: AGI) ->
          vertexCount (toUndirected x) == G.vertexCount x

    test "(*2) . edgeCount . toUndirected >= edgeCount" $ \(x :: AGI) ->
          ((*2) . edgeCount . toUndirected) x >= G.edgeCount x

    putStrLn $ "\n============ Graph.Undirected.fromUndirected ============"
    test "fromUndirected (edge 1 2)    == edges [(1,2),(2,1)]" $
          fromUndirected (edge 1 2)    == G.edges [(1,2), (2,1 :: Int)]

    test "toUndirected . fromUndirected == id" $ \(x :: G) ->
          (toUndirected . fromUndirected) x == id x

    test "vertexCount . fromUndirected == vertexCount" $ \(x :: G) ->
          (G.vertexCount . fromUndirected) x == vertexCount x

    test "edgeCount   . fromUndirected <= (*2) . edgeCount" $ \(x :: G) ->
          (G.edgeCount . fromUndirected) x <= ((*2) . edgeCount) x

    putStrLn $ "\n============ Graph.Undirected.complement ================"
    test "complement empty              == empty" $
          complement (empty :: UGI)     == empty

    test "complement (vertex x)         == vertex x" $ \x ->
          complement (vertex x :: UGI)  == vertex x

    test "complement (edge 1 1)         == edge 1 1" $
          complement (edge 1 1)         == edge 1 (1 :: Int)

    test "complement (edge 1 2)         == vertices [1, 2]" $
          complement (edge 1 2 :: UGI)  == vertices [1, 2]

    test "complement (star 1 [2, 3])    == overlay (vertex 1) (edge 2 3)" $
          complement (star 1 [2, 3])    == overlay (vertex 1) (edge 2 3 :: UGI)

    test "complement . complement       == id" $ \(x :: UGI) ->
         (complement . complement $ x)  == x

    testSymmetricBasicPrimitives t
    testSymmetricIsSubgraphOf    t
    testSymmetricGraphFamilies   t
    testSymmetricTransformations t
    testInduceJust               tPoly
