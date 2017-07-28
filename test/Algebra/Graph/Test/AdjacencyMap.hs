-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.AdjacencyMap (
    -- * Testsuite
    testAdjacencyMap
  ) where

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

import qualified Data.Graph as KL
import qualified Data.Set   as Set

t :: Testsuite
t = testsuite "AdjacencyMap." empty

type AI = AdjacencyMap Int

testAdjacencyMap :: IO ()
testAdjacencyMap = do
    putStrLn "\n============ AdjacencyMap ============"
    test "Axioms of graphs" $ (axioms :: GraphTestsuite AI)

    test "Consistency of arbitraryAdjacencyMap" $ \(m :: AI) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs :: AI)

    testShow              t
    testBasicPrimitives   t
    testFromAdjacencyList t
    testIsSubgraphOf      t
    testProperties        t
    testAdjacencyList     t
    testPostSet           t
    testGraphFamilies     t
    testTransformations   t
    testDfsForest         t
    testDfsForestFrom     t
    testTopSort           t
    testIsTopSort         t

    putStrLn "\n============ AdjacencyMap.scc ============"
    test "scc empty               == empty" $
          scc(empty :: AI)        == empty

    test "scc (vertex x)          == vertex (Set.singleton x)" $ \(x :: Int) ->
          scc (vertex x)          == vertex (Set.singleton x)

    test "scc (edge x y)          == edge (Set.singleton x) (Set.singleton y)" $ \(x :: Int) y ->
          scc (edge x y)          == edge (Set.singleton x) (Set.singleton y)

    test "scc (circuit (1:xs))    == edge (Set.fromList (1:xs)) (Set.fromList (1:xs))" $ \(xs :: [Int]) ->
          scc (circuit (1:xs))    == edge (Set.fromList (1:xs)) (Set.fromList (1:xs))

    test "scc (3 * 1 * 4 * 1 * 5) == <correct result>" $
          scc (3 * 1 * 4 * 1 * 5) == edges [ (Set.fromList [1,4], Set.fromList [1,4])
                                           , (Set.fromList [1,4], Set.fromList [5]  )
                                           , (Set.fromList [3]  , Set.fromList [1,4])
                                           , (Set.fromList [3]  , Set.fromList [5 :: Int])]

    putStrLn "\n============ AdjacencyMap.Internal.GraphKL ============"
    test "map (fromVertexKL h) (vertices $ toGraphKL h) == vertexList g"
      $ \(g :: AI) -> let h = mkGraphKL (adjacencyMap g) in
          map (fromVertexKL h) (KL.vertices $ toGraphKL h) == vertexList g

    test "map (\\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (edges $ toGraphKL h) == edgeList g"
      $ \(g :: AI) -> let h = mkGraphKL (adjacencyMap g) in
          map ( \(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (KL.edges $ toGraphKL h) == edgeList g
