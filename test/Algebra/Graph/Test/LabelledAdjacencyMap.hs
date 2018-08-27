-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.LabelledAdjacencyMap (
    -- * Testsuite
    testLabelledAdjacencyMap
  ) where

import Algebra.Graph.LabelledAdjacencyMap
import Algebra.Graph.LabelledAdjacencyMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

import qualified Data.Graph as KL
import qualified Data.Set   as Set

t :: Testsuite
t = testsuite "LabelledAdjacencyMap." (empty :: LabelledAdjacencyMap Int Bool)

type LAI = LabelledAdjacencyMap Int Bool

testLabelledAdjacencyMap :: IO ()
testLabelledAdjacencyMap = do
    putStrLn "\n============ LabelledAdjacencyMap ============"
    test "Axioms of graphs" (axioms :: GraphTestsuite LAI)

    test "Consistency of arbitraryLabelledAdjacencyMap" $ \(m :: LAI) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs :: LAI)

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
    testDfs               t
    testTopSort           t
    testIsTopSort         t

    putStrLn "\n============ LabelledAdjacencyMap.scc ============"
    test "scc empty               == empty" $
          scc(empty :: LAI)        == empty

    test "scc (vertex x)          == vertex (Set.singleton x)" $ \(x :: Int) ->
          scc (vertex x :: LAI)          == vertex (Set.singleton x)

    test "scc (edge x y)          == edge (Set.singleton x) (Set.singleton y)" $ \(x :: Int) y ->
          scc (edge x y :: LAI)          == edge (Set.singleton x) (Set.singleton y)

    test "scc (circuit (1:xs))    == edge (Set.fromList (1:xs)) (Set.fromList (1:xs))" $ \(xs :: [Int]) ->
          scc (circuit (1:xs):: LAI)    == edge (Set.fromList (1:xs)) (Set.fromList (1:xs))

    test "scc (3 * 1 * 4 * 1 * 5) == <correct result>" $
          scc (3 * 1 * 4 * 1 * 5 :: LAI) == edges [ (Set.fromList [1,4], Set.fromList [1,4])
                                           , (Set.fromList [1,4], Set.fromList [5]  )
                                           , (Set.fromList [3]  , Set.fromList [1,4])
                                           , (Set.fromList [3]  , Set.fromList [5 :: Int])]

    putStrLn "\n============ LabelledAdjacencyMap.Internal.GraphKL ============"
    test "map (fromVertexKL h) (vertices $ toGraphKL h) == vertexList g"
      $ \(g :: LAI) -> let h = mkGraphKL (labelledAdjacencyMap g) in
          map (fromVertexKL h) (KL.vertices $ toGraphKL h) == vertexList g

    test "map (\\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (edges $ toGraphKL h) == edgeList g"
      $ \(g :: LAI) -> let h = mkGraphKL (labelledAdjacencyMap g) in
          map ( \(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (KL.edges $ toGraphKL h) == edgeList g
