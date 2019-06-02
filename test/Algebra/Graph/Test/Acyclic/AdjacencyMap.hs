-------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Acyclic.AdjacencyMap
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Acyclic.AdjacencyMap".
-------------------------------------------------------------

module Algebra.Graph.Test.Acyclic.AdjacencyMap (
  testAcyclicAdjacencyMap 
  ) where

import Algebra.Graph.Acyclic.AdjacencyMap
import Algebra.Graph.Test
import Algebra.Graph ()
import Data.List.NonEmpty

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.List as List

-- t :: Testsuite
-- t = testsuite "AcyclicAdjacencyMap." empty

type AAI = AdjacencyMap Int
type AI = AM.AdjacencyMap Int

testAcyclicAdjacencyMap :: IO ()
testAcyclicAdjacencyMap = do
  putStrLn "\n=====AcyclicAdjacencyMap Num instance====="
  test "edgeList    0                 == []" $
        edgeList   (0 :: AAI)         == []
  test "vertexList  0                 == [0]" $
        vertexList (0 :: AAI)         == [0]
  test "edgeList   (1 + 2)            == []" $
        edgeList   (1 + 2 :: AAI)     == []
  test "vertexList (1 + 2)            == [1,2]" $
        vertexList (1 + 2 :: AAI)     == [1,2]
  test "edgeList   (1 * 2)            == [(1,2)]" $
        edgeList   (1 * 2 :: AAI)     == [(1,2)]
  test "vertexList (1 * 2)            == [1,2]" $
        vertexList (1 * 2 :: AAI)     == [1,2]
  test "edgeList   (1 + 2 * 3)        == [(2,3)]" $
        edgeList   (1 + 2 * 3 :: AAI) == [(2,3)]
  test "vertexList (1 + 2 * 3)        == [1,2,3]" $
        vertexList (1 + 2 * 3 :: AAI) == [1,2,3]
  test "edgeList   (1 * 2 + 3)        == [(1,2)]" $
        edgeList   (1 * 2 + 3 :: AAI) == [(1,2)]
  test "vertexList (1 * 2 + 3)        == [1,2,3]" $
        vertexList (1 * 2 + 3 :: AAI) == [1,2,3]

  putStrLn "\n=====AcyclicAdjacencyMap consistency====="

  test "consistent empty                  == True" $
        consistent (empty :: AAI)         == True
  test "consistent (1 + 2)                == True" $
        consistent (1 + 2 :: AAI)         == True
  test "consistent (1 * 2 + 2 * 3)        == True" $
        consistent (1 * 2 + 2 * 3 :: AAI) == True

  putStrLn "\n=====AcyclicAdjacencyMap construction primitives====="

  test "vertex 1 == 1" $
        vertex 1 == (1 :: AAI)

  test "vertices []        == empty" $
        vertices []        == (empty :: AAI)
  test "vertices [1]       == vertex 1" $
        vertices [1]       == (vertex 1 :: AAI)
  test "vertices [1, 2, 3] == 1 + 2 + 3" $
        vertices [1, 2, 3] == (1 + 2 + 3 :: AAI)

  test "edgeList (overlayD empty empty)                           == []" $
        edgeList (overlayD (empty :: AAI) (empty :: AAI))         == []
  test "edgeList (overlayD (1 * 2 + 1 * 3) (1 * 2))               == [(Left 1,Left 2),(Left 1,Left 3),(Right 1,Right 2)]" $
        edgeList (overlayD (1 * 2 + 1 * 3 :: AAI) (1 * 2 :: AAI)) == [(Left 1,Left 2),(Left 1,Left 3),(Right 1,Right 2)]

  test "edgeList (connectD empty empty)                   == []" $
        edgeList (connectD (empty :: AAI) (empty :: AAI)) == []
  test "edgeList (connectD (1 + 2) (1 + 2))               == [(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2)]" $
        edgeList (connectD (1 + 2 :: AAI) (1 + 2 :: AAI)) == [(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2)]
  test "edgeList (connectD (1 * 2) (1 * 2))               == [(Left 1,Left 2),(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2),(Right 1,Right 2)]" $
        edgeList (connectD (1 * 2 :: AAI) (1 * 2 :: AAI)) == [(Left 1,Left 2),(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2),(Right 1,Right 2)]

  test "edgeList (box (1 * 2) (3 * 4))                 == [((1,3),(1,4)),((1,3),(2,3)),((1,4),(2,4)),((2,3),(2,4))]" $
        edgeList (box (1 * 2 :: AAI) (3 * 4 :: AAI))   == [((1,3),(1,4)),((1,3),(2,3)),((1,4),(2,4)),((2,3),(2,4))]
  test "edgeList (box (1 + 2) (3 + 4))                 == []" $
        edgeList (box (1 + 2 :: AAI) (3 + 4 :: AAI))   == []
  test "vertexList (box (1 + 2) (3 + 4))               == [(1,3),(1,4),(2,3),(2,4)]" $
        vertexList (box (1 + 2 :: AAI) (3 + 4 :: AAI)) == [(1,3),(1,4),(2,3),(2,4)]

  putStrLn "\n=====AcyclicAdjacencyMap transitiveClosure====="

  test "transitiveClosure empty                  == empty" $
        transitiveClosure (empty :: AAI)         == (empty :: AAI)
  test "transitiveClosure (vertex 5)             == vertex 5" $
        transitiveClosure (vertex 5 :: AAI)      == (vertex 5 :: AAI)
  test "transitiveClosure (1 * 2 + 2 * 3)        == 1 * 2 + 2 * 3 + 1 * 3" $
        transitiveClosure (1 * 2 + 2 * 3 :: AAI) == (1 * 2 + 2 * 3 + 1 * 3 :: AAI)

  putStrLn "\n=====AcyclicAdjacencyMap topsort====="

  test "topSort (1)                    == [1]" $
        topSort (1 :: AAI)             == [1]
  test "topSort (1 * 2 + 3 * 1)        == [3,1,2]" $
        topSort (1 * 2 + 3 * 1 :: AAI) == [3,1,2]

  putStrLn "\n=====AcyclicAdjacencyMap fromGraph primitive====="

  test "fromGraph (<) (2 * 1)         == 1 + 2" $
        fromGraph (<) (2 * 1)         == (1 + 2 :: AAI)
  test "fromGraph (<) (1 * 2)         == 1 * 2" $
        fromGraph (<) (1 * 2)         == (1 * 2 :: AAI)
  test "fromGraph (<) (1 * 2 + 2 * 1) == 1 * 2" $
        fromGraph (<) (1 * 2 + 2 * 1) == (1 * 2 :: AAI)

  putStrLn "\n=====AcyclicAdjacencyMap properties====="

  test "edgeList empty                           == []" $
        edgeList (empty :: AAI)                  == []
  test "edgeList (vertex 5)                      == []" $
        edgeList (vertex 5 :: AAI)               == []
  test "edgeList (1 * 2)                         == [(1,2)]" $
        edgeList (1 * 2 :: AAI)                  == [(1,2)]
  test "edgeList (2 * 1)                         == []" $
        edgeList (2 * 1 :: AAI)                  == []

  test "vertexList empty                         == []" $
        vertexList (empty :: AAI)                == []
  test "vertexList (vertex 1)                    == [1]" $
        vertexList (vertex 1 :: AAI)             == [1]
  test "vertexList (vertices ([1, 3, 2]))        == List.sort [1, 3, 2] == [1,2,3]" $
        vertexList (vertices ([1, 3, 2]) :: AAI) == List.sort [1, 3, 2]


  putStrLn "\n============ AcyclicAdjacencyMap scc ============"

  test "scc AM.empty         == empty" $
        scc (AM.empty :: AI) == empty

  test "scc (AM.vertex x) == vertex (NonEmpty.vertex x)" $ \(x :: Int) ->
        scc (AM.vertex x) == vertex (NonEmpty.vertex x)

  test "scc (edge 1 1)          == vertex (NonEmpty.edge 1 1)" $
        scc (AM.edge 1 1 :: AI) == vertex (NonEmpty.edge 1 1)

  test "vertexList (scc (edge 1 2))          == [NonEmpty.vertex 1,NonEmpty.vertex 2]" $
        vertexList (scc (AM.edge 1 2 :: AI)) == [NonEmpty.vertex 1,NonEmpty.vertex 2]
  test "edgeList (scc (edge 1 2))            == [(NonEmpty.vertex 1,NonEmpty.vertex 2)]" $
        edgeList (scc (AM.edge 1 2 :: AI))   == [(NonEmpty.vertex 1,NonEmpty.vertex 2)]

  test "scc (AM.circuit (1:xs)) == vertex (NonEmpty.circuit1 (1 :| xs))" $ \(xs :: [Int]) ->
        scc (AM.circuit (1:xs)) == vertex (NonEmpty.circuit1 (1 :| xs))

  test "vertexList (scc (3 * 1 * 4 * 1 * 5))       == <correct result>" $
        vertexList (scc (3 * 1 * 4 * 1 * 5 :: AI)) == [NonEmpty.vertex 3,NonEmpty.vertex 5,NonEmpty.clique1 (1 :| [4,1])]
  test "edgeList (scc (3 * 1 * 4 * 1 * 5))         == <correct result>" $
        edgeList (scc (3 * 1 * 4 * 1 * 5 :: AI))   == [(NonEmpty.vertex 3,NonEmpty.vertex 5),(NonEmpty.vertex 3,NonEmpty.clique1 (1 :| [4,1])),(NonEmpty.clique1 (1 :| [4,1]),NonEmpty.vertex 5)]


