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
import Algebra.Graph.Test.Generic

import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.List as List

-- t :: Testsuite
-- t = testsuite "AcyclicAdjacencyMap." empty

type AAI = AdjacencyMap Int

testAcyclicAdjacencyMap :: IO ()
testAcyclicAdjacencyMap = do
  let empty = empty :: AdjacencyMap Int
  putStrLn "\n=====AcyclicAdjacencyMap Num instance====="
  test "edgeList 0 == []" $
        edgeList 0 == []
  test "vertexList 0 == [0]" $
        vertexList 0 == [0]
  test "edgeList   (1 + 2)     == []" $
        edgeList   (1 + 2)     == []
  test "vertexList (1 + 2)     == [1,2]" $
        vertexList (1 + 2)     == [1,2]
  test "edgeList   (1 * 2)     == [(1,2)]" $
        edgeList   (1 * 2)     == [(1,2)]
  test "vertexList (1 * 2)     == [1,2]" $
        vertexList (1 * 2)     == [1,2]
  test "edgeList   (1 + 2 * 3) == [(2,3)]" $
        edgeList   (1 + 2 * 3) == [(2,3)]
  test "vertexList (1 + 2 * 3) == [1,2,3]" $
        vertexList (1 + 2 * 3) == [1,2,3]
  test "edgeList   (1 * 2 + 3) == [(1,2)]" $
        edgeList   (1 * 2 + 3) == [(1,2)]
  test "vertexList (1 * 2 + 3) == [1,2,3]" $
        vertexList (1 * 2 + 3) == [1,2,3]

  putStrLn "\n=====AcyclicAdjacencyMap consistency====="

  test "consistent empty           == True" $
        consistent empty           == True
  test "consistent (1 + 2)         == True" $
        consistent (1 + 2)         == True
  test "consistent (1 * 2 + 2 * 3) == True" $
        consistent (1 * 2 + 2 * 3) == True

  putStrLn "\n=====AcyclicAdjacencyMap construction primitives====="

  test "vertex 1 == 1" $
        vertex 1 == 1

  test "vertices []        == empty" $
        vertices []        == empty
  test "vertices [1]       == vertex 1" $
        vertices [1]       == vertex 1
  test "vertices [1, 2, 3] == 1 + 2 + 3" $
        vertices [1, 2, 3] == 1 + 2 + 3

  test "edgeList (overlayD empty empty)             == []" $
        edgeList (overlayD empty empty)             == []
  test "edgeList (overlayD (1 * 2 + 1 * 3) (1 * 2)) == [(Left 1,Left 2),(Left 1,Left 3),(Right 1,Right 2)]" $
        edgeList (overlayD (1 * 2 + 1 * 3) (1 * 2)) == [(Left 1,Left 2),(Left 1,Left 3),(Right 1,Right 2)]

  test "edgeList (connectD empty empty)    == []" $
        edgeList (connectD empty empty)    == []
  test "edgeList (connectD (1 + 2) (1 + 2)) == [(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2)]" $
        edgeList (connectD (1 + 2) (1 + 2)) == [(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2)]
  test "edgeList (connectD (1 * 2) (1 * 2)) == [(Left 1,Left 2),(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2),(Right 1,Right 2)]" $
        edgeList (connectD (1 * 2) (1 * 2)) == [(Left 1,Left 2),(Left 1,Right 1),(Left 1,Right 2),(Left 2,Right 1),(Left 2,Right 2),(Right 1,Right 2)]

  test "edgeList (box (1 * 2) (3 * 4)) == [((1,3),(1,4)),((1,3),(2,3)),((1,4),(2,4)),((2,3),(2,4))]" $
        edgeList (box (1 * 2) (3 * 4)) == [((1,3),(1,4)),((1,3),(2,3)),((1,4),(2,4)),((2,3),(2,4))]
  test "edgeList (box (1 + 2) (3 + 4)) == []" $
        edgeList (box (1 + 2) (3 + 4)) == []
  test "vertexList (box (1 + 2) (3 + 4)) == [(1,3),(1,4),(2,3),(2,4)]" $
        vertexList (box (1 + 2) (3 + 4)) == [(1,3),(1,4),(2,3),(2,4)]

  putStrLn "\n=====AcyclicAdjacencyMap transitiveClosure====="

  test "transitiveClosure empty             == empty" $
        transitiveClosure empty             == empty
  test "transitiveClosure (vertex 5)        == vertex 5" $
        transitiveClosure (vertex 5)        == vertex 5
  test "transitiveClosure (1 * 2 + 2 * 3)     == 1 * 2 + 2 * 3 + 1 * 3" $
        transitiveClosure (1 * 2 + 2 * 3)     == 1 * 2 + 2 * 3 + 1 * 3

  putStrLn "\n=====AcyclicAdjacencyMap topsort====="

  test "topSort (1)             == [1]" $
        topSort (1)             == [1]
  test "topSort (1 * 2 + 3 * 1) == [3,1,2]" $
        topSort (1 * 2 + 3 * 1) == [3,1,2]

  putStrLn "\n=====AcyclicAdjacencyMap fromGraph primitive====="

  test "fromGraph (<) (2 * 1)         == empty" $
        fromGraph (<) (2 * 1)         == empty
  test "fromGraph (<) (1 * 2)         == 1 * 2" $
        fromGraph (<) (1 * 2)         == 1 * 2
  test "fromGraph (<) (1 * 2 + 2 * 1) == 1 * 2" $
        fromGraph (<) (1 * 2 + 2 * 1) == 1 * 2

  putStrLn "\n=====AcyclicAdjacencyMap properties====="

  test "edgeList empty      == []" $
        edgeList empty      == []
  test "edgeList (vertex 5) == []" $
        edgeList (vertex 5) == []
  test "edgeList (1 * 2)      == [(1,2)]" $
        edgeList (1 * 2)      == [(1,2)]
  test "edgeList (2 * 1)      == []" $
        edgeList (2 * 1)      == []

  test "vertexList empty      == []" $
        vertexList empty      == []
  test "vertexList (vertex 1) == [1]" $
        vertexList (vertex 1) == [1]
  test "vertexList (vertices ([1, 3, 2])) == List.sort [1, 3, 2] == [1,2,3]" $
        vertexList (vertices ([1, 3, 2])) == List.sort [1, 3, 2]
