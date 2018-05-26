-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.IntAdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : anfelor@posteo.de, andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.IntAdjacencyMap".
-----------------------------------------------------------------------------
module Data.Graph.Test.Typed (
    -- * Testsuite
    testTyped
  ) where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.IntAdjacencyMap as IM
import Algebra.Graph.Test
import Data.Array (array)
import Data.Graph.Typed

import qualified Data.Graph  as KL
import qualified Data.IntSet as IntSet

type AI = AM.AdjacencyMap Int

testTyped :: IO ()
testTyped = do
    putStrLn "\n============ Typed ============"

    putStrLn "\n============ Typed.fromAdjacencyMap ============"

    test "toGraphKL (fromAdjacencyMap (1 * 2 + 3 * 1)) == array (0,2) [(0,[1]),(1,[]),(2,[0])]" $
          toGraphKL (fromAdjacencyMap (1 * 2 + 3 * 1 :: AI)) == array (0,2) [(0,[1]),(1,[]),(2,[0])]

    test "toGraphKL (fromAdjacencyMap (1 * 2 + 2 * 1)) == array (0,1) [(0,[1]),(1,[0])]" $
          toGraphKL (fromAdjacencyMap (1 * 2 + 2 * 1 :: AI)) == array (0,1) [(0,[1]),(1,[0])]

    test "map (fromVertexKL h) (vertices $ toGraphKL h) == vertexList g"
      $ \(g :: AI) -> let h = fromAdjacencyMap g in
          map (fromVertexKL h) (KL.vertices $ toGraphKL h) == AM.vertexList g

    test "map (\\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (edges $ toGraphKL h) == edgeList g"
      $ \(g :: AI) -> let h = fromAdjacencyMap g in
          map (\(x, y) -> (fromVertexKL h x, fromVertexKL h y))
              (KL.edges $ toGraphKL h) == AM.edgeList g

    putStrLn "\n============ Typed.fromIntAdjacencyMap ============"

    test "toGraphKL (fromIntAdjacencyMap (1 * 2 + 3 * 1)) == array (0,2) [(0,[1]),(1,[]),(2,[0])]" $
          toGraphKL (fromIntAdjacencyMap (1 * 2 + 3 * 1)) == array (0,2) [(0,[1]),(1,[]),(2,[0])]

    test "toGraphKL (fromIntAdjacencyMap (1 * 2 + 2 * 1)) == array (0,1) [(0,[1]),(1,[0])]" $
          toGraphKL (fromIntAdjacencyMap (1 * 2 + 2 * 1)) == array (0,1) [(0,[1]),(1,[0])]

    test "map (fromVertexKL h) (vertices $ toGraphKL h) == IntSet.toAscList (vertexIntSet g)"
      $ \g -> let h = fromIntAdjacencyMap g in
        map (fromVertexKL h) (KL.vertices $ toGraphKL h) == IntSet.toAscList (IM.vertexIntSet g)

    test "map (\\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (edges $ toGraphKL h) == edgeList g"
      $ \g -> let h = fromIntAdjacencyMap g in
         map (\(x, y) -> (fromVertexKL h x, fromVertexKL h y))
             (KL.edges $ toGraphKL h) == IM.edgeList g

    putStrLn "\n============ Typed.topSort ============"

    test "topSort (fromAdjacencyMap (1 * 2 + 3 * 1)) == [3,1,2]" $
          topSort (fromAdjacencyMap (1 * 2 + 3 * 1)) == ([3,1,2] :: [Int])

    test "topSort (fromAdjacencyMap (1 * 2 + 2 * 1)) == [1,2]" $
          topSort (fromAdjacencyMap (1 * 2 + 2 * 1)) == ([1,2] :: [Int])
