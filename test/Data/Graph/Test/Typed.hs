-----------------------------------------------------------------------------
-- |
-- Module     : Data.Graph.Test.Typed
-- Copyright  : (c) Andrey Mokhov 2016-2022
-- License    : MIT (see the file LICENSE)
-- Maintainer : anfelor@posteo.de, andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Data.Graph.Typed".
-----------------------------------------------------------------------------
module Data.Graph.Test.Typed (
    -- * Testsuite
    testTyped
    ) where

import Algebra.Graph.Test
import Algebra.Graph.AdjacencyMap ( forest, empty, vertex, edge, vertices
                                  , isSubgraphOf, vertexList )

import Data.Array (array)
import Data.Graph.Typed
import Data.Tree
import Data.List (nub, sort)

import qualified Data.Graph  as KL
import qualified Data.IntSet as IntSet

import qualified Algebra.Graph.AdjacencyMap    as AM
import qualified Algebra.Graph.AdjacencyIntMap as AIM

type AI = AM.AdjacencyMap Int

-- TODO: Improve the alignment in the testsuite to match the documentation.
(%) :: (GraphKL Int -> a) -> AM.AdjacencyMap Int -> a
f % x = f (fromAdjacencyMap x)

testTyped :: IO ()
testTyped = do
    putStrLn "\n============ Typed ============"

    putStrLn "\n============ Typed.fromAdjacencyMap ============"

    test "toGraphKL (fromAdjacencyMap (1 * 2 + 3 * 1))                                == array (0,2) [(0,[1]), (1,[]), (2,[0])]" $
          toGraphKL (fromAdjacencyMap (1 * 2 + 3 * 1 :: AI))                          == array (0,2) [(0,[1]), (1,[]), (2,[0])]

    test "toGraphKL (fromAdjacencyMap (1 * 2 + 2 * 1))                                == array (0,1) [(0,[1]), (1,[0])]" $
          toGraphKL (fromAdjacencyMap (1 * 2 + 2 * 1 :: AI))                          == array (0,1) [(0,[1]), (1,[0])]

    test "map (fromVertexKL h) (vertices $ toGraphKL h)                               == vertexList g"
      $ \(g :: AI) -> let h = fromAdjacencyMap g in
          map (fromVertexKL h) (KL.vertices $ toGraphKL h)                            == AM.vertexList g

    test "map (\\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (edges $ toGraphKL h) == edgeList g"
      $ \(g :: AI) -> let h = fromAdjacencyMap g in
          map (\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (KL.edges $ toGraphKL h) == AM.edgeList g

    putStrLn "\n============ Typed.fromAdjacencyIntMap ============"

    test "toGraphKL (fromAdjacencyIntMap (1 * 2 + 3 * 1))                             == array (0,2) [(0,[1]), (1,[]), (2,[0])]" $
          toGraphKL (fromAdjacencyIntMap (1 * 2 + 3 * 1))                             == array (0,2) [(0,[1]), (1,[]), (2,[0])]

    test "toGraphKL (fromAdjacencyIntMap (1 * 2 + 2 * 1))                             == array (0,1) [(0,[1]), (1,[0])]" $
          toGraphKL (fromAdjacencyIntMap (1 * 2 + 2 * 1))                             == array (0,1) [(0,[1]), (1,[0])]

    test "map (fromVertexKL h) (vertices $ toGraphKL h)                               == IntSet.toAscList (vertexIntSet g)"
      $ \g -> let h = fromAdjacencyIntMap g in
        map (fromVertexKL h) (KL.vertices $ toGraphKL h)                              == IntSet.toAscList (AIM.vertexIntSet g)

    test "map (\\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (edges $ toGraphKL h) == edgeList g"
      $ \g -> let h = fromAdjacencyIntMap g in
         map (\(x, y) -> (fromVertexKL h x, fromVertexKL h y)) (KL.edges $ toGraphKL h) == AIM.edgeList g

    putStrLn $ "\n============ Typed.dfsForest ============"
    test "forest (dfsForest % edge 1 1)           == vertex 1" $
          forest (dfsForest % edge 1 1)           == vertex 1

    test "forest (dfsForest % edge 1 2)           == edge 1 2" $
          forest (dfsForest % edge 1 2)           == edge 1 2

    test "forest (dfsForest % edge 2 1)           == vertices [1, 2]" $
          forest (dfsForest % edge 2 1)           == vertices [1, 2]

    test "isSubgraphOf (forest $ dfsForest % x) x == True" $ \x ->
          isSubgraphOf (forest $ dfsForest % x) x == True

    test "dfsForest % forest (dfsForest % x)      == dfsForest % x" $ \x ->
          dfsForest % forest (dfsForest % x)      == dfsForest % x

    test "dfsForest % vertices vs                 == map (\\v -> Node v []) (nub $ sort vs)" $ \vs ->
          dfsForest % vertices vs                 == map (\v -> Node v []) (nub $ sort vs)

    test "dfsForest % (3 * (1 + 4) * (1 + 5))     == <correct result>" $
          dfsForest % (3 * (1 + 4) * (1 + 5))     == [ Node { rootLabel = 1
                                                     , subForest = [ Node { rootLabel = 5
                                                                          , subForest = [] }]}
                                                     , Node { rootLabel = 3
                                                     , subForest = [ Node { rootLabel = 4
                                                                          , subForest = [] }]}]

    putStrLn $ "\n============ Typed.dfsForestFrom ============"
    test "forest $ (dfsForestFrom % edge 1 1) [1]         == vertex 1" $
         (forest $ (dfsForestFrom % edge 1 1) [1])        == vertex 1

    test "forest $ (dfsForestFrom % edge 1 2) [0]         == empty" $
         (forest $ (dfsForestFrom % edge 1 2) [0])        == empty

    test "forest $ (dfsForestFrom % edge 1 2) [1]         == edge 1 2" $
         (forest $ (dfsForestFrom % edge 1 2) [1])        == edge 1 2

    test "forest $ (dfsForestFrom % edge 1 2) [2]         == vertex 2" $
         (forest $ (dfsForestFrom % edge 1 2) [2])        == vertex 2

    test "forest $ (dfsForestFrom % edge 1 2) [2,1]       == vertices [1,2]" $
         (forest $ (dfsForestFrom % edge 1 2) [2,1])      == vertices [1,2]

    test "isSubgraphOf (forest $ dfsForestFrom % x $ vs) x == True" $ \x vs ->
          isSubgraphOf (forest $ dfsForestFrom % x $ vs) x == True

    test "dfsForestFrom % x $ vertexList x                == dfsForest % x" $ \x ->
         (dfsForestFrom % x $ vertexList x)               == dfsForest % x

    test "dfsForestFrom % vertices vs $ vs                == map (\\v -> Node v []) (nub vs)" $ \vs ->
         (dfsForestFrom % vertices vs $ vs)               == map (\v -> Node v []) (nub vs)

    test "dfsForestFrom % x $ []                          == []" $ \x ->
         (dfsForestFrom % x $ [])                         == []

    test "dfsForestFrom % (3 * (1 + 4) * (1 + 5)) $ [1,4] == <correct result>" $
         (dfsForestFrom % (3 * (1 + 4) * (1 + 5)) $ [1,4])== [ Node { rootLabel = 1
                                                                    , subForest = [ Node { rootLabel = 5
                                                                                         , subForest = [] }]}
                                                             , Node { rootLabel = 4
                                                                    , subForest = [] }]

    putStrLn $ "\n============ Typed.dfs ============"
    test "dfs % edge 1 1 $ [1]   == [1]" $
         (dfs % edge 1 1 $ [1])  == [1]

    test "dfs % edge 1 2 $ [0]   == []" $
         (dfs % edge 1 2 $ [0])  == []

    test "dfs % edge 1 2 $ [1]   == [1,2]" $
         (dfs % edge 1 2 $ [1])  == [1,2]

    test "dfs % edge 1 2 $ [2]   == [2]" $
         (dfs % edge 1 2 $ [2])  == [2]

    test "dfs % edge 1 2 $ [1,2] == [1,2]" $
         (dfs % edge 1 2 $ [1,2])== [1,2]

    test "dfs % edge 1 2 $ [2,1] == [2,1]" $
         (dfs % edge 1 2 $ [2,1])== [2,1]

    test "dfs % x        $ []    == []" $ \x ->
         (dfs % x        $ [])   == []

    putStrLn ""
    test "dfs % (3 * (1 + 4) * (1 + 5)) $ [1,4]     == [1,5,4]" $
         (dfs % (3 * (1 + 4) * (1 + 5)) $ [1,4])    == [1,5,4]

    test "and [ hasVertex v x | v <- dfs % x $ vs ] == True" $ \x vs ->
          and [ hasVertex v x | v <- dfs % x $ vs ] == True

    putStrLn "\n============ Typed.topSort ============"
    test "topSort % (1 * 2 + 3 * 1) == [3,1,2]" $
          topSort % (1 * 2 + 3 * 1) == ([3,1,2] :: [Int])

    test "topSort % (1 * 2 + 2 * 1) == [1,2]" $
          topSort % (1 * 2 + 2 * 1) == ([1,2] :: [Int])
