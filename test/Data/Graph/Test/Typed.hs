-----------------------------------------------------------------------------
-- |
-- Module     : Data.Graph.Test.Typed
-- Copyright  : (c) Andrey Mokhov 2016-2018
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

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import Algebra.Graph.Test
import Data.Array (array)
import Data.Graph.Typed
import Data.Tree
import Data.List

import qualified Data.Graph  as KL
import qualified Data.IntSet as IntSet

type AI = AM.AdjacencyMap Int

(%) :: (GraphKL Int -> a) -> AM.AdjacencyMap Int -> a
a % g = a $ fromAdjacencyMap g

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
          AM.forest (dfsForest % AM.edge 1 1)     == AM.vertex 1

    test "forest (dfsForest % edge 1 2)           == edge 1 2" $
          AM.forest (dfsForest % AM.edge 1 2)     == AM.edge 1 2

    test "forest (dfsForest % edge 2 1)           == vertices [1, 2]" $
          AM.forest (dfsForest % AM.edge 2 1)     == AM.vertices [1, 2]

    test "isSubgraphOf (forest $ dfsForest % x) x == True" $ \x ->
          AM.isSubgraphOf (AM.forest $ dfsForest % x) x == True

    test "dfsForest % forest (dfsForest % x)      == dfsForest % x" $ \x ->
          dfsForest % AM.forest (dfsForest % x)   == dfsForest % x

    test "dfsForest % vertices vs                 == map (\\v -> Node v []) (nub $ sort vs)" $ \vs ->
          dfsForest % AM.vertices vs              == map (\v -> Node v []) (nub $ sort vs)

    test "dfsForest % (3 * (1 + 4) * (1 + 5))     == <correct result>" $
          dfsForest % (3 * (1 + 4) * (1 + 5))     == [ Node { rootLabel = 1
                                                   , subForest = [ Node { rootLabel = 5
                                                                        , subForest = [] }]}
                                                   , Node { rootLabel = 3
                                                   , subForest = [ Node { rootLabel = 4
                                                                        , subForest = [] }]}]

    putStrLn $ "\n============ Typed.dfsForestFrom ============"
    test "forest (dfsForestFrom [1]       % edge 1 1)     == vertex 1" $
          AM.forest (dfsForestFrom [1]    % AM.edge 1 1)  == AM.vertex 1

    test "forest (dfsForestFrom [1]       % edge 1 2)     == edge 1 2" $
          AM.forest (dfsForestFrom [1]    % AM.edge 1 2)  == AM.edge 1 2

    test "forest (dfsForestFrom [2]       % edge 1 2)     == vertex 2" $
          AM.forest (dfsForestFrom [2]    % AM.edge 1 2)  == AM.vertex 2

    test "forest (dfsForestFrom [3]       % edge 1 2)     == empty" $
          AM.forest (dfsForestFrom [3]    % AM.edge 1 2)  == AM.empty

    test "forest (dfsForestFrom [2, 1]    % edge 1 2)     == vertices [1, 2]" $
          AM.forest (dfsForestFrom [2, 1] % AM.edge 1 2)  == AM.vertices [1, 2]

    test "isSubgraphOf (forest $ dfsForestFrom vs % x) x  == True" $ \vs x ->
          AM.isSubgraphOf (AM.forest (dfsForestFrom vs % x)) x == True

    test "dfsForestFrom (vertexList x) % x                == dfsForest % x" $ \x ->
          dfsForestFrom (AM.vertexList x) % x             == dfsForest % x

    test "dfsForestFrom vs           % (AM.vertices vs)   == map (\\v -> Node v []) (nub vs)" $ \vs ->
          dfsForestFrom vs           %  AM.vertices vs    == map (\v -> Node v []) (nub vs)

    test "dfsForestFrom []           % x                  == []" $ \x ->
          dfsForestFrom []           % x                  == []

    test "dfsForestFrom [1, 4] % 3 * (1 + 4) * (1 + 5)    == <correct result>" $
          dfsForestFrom [1, 4] % (3 * (1 + 4) * (1 + 5))  == [ Node { rootLabel = 1
                                                                    , subForest = [ Node { rootLabel = 5
                                                                                         , subForest = [] }]}
                                                             , Node { rootLabel = 4
                                                                    , subForest = [] }]

    putStrLn $ "\n============ Typed.dfs ============"
    test "dfs [1]    % edge 1 1                  == [1]" $
          dfs [1]    % AM.edge 1 1               == [1]

    test "dfs [1]    % edge 1 2                  == [1,2]" $
          dfs [1]    % AM.edge 1 2               == [1,2]

    test "dfs [2]    % edge 1 2                  == [2]" $
          dfs [2]    % AM.edge 1 2               == [2]

    test "dfs [3]    % edge 1 2                  == []" $
          dfs [3]    % AM.edge 1 2               == []

    test "dfs [1, 2] % edge 1 2                  == [1, 2]" $
          dfs [1, 2] % AM.edge 1 2               == [1, 2]

    test "dfs [2, 1] % edge 1 2                  == [2, 1]" $
          dfs [2, 1] % AM.edge 1 2               == [2, 1]

    test "dfs []     % x                         == []" $ \x ->
          dfs []     % x                         == []

    test "dfs [1, 4] % 3 * (1 + 4) * (1 + 5)     == [1, 5, 4]" $
          dfs [1, 4] % (3 * (1 + 4) * (1 + 5))   == [1, 5, 4]

    test "isSubgraphOf (vertices $ dfs vs % x) x == True" $ \vs x ->
          AM.isSubgraphOf (AM.vertices $ dfs vs % x) x == True

    putStrLn "\n============ Typed.topSort ============"
    test "topSort % (1 * 2 + 3 * 1) == [3,1,2]" $
          topSort % (1 * 2 + 3 * 1) == ([3,1,2] :: [Int])

    test "topSort % (1 * 2 + 2 * 1) == [1,2]" $
          topSort % (1 * 2 + 2 * 1) == ([1,2] :: [Int])
