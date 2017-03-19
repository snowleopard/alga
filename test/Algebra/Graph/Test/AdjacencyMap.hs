{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'AdjacencyMap'.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.AdjacencyMap (
    -- * Testsuite
    testAdjacencyMap
  ) where

import Data.Tree

import Algebra.Graph.Class hiding (edges)
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.Test

import qualified Data.Graph as KL
import qualified Data.Set   as Set

type AI = AdjacencyMap Int
type II = Int -> Int

testAdjacencyMap :: IO ()
testAdjacencyMap = do
    putStrLn "\n============ AdjacencyMap ============"
    test "Axioms of graphs" $ (axioms :: GraphTestsuite AI)

    test "Consistency of arbitraryAdjacencyMap" $ \(m :: AI) ->
        consistent m

    test "Consistency of fromAdjacencyList    " $ \xs ->
        consistent (fromAdjacencyList xs :: AI)

    putStrLn "\n============ Show ============"
    test "show (empty     :: AdjacencyMap Int) == \"empty\"                    " $
          show (empty     :: AdjacencyMap Int) == "empty"

    test "show (1         :: AdjacencyMap Int) == \"vertex 1\"                 " $
          show (1         :: AdjacencyMap Int) == "vertex 1"

    test "show (1 + 2     :: AdjacencyMap Int) == \"vertices [1,2]\"           " $
          show (1 + 2     :: AdjacencyMap Int) == "vertices [1,2]"

    test "show (1 * 2     :: AdjacencyMap Int) == \"edge 1 2\"                 " $
          show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"

    test "show (1 * 2 * 3 :: AdjacencyMap Int) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 :: AdjacencyMap Int) == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: AdjacencyMap Int) == \"graph [1,2,3] [(1,2)]\"    " $
          show (1 * 2 + 3 :: AdjacencyMap Int) == "graph [1,2,3] [(1,2)]"

    putStrLn "\n============ gmap ============"

    test "gmap f empty      == empty           " $ \(apply -> f :: II) ->
          gmap f empty      == empty

    test "gmap f (vertex x) == vertex (f x)    " $ \(apply -> f :: II) x ->
          gmap f (vertex x) == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f :: II) x y ->
          gmap f (edge x y) == edge (f x) (f y)

    test "gmap id           == id              " $ \x ->
          gmap id x         == (x :: AI)

    test "gmap f . gmap g   == gmap (f . g)    " $ \(apply -> f :: II) (apply -> g :: II) x ->
         (gmap f . gmap g) x== gmap (f . g) x

    putStrLn "\n============ edgeList ============"

    test "edgeList empty          == []            " $
          edgeList (empty :: AI)  == []

    test "edgeList (vertex x)     == []            " $ \(x :: Int) ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]       " $ \(x :: Int) y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [1,3]) == [(2,1), (2,3)]" $
          edgeList (star 2 [1,3]) == [(2,1), (2,3 :: Int)]

    test "edgeList . edges        == nub . sort    " $ \(xs :: [(Int, Int)]) ->
         (edgeList . edges) xs    ==(nubOrd . sort) xs

    putStrLn "\n============ edges ============"

    test "edges []       == empty   " $
          edges []       ==(empty :: AI)

    test "edges [(x, y)] == edge x y" $ \(x :: Int) y ->
          edges [(x, y)] == edge x y

    putStrLn "\n============ adjacencyList ============"

    test "adjacencyList empty          == []                            " $
          adjacencyList (empty :: AI)  == []

    test "adjacencyList (vertex x)     == [(x, [])]                     " $ \(x :: Int) ->
          adjacencyList (vertex x)     == [(x, [])]

    test "adjacencyList (edge 1 2)     == [(1, [2]), (2, [])]           " $
          adjacencyList (edge 1 (2 :: Int)) == [(1, [2]), (2, [])]

    test "adjacencyList (star 2 [1,3]) == [(1, []), (2, [1,3]), (3, [])]" $
          adjacencyList (star 2 [1,3::Int]) == [(1, []), (2, [1,3]), (3, [])]

    putStrLn "\n============ fromAdjacencyList ============"

    test "fromAdjacencyList []                                  == empty                       " $
          fromAdjacencyList []                                  == (empty :: AI)

    test "fromAdjacencyList [(x, [])]                           == vertex x                    " $ \(x :: Int) ->
          fromAdjacencyList [(x, [])]                           == vertex x

    test "fromAdjacencyList [(x, [y])]                          == edge x y                    " $ \(x :: Int) y ->
          fromAdjacencyList [(x, [y])]                          == edge x y

    test "fromAdjacencyList . adjacencyList                     == id                          " $ \(x :: AI) ->
         (fromAdjacencyList . adjacencyList) x                  == x

    test "overlay (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyList xs) (fromAdjacencyList ys) ==(fromAdjacencyList (xs ++ ys) :: AI)

    putStrLn "\n============ postset ============"

    test "postset x empty      == Set.empty       " $ \(x :: Int) ->
          postset x empty      == Set.empty

    test "postset x (vertex x) == Set.empty       " $ \(x :: Int) ->
          postset x (vertex x) == Set.empty

    test "postset x (edge x y) == Set.fromList [y]" $ \(x :: Int) y ->
          postset x (edge x y) == Set.fromList [y]

    test "postset 2 (edge 1 2) == Set.empty       " $
          postset 2 (edge 1 2) ==(Set.empty :: Set.Set Int)

    putStrLn "\n============ isEmpty ============"

    test "isEmpty empty      == True " $
          isEmpty (empty::AI)== True

    test "isEmpty (vertex x) == False" $ \(x :: Int) ->
          isEmpty (vertex x) == False

    putStrLn "\n============ hasVertex ============"

    test "hasVertex x empty      == False" $ \(x :: Int) ->
          hasVertex x empty      == False

    test "hasVertex x (vertex x) == True " $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    putStrLn "\n============ hasEdge ============"

    test "hasEdge x y empty      == False" $ \(x :: Int) y ->
          hasEdge x y empty      == False

    test "hasEdge x y (vertex z) == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z) == False

    test "hasEdge x y (edge x y) == True " $ \(x :: Int) y ->
          hasEdge x y (edge x y) == True

    putStrLn "\n============ toSet ============"

    test "toSet empty         == Set.empty      " $
          toSet (empty :: AI) == Set.empty

    test "toSet (vertex x)    == Set.singleton x" $ \(x :: Int) ->
          toSet (vertex x)    == Set.singleton x

    test "toSet (vertices xs) == Set.fromList xs" $ \(xs :: [Int]) ->
          toSet (vertices xs) == Set.fromList xs

    test "toSet (clique xs)   == Set.fromList xs" $ \(xs :: [Int]) ->
          toSet (clique xs)   == Set.fromList xs

    putStrLn "\n============ dfsForest ============"

    test "forest (dfsForest $ edge 1 1)         == vertex 1        " $
          forest (dfsForest $ edge 1 (1 :: Int))==(vertex 1 :: AI)

    test "forest (dfsForest $ edge 1 2)         == edge 1 2        " $
          forest (dfsForest $ edge 1 (2 :: Int))==(edge 1 2 :: AI)

    test "forest (dfsForest $ edge 2 1)         == vertices [1, 2] " $
          forest (dfsForest $ edge 2 (1 :: Int))==(vertices [1, 2] :: AI)

    test "isSubgraphOf (forest $ dfsForest x) x == True            " $ \(x :: AI) ->
          isSubgraphOf (forest $ dfsForest x) x == True

    test "dfsForest . forest . dfsForest        == dfsForest       " $ \(x :: AI) ->
         (dfsForest . forest . dfsForest) x     == dfsForest x

    test "dfsForest $ 3 * (1 + 4) * (1 + 5)     == <correct result>" $
          dfsForest  (3 * (1 + 4) * (1 + 5))    == [ Node { rootLabel = 1 :: Int
                                                   , subForest = [ Node { rootLabel = 5
                                                                        , subForest = [] }]}
                                                   , Node { rootLabel = 3
                                                   , subForest = [ Node { rootLabel = 4
                                                                        , subForest = [] }]}]

    putStrLn "\n============ topSort ============"

    test "topSort (1 * 2 + 3 * 1)             == Just [3,1,2]" $
          topSort (1 * 2 + 3 * 1)             == Just [3,1,2 :: Int]

    test "topSort (1 * 2 + 2 * 1)             == Nothing     " $
          topSort (1 * 2 + 2 * 1 :: AI)       == Nothing

    test "fmap (flip isTopSort x) (topSort x) /= Just False  " $ \(x :: AI) ->
          fmap (flip isTopSort x) (topSort x) /= Just False

    putStrLn "\n============ isTopSort  ============"

    test "isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True " $
          isTopSort [3, 1, 2] (1 * 2 + 3 * 1 :: AI) == True

    test "isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False" $
          isTopSort [1, 2, 3] (1 * 2 + 3 * 1 :: AI) == False

    test "isTopSort []        (1 * 2 + 3 * 1) == False" $
          isTopSort []        (1 * 2 + 3 * 1 :: AI) == False

    test "isTopSort []        empty           == True " $
          isTopSort []       (empty :: AI)    == True

    test "isTopSort [x]       (vertex x)      == True " $ \(x :: Int) ->
          isTopSort [x]       (vertex x)      == True

    test "isTopSort [x]       (edge x x)      == False" $ \(x :: Int) ->
          isTopSort [x]       (edge x x)      == False

    putStrLn "\n============ scc ============"

    test "scc empty               == empty                                           " $
          scc(empty :: AI)        == empty

    test "scc (vertex x)          == vertex (Set.singleton x)                        " $ \(x :: Int) ->
          scc (vertex x)          == vertex (Set.singleton x)

    test "scc (edge x y)          == edge (Set.singleton x) (Set.singleton y)        " $ \(x :: Int) y ->
          scc (edge x y)          == edge (Set.singleton x) (Set.singleton y)

    test "scc (circuit (1:xs))    == edge (Set.fromList (1:xs)) (Set.fromList (1:xs))" $ \(xs :: [Int]) ->
          scc (circuit (1:xs))    == edge (Set.fromList (1:xs)) (Set.fromList (1:xs))

    test "scc (3 * 1 * 4 * 1 * 5) == <correct result>                                " $
          scc (3 * 1 * 4 * 1 * 5) == edges [ (Set.fromList [1,4], Set.fromList [1,4])
                                           , (Set.fromList [1,4], Set.fromList [5]  )
                                           , (Set.fromList [3]  , Set.fromList [1,4])
                                           , (Set.fromList [3]  , Set.fromList [5 :: Int])]

    putStrLn "\n============ GraphKL ============"

    test "map (getVertex h) (vertices $ getGraph h) == Set.toAscList (toSet g)"
      $ \(g :: AI) -> let h = graphKL g in
        map (getVertex h) (KL.vertices $ getGraph h) == Set.toAscList (toSet g)

    test "map (\\(x, y) -> (getVertex h x, getVertex h y)) (edges $ getGraph h) == edgeList g"
      $ \(g :: AI) -> let h = graphKL g in
        map (\(x, y) -> (getVertex h x, getVertex h y)) (KL.edges $ getGraph h) == edgeList g

    test "fromGraphKL . graphKL == id" $ \(x :: AI) ->
        (fromGraphKL . graphKL) x == x
