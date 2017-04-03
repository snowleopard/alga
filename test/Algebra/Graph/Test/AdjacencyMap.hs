{-# LANGUAGE ViewPatterns #-}
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

import Data.Tree

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

import qualified Data.Graph as KL
import qualified Data.Set   as Set

type AI = AdjacencyMap Int
type II = Int -> Int
type IB = Int -> Bool

testAdjacencyMap :: IO ()
testAdjacencyMap = do
    putStrLn "\n============ AdjacencyMap ============"
    test "Axioms of graphs" $ (axioms :: GraphTestsuite AI)

    test "Consistency of arbitraryAdjacencyMap" $ \(m :: AI) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs :: AI)

    putStrLn "\n============ AdjacencyMap.Show ============"
    test "show (empty     :: AdjacencyMap Int) == \"empty\"" $
          show (empty     :: AdjacencyMap Int) == "empty"

    test "show (1         :: AdjacencyMap Int) == \"vertex 1\"" $
          show (1         :: AdjacencyMap Int) == "vertex 1"

    test "show (1 + 2     :: AdjacencyMap Int) == \"vertices [1,2]\"" $
          show (1 + 2     :: AdjacencyMap Int) == "vertices [1,2]"

    test "show (1 * 2     :: AdjacencyMap Int) == \"edge 1 2\"" $
          show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"

    test "show (1 * 2 * 3 :: AdjacencyMap Int) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 :: AdjacencyMap Int) == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: AdjacencyMap Int) == \"graph [1,2,3] [(1,2)]\"" $
          show (1 * 2 + 3 :: AdjacencyMap Int) == "graph [1,2,3] [(1,2)]"

    testEmpty  (empty :: AI)
    testVertex (empty :: AI)

    putStrLn "\n============ AdjacencyMap.edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \(x :: Int) y ->
         (edge x y :: AI)        == connect (vertex x) (vertex y)

    test "hasEdge x y (edge x y) == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y) == True

    test "edgeCount   (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount   (edge x y) == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount (edge 1 1 :: AI) == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount (edge 1 2 :: AI) == 2

    putStrLn "\n============ AdjacencyMap.overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \(x :: AI) y ->
          isEmpty     (overlay x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \(x :: AI) y z ->
          hasVertex z (overlay x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (overlay x y) >= vertexCount x" $ \(x :: AI) y ->
          vertexCount (overlay x y) >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \(x :: AI) y ->
          vertexCount (overlay x y) <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x" $ \(x :: AI) y ->
          edgeCount   (overlay x y) >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \(x :: AI) y ->
          edgeCount   (overlay x y) <= edgeCount x   + edgeCount y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount (overlay 1 2 :: AI) == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount   (overlay 1 2 :: AI) == 0

    putStrLn "\n============ AdjacencyMap.connect ============"
    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \(x :: AI) y ->
          isEmpty     (connect x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \(x :: AI) y z ->
          hasVertex z (connect x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x" $ \(x :: AI) y ->
          vertexCount (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \(x :: AI) y ->
          vertexCount (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \(x :: AI) y ->
          edgeCount   (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \(x :: AI) y ->
          edgeCount   (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y" $ \(x :: AI) y ->
          edgeCount   (connect x y) >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \(x :: AI) y ->
          edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount (connect 1 2 :: AI) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount   (connect 1 2 :: AI) == 1

    putStrLn "\n============ AdjacencyMap.vertices ============"
    test "vertices []            == empty" $
          vertices []            == (empty :: AI)

    test "vertices [x]           == vertex x" $ \(x :: Int) ->
          vertices [x]           == (vertex x :: AI)

    test "hasVertex x . vertices == elem x" $ \x (xs :: [Int]) ->
         (hasVertex x . vertices) xs == elem x xs

    test "vertexCount . vertices == length . nub" $ \(xs :: [Int]) ->
         (vertexCount . vertices) xs == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet   . vertices) xs == Set.fromList xs

    putStrLn "\n============ AdjacencyMap.edges ============"
    test "edges []          == empty" $
          edges []          == (empty :: AI)

    test "edges [(x,y)]     == edge x y" $ \(x :: Int) y ->
          edges [(x,y)]     == (edge x y :: AI)

    test "edgeCount . edges == length . nub" $ \(xs :: [(Int, Int)]) ->
         (edgeCount . edges) xs == (length . nubOrd) xs

    putStrLn "\n============ AdjacencyMap.overlays ============"
    test "overlays []        == empty" $
          overlays []        == (empty :: AI)

    test "overlays [x]       == x" $ \(x :: AI) ->
          overlays [x]       == x

    test "overlays [x,y]     == overlay x y" $ \(x :: AI) y ->
          overlays [x,y]     == overlay x y

    test "isEmpty . overlays == all isEmpty" $ mapSize (min 10) $ \(xs :: [AI]) ->
         (isEmpty . overlays) xs == all isEmpty xs

    putStrLn "\n============ AdjacencyMap.connects ============"
    test "connects []        == empty" $
          connects []        == (empty :: AI)

    test "connects [x]       == x" $ \(x :: AI) ->
          connects [x]       == x

    test "connects [x,y]     == connect x y" $ \(x :: AI) y ->
          connects [x,y]     == connect x y

    test "isEmpty . connects == all isEmpty" $ mapSize (min 10) $ \(xs :: [AI]) ->
         (isEmpty . connects) xs == all isEmpty xs

    putStrLn "\n============ AdjacencyMap.graph ============"
    test "graph []  []      == empty" $
          graph []  []      == (empty :: AI)

    test "graph [x] []      == vertex x" $ \(x :: Int) ->
          graph [x] []      == (vertex x :: AI)

    test "graph []  [(x,y)] == edge x y" $ \(x :: Int) y ->
          graph []  [(x,y)] == (edge x y :: AI)

    test "graph vs  es      == overlay (vertices vs) (edges es)" $ \(vs :: [Int]) es ->
          graph vs  es      == (overlay (vertices vs) (edges es) :: AI)

    putStrLn "\n============ AdjacencyMap.fromAdjacencyList ============"
    test "fromAdjacencyList []                                  == empty" $
          fromAdjacencyList []                                  == (empty :: AI)

    test "fromAdjacencyList [(x, [])]                           == vertex x" $ \(x :: Int) ->
          fromAdjacencyList [(x, [])]                           == vertex x

    test "fromAdjacencyList [(x, [y])]                          == edge x y" $ \(x :: Int) y ->
          fromAdjacencyList [(x, [y])]                          == edge x y

    test "fromAdjacencyList . adjacencyList                     == id" $ \(x :: AI) ->
         (fromAdjacencyList . adjacencyList) x                  == x

    test "overlay (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyList xs) (fromAdjacencyList ys) ==(fromAdjacencyList (xs ++ ys) :: AI)

    putStrLn "\n============ AdjacencyMap.isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ \(x :: AI) ->
          isSubgraphOf empty         x             == True

    test "isSubgraphOf (vertex x)    empty         == False" $ \x ->
          isSubgraphOf (vertex x)   (empty :: AI)   == False

    test "isSubgraphOf x             (overlay x y) == True" $ \(x :: AI) y ->
          isSubgraphOf x             (overlay x y) == True

    test "isSubgraphOf (overlay x y) (connect x y) == True" $ \(x :: AI) y ->
          isSubgraphOf (overlay x y) (connect x y) == True

    test "isSubgraphOf (path xs)     (circuit xs)  == True" $ \xs ->
          isSubgraphOf (path xs :: AI)(circuit xs)  == True

    putStrLn "\n============ AdjacencyMap.isEmpty ============"
    test "isEmpty empty                       == True" $
          isEmpty (empty :: AI)                == True

    test "isEmpty (overlay empty empty)       == True" $
          isEmpty (overlay empty empty :: AI)  == True

    test "isEmpty (vertex x)                  == False" $ \(x :: Int) ->
          isEmpty (vertex x)                  == False

    test "isEmpty (removeVertex x $ vertex x) == True" $ \(x :: Int) ->
          isEmpty (removeVertex x $ vertex x) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \(x :: Int) y ->
          isEmpty (removeEdge x y $ edge x y) == False

    putStrLn "\n============ AdjacencyMap.hasVertex ============"
    test "hasVertex x empty            == False" $ \(x :: Int) ->
          hasVertex x empty            == False

    test "hasVertex x (vertex x)       == True" $ \(x :: Int) ->
          hasVertex x (vertex x)       == True

    test "hasVertex x . removeVertex x == const False" $ \(x :: Int) y ->
          hasVertex x (removeVertex x y)==const False y

    putStrLn "\n============ AdjacencyMap.hasEdge ============"
    test "hasEdge x y empty            == False" $ \(x :: Int) y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y z ->
          hasEdge x y (removeEdge x y z)==const False z

    putStrLn "\n============ AdjacencyMap.vertexCount ============"
    test "vertexCount empty      == 0" $
          vertexCount (empty :: AI) == 0

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "vertexCount            == length . vertexList" $ \(x :: AI) ->
          vertexCount x          == (length . vertexList) x

    putStrLn "\n============ AdjacencyMap.edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount (empty :: AI) == 0

    test "edgeCount (vertex x) == 0" $ \(x :: Int) ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \(x :: AI) ->
          edgeCount x          == (length . edgeList) x

    putStrLn "\n============ AdjacencyMap.vertexList ============"
    test "vertexList empty      == []" $
          vertexList (empty :: AI) == []

    test "vertexList (vertex x) == [x]" $ \(x :: Int) ->
          vertexList (vertex x) == [x]

    test "vertexList . vertices == nub . sort" $ \(xs :: [Int]) ->
         (vertexList . vertices) xs == (nubOrd . sort) xs

    putStrLn "\n============ AdjacencyMap.edgeList ============"
    test "edgeList empty          == []" $
          edgeList (empty :: AI )  == []

    test "edgeList (vertex x)     == []" $ \(x :: Int) ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]" $ \(x :: Int) y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [3,1]) == [(2,1), (2,3)]" $
          edgeList (star 2 [3,1]) == [(2,1), (2,3 :: Int)]

    test "edgeList . edges        == nub . sort" $ \(xs :: [(Int, Int)]) ->
         (edgeList . edges) xs    == (nubOrd . sort) xs

    putStrLn "\n============ AdjacencyMap.adjacencyList ============"
    test "adjacencyList empty          == []" $
          adjacencyList (empty :: AI)  == []

    test "adjacencyList (vertex x)     == [(x, [])]" $ \(x :: Int) ->
          adjacencyList (vertex x)     == [(x, [])]

    test "adjacencyList (edge 1 2)     == [(1, [2]), (2, [])]" $
          adjacencyList (edge 1 (2 :: Int)) == [(1, [2]), (2, [])]

    test "adjacencyList (star 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]" $
          adjacencyList (star 2 [3,1::Int]) == [(1, []), (2, [1,3]), (3, [])]

    putStrLn "\n============ AdjacencyMap.vertexSet ============"
    test "vertexSet empty      == Set.empty" $
          vertexSet(empty :: AI)== Set.empty

    test "vertexSet . vertex   == Set.singleton" $ \(x :: Int) ->
         (vertexSet . vertex) x== Set.singleton x

    test "vertexSet . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet . vertices) xs == Set.fromList xs

    test "vertexSet . clique   == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet . clique) xs == Set.fromList xs

    putStrLn "\n============ AdjacencyMap.edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet (empty :: AI) == Set.empty

    test "edgeSet (vertex x) == Set.empty" $ \(x :: Int) ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \(x :: Int) y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges    == Set.fromList" $ \(xs :: [(Int, Int)]) ->
         (edgeSet . edges) xs== Set.fromList xs

    putStrLn "\n============ AdjacencyMap.postSet ============"
    test "postSet x empty      == Set.empty" $ \(x :: Int) ->
          postSet x empty      == Set.empty

    test "postSet x (vertex x) == Set.empty" $ \(x :: Int) ->
          postSet x (vertex x) == Set.empty

    test "postSet x (edge x y) == Set.fromList [y]" $ \(x :: Int) y ->
          postSet x (edge x y) == Set.fromList [y]

    test "postSet 2 (edge 1 2) == Set.empty" $
          postSet 2 (edge 1 2) ==(Set.empty :: Set.Set Int)

    putStrLn "\n============ AdjacencyMap.path ============"
    test "path []    == empty" $
          path []    == (empty :: AI)

    test "path [x]   == vertex x" $ \(x :: Int) ->
          path [x]   == (vertex x :: AI)

    test "path [x,y] == edge x y" $ \(x :: Int) y ->
          path [x,y] == (edge x y :: AI)

    putStrLn "\n============ AdjacencyMap.circuit ============"
    test "circuit []    == empty" $
          circuit []    == (empty :: AI)

    test "circuit [x]   == edge x x" $ \(x :: Int) ->
          circuit [x]   == (edge x x :: AI)

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \(x :: Int) y ->
          circuit [x,y] == (edges [(x,y), (y,x)] :: AI)

    putStrLn "\n============ AdjacencyMap.clique ============"
    test "clique []         == empty" $
          clique []         == (empty :: AI)

    test "clique [x]        == vertex x" $ \(x :: Int) ->
          clique [x]        == (vertex x :: AI)

    test "clique [x,y]      == edge x y" $ \(x :: Int) y ->
          clique [x,y]      == (edge x y :: AI)

    test "clique [x,y,z]    == edges [(x,y), (x,z), (y,z)]" $ \(x :: Int) y z ->
          clique [x,y,z]    == (edges [(x,y), (x,z), (y,z)] :: AI)

    test "clique (xs ++ ys) == connect (clique xs) (clique ys)" $ \(xs :: [Int]) ys ->
          clique (xs ++ ys) == connect (clique xs) (clique ys)

    putStrLn "\n============ AdjacencyMap.biclique ============"
    test "biclique []      []      == empty" $
          biclique []      []      == (empty :: AI)

    test "biclique [x]     []      == vertex x" $ \(x :: Int) ->
          biclique [x]     []      == (vertex x :: AI)

    test "biclique []      [y]     == vertex y" $ \(y :: Int) ->
          biclique []      [y]     == (vertex y :: AI)

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \(x1 :: Int) x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == (edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)] :: AI)

    test "biclique xs      ys      == connect (vertices xs) (vertices ys)" $ \(xs :: [Int]) ys ->
          biclique xs      ys      == connect (vertices xs) (vertices ys)

    putStrLn "\n============ AdjacencyMap.star ============"
    test "star x []    == vertex x" $ \(x :: Int) ->
          star x []    == (vertex x :: AI)

    test "star x [y]   == edge x y" $ \(x :: Int) y ->
          star x [y]   == (edge x y :: AI)

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \(x :: Int) y z ->
          star x [y,z] == (edges [(x,y), (x,z)] :: AI)

    putStrLn "\n============ AdjacencyMap.tree ============"
    test "tree (Node x [])                                         == vertex x" $ \(x :: Int) ->
          tree (Node x [])                                         == vertex x

    test "tree (Node x [Node y [Node z []]])                       == path [x,y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [Node z []]])                       == path [x,y,z]

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [], Node z []])                     == star x [y,z]

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5::Int)]

    putStrLn "\n============ AdjacencyMap.forest ============"
    test "forest []                                                  == empty" $
          forest []                                                  == (empty :: AI)

    test "forest [x]                                                 == tree x" $ \(x :: Tree Int) ->
          forest [x]                                                 == tree x

    test "forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]" $
          forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5::Int)]

    test "forest                                                     == overlays . map tree" $ \(x :: Forest Int) ->
         (forest x)                                                  ==(overlays . map tree) x

    putStrLn "\n============ AdjacencyMap.removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \(x :: Int) ->
          removeVertex x (vertex x)       == (empty :: AI)

    test "removeVertex x . removeVertex x == removeVertex x" $ \x (y :: AI) ->
         (removeVertex x . removeVertex x)y==(removeVertex x y :: AI)

    putStrLn "\n============ AdjacencyMap.removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices [x, y]" $ \(x :: Int) y ->
          removeEdge x y (edge x y)       == (vertices [x, y] :: AI)

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \(x :: Int) y z ->
         (removeEdge x y . removeEdge x y)z==(removeEdge x y z :: AI)

    test "removeEdge x y . removeVertex x == removeVertex x" $ \(x :: Int) y z ->
         (removeEdge x y . removeVertex x)z==(removeVertex x z :: AI)

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == (1 * 2 * (2 :: AI))

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == (1 * 1 + 2 * (2 :: AI))

    putStrLn "\n============ AdjacencyMap.replaceVertex ============"
    test "replaceVertex x x            == id" $ \x (y :: AI) ->
          replaceVertex x x y          == y

    test "replaceVertex x y (vertex x) == vertex y" $ \x (y :: Int) ->
          replaceVertex x y (vertex x) == (vertex y :: AI)

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y z ->
          replaceVertex x y z          == (mergeVertices (== x) y z :: AI)

    putStrLn "\n============ AdjacencyMap.mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \x (y :: AI) ->
          mergeVertices (const False) x y  == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y (z :: AI) ->
          mergeVertices (== x) y z         == (replaceVertex x y z :: AI)

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 (0 * 2)     == (1 * 1 :: AI)

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 (3 + 4 * 5) == (4 * 1 :: AI)

    testTranspose (empty :: AI)

    putStrLn "\n============ AdjacencyMap.gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f :: II) ->
          gmap f empty      == empty

    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f :: II) x ->
          gmap f (vertex x) == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f :: II) x y ->
          gmap f (edge x y) == edge (f x) (f y)

    test "gmap id           == id" $ \x ->
          gmap id x         == (x :: AI)

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f :: II) (apply -> g :: II) x ->
         (gmap f . gmap g) x== gmap (f . g) x

    putStrLn "\n============ AdjacencyMap.induce ============"
    test "induce (const True)  x      == x" $ \(x :: AI) ->
          induce (const True)  x      == x

    test "induce (const False) x      == empty" $ \(x :: AI) ->
          induce (const False) x      == (empty :: AI)

    test "induce (/= x)               == removeVertex x" $ \x (y :: AI) ->
          induce (/= x) y             == (removeVertex x y :: AI)

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p :: IB) (apply -> q :: IB) (y :: AI) ->
         (induce p . induce q) y      == (induce (\x -> p x && q x) y :: AI)

    test "isSubgraphOf (induce p x) x == True" $ \(apply -> p :: IB) (x :: AI) ->
          isSubgraphOf (induce p x) x == True

    putStrLn "\n============ AdjacencyMap.dfsForest ============"
    test "forest (dfsForest $ edge 1 1)         == vertex 1" $
          forest (dfsForest $ edge 1 (1 :: Int))==(vertex 1 :: AI)

    test "forest (dfsForest $ edge 1 2)         == edge 1 2" $
          forest (dfsForest $ edge 1 (2 :: Int))==(edge 1 2 :: AI)

    test "forest (dfsForest $ edge 2 1)         == vertices [1, 2]" $
          forest (dfsForest $ edge 2 (1 :: Int))==(vertices [1, 2] :: AI)

    test "isSubgraphOf (forest $ dfsForest x) x == True" $ \(x :: AI) ->
          isSubgraphOf (forest $ dfsForest x) x == True

    test "dfsForest . forest . dfsForest        == dfsForest" $ \(x :: AI) ->
         (dfsForest . forest . dfsForest) x     == dfsForest x

    test "dfsForest $ 3 * (1 + 4) * (1 + 5)     == <correct result>" $
          dfsForest  (3 * (1 + 4) * (1 + 5))    == [ Node { rootLabel = 1 :: Int
                                                   , subForest = [ Node { rootLabel = 5
                                                                        , subForest = [] }]}
                                                   , Node { rootLabel = 3
                                                   , subForest = [ Node { rootLabel = 4
                                                                        , subForest = [] }]}]

    putStrLn "\n============ AdjacencyMap.topSort ============"
    test "topSort (1 * 2 + 3 * 1)             == Just [3,1,2]" $
          topSort (1 * 2 + 3 * 1)             == Just [3,1,2 :: Int]

    test "topSort (1 * 2 + 2 * 1)             == Nothing" $
          topSort (1 * 2 + 2 * 1 :: AI)       == Nothing

    test "fmap (flip isTopSort x) (topSort x) /= Just False" $ \(x :: AI) ->
          fmap (flip isTopSort x) (topSort x) /= Just False

    putStrLn "\n============ AdjacencyMap.isTopSort  ============"
    test "isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True" $
          isTopSort [3, 1, 2] (1 * 2 + 3 * 1 :: AI) == True

    test "isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False" $
          isTopSort [1, 2, 3] (1 * 2 + 3 * 1 :: AI) == False

    test "isTopSort []        (1 * 2 + 3 * 1) == False" $
          isTopSort []        (1 * 2 + 3 * 1 :: AI) == False

    test "isTopSort []        empty           == True" $
          isTopSort []       (empty :: AI)    == True

    test "isTopSort [x]       (vertex x)      == True" $ \(x :: Int) ->
          isTopSort [x]       (vertex x)      == True

    test "isTopSort [x]       (edge x x)      == False" $ \(x :: Int) ->
          isTopSort [x]       (edge x x)      == False

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

    putStrLn "\n============ AdjacencyMap.GraphKL ============"
    test "map (getVertex h) (vertices $ getGraph h) == Set.toAscList (vertexSet g)"
      $ \(g :: AI) -> let h = graphKL g in
        map (getVertex h) (KL.vertices $ getGraph h) == Set.toAscList (vertexSet g)

    test "map (\\(x, y) -> (getVertex h x, getVertex h y)) (edges $ getGraph h) == edgeList g"
      $ \(g :: AI) -> let h = graphKL g in
        map (\(x, y) -> (getVertex h x, getVertex h y)) (KL.edges $ getGraph h) == edgeList g

    test "fromGraphKL . graphKL == id" $ \(x :: AI) ->
        (fromGraphKL . graphKL) x == x
