{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.IntAdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.IntAdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.IntAdjacencyMap (
    -- * Testsuite
    testIntAdjacencyMap
  ) where

import Data.Tree

import Algebra.Graph.IntAdjacencyMap
import Algebra.Graph.IntAdjacencyMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

import qualified Data.Graph  as KL
import qualified Data.IntSet as IntSet
import qualified Data.Set    as Set

testIntAdjacencyMap :: IO ()
testIntAdjacencyMap = do
    putStrLn "\n============ IntAdjacencyMap ============"
    test "Axioms of graphs" $ (axioms :: GraphTestsuite IntAdjacencyMap)

    test "Consistency of arbitraryAdjacencyMap" $ \m ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs)

    putStrLn "\n============ IntAdjacencyMap.Show ============"
    test "show (empty     :: IntAdjacencyMap) == \"empty\"" $
          show (empty     :: IntAdjacencyMap) == "empty"

    test "show (1         :: IntAdjacencyMap) == \"vertex 1\"" $
          show (1         :: IntAdjacencyMap) == "vertex 1"

    test "show (1 + 2     :: IntAdjacencyMap) == \"vertices [1,2]\"" $
          show (1 + 2     :: IntAdjacencyMap) == "vertices [1,2]"

    test "show (1 * 2     :: IntAdjacencyMap) == \"edge 1 2\"" $
          show (1 * 2     :: IntAdjacencyMap) == "edge 1 2"

    test "show (1 * 2 * 3 :: IntAdjacencyMap) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 :: IntAdjacencyMap) == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: IntAdjacencyMap) == \"graph [1,2,3] [(1,2)]\"" $
          show (1 * 2 + 3 :: IntAdjacencyMap) == "graph [1,2,3] [(1,2)]"

    testEmpty  empty
    testVertex empty

    putStrLn "\n============ IntAdjacencyMap.edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \x y ->
          edge x y               == connect (vertex x) (vertex y)

    test "hasEdge x y (edge x y) == True" $ \x y ->
          hasEdge x y (edge x y) == True

    test "edgeCount   (edge x y) == 1" $ \x y ->
          edgeCount   (edge x y) == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount (edge 1 1) == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount (edge 1 2) == 2

    putStrLn "\n============ IntAdjacencyMap.overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty     (overlay x y) == (isEmpty  x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z (overlay x y) == (hasVertex z x|| hasVertex z y)

    test "vertexCount (overlay x y) >= vertexCount x" $ \x y ->
          vertexCount (overlay x y) >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount (overlay x y) <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x" $ \x y ->
          edgeCount   (overlay x y) >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \x y ->
          edgeCount   (overlay x y) <= edgeCount x   + edgeCount y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount (overlay 1 2) == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount   (overlay 1 2) == 0

    putStrLn "\n============ IntAdjacencyMap.connect ============"
    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty     (connect x y) == (isEmpty  x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z (connect x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x" $ \x y ->
          vertexCount (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \x y ->
          edgeCount   (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \x y ->
          edgeCount   (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y" $ \x y ->
          edgeCount   (connect x y) >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \x y ->
          edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount (connect 1 2) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount   (connect 1 2) == 1

    putStrLn "\n============ IntAdjacencyMap.vertices ============"
    test "vertices []             == empty" $
          vertices []             == empty

    test "vertices [x]            == vertex x" $ \x ->
          vertices [x]            == vertex x

    test "hasVertex x  . vertices == elem x" $ \x xs ->
         (hasVertex x  . vertices) xs == elem x xs

    test "vertexCount  . vertices == length . nub" $ \xs ->
         (vertexCount  . vertices) xs == (length . nubOrd) xs

    test "vertexIntSet . vertices == IntSet.fromList" $ \xs ->
         (vertexIntSet . vertices) xs == IntSet.fromList xs

    putStrLn "\n============ IntAdjacencyMap.edges ============"
    test "edges []          == empty" $
          edges []          ==  empty

    test "edges [(x,y)]     == edge x y" $ \x y ->
          edges [(x,y)]     == edge x y

    test "edgeCount . edges == length . nub" $ \xs ->
         (edgeCount . edges) xs == (length . nubOrd) xs

    putStrLn "\n============ IntAdjacencyMap.overlays ============"
    test "overlays []        == empty" $
          overlays []        == empty

    test "overlays [x]       == x" $ \x ->
          overlays [x]       == x

    test "overlays [x,y]     == overlay x y" $ \x y ->
          overlays [x,y]     == overlay x y

    test "isEmpty . overlays == all isEmpty" $ mapSize (min 10) $ \xs ->
         (isEmpty . overlays) xs == all isEmpty xs

    putStrLn "\n============ IntAdjacencyMap.connects ============"
    test "connects []        == empty" $
          connects []        == empty

    test "connects [x]       == x" $ \x ->
          connects [x]       == x

    test "connects [x,y]     == connect x y" $ \x y ->
          connects [x,y]     == connect x y

    test "isEmpty . connects == all isEmpty" $ mapSize (min 10) $ \xs ->
         (isEmpty . connects) xs == all isEmpty xs

    putStrLn "\n============ IntAdjacencyMap.graph ============"
    test "graph []  []      == empty" $
          graph []  []      == empty

    test "graph [x] []      == vertex x" $ \x ->
          graph [x] []      == vertex x

    test "graph []  [(x,y)] == edge x y" $ \x y ->
          graph []  [(x,y)] == edge x y

    test "graph vs  es      == overlay (vertices vs) (edges es)" $ \(vs :: [Int]) es ->
          graph vs  es      == overlay (vertices vs) (edges es)

    putStrLn "\n============ IntAdjacencyMap.fromAdjacencyList ============"
    test "fromAdjacencyList []                                  == empty" $
          fromAdjacencyList []                                  == empty

    test "fromAdjacencyList [(x, [])]                           == vertex x" $ \x ->
          fromAdjacencyList [(x, [])]                           == vertex x

    test "fromAdjacencyList [(x, [y])]                          == edge x y" $ \x y ->
          fromAdjacencyList [(x, [y])]                          == edge x y

    test "fromAdjacencyList . adjacencyList                     == id" $ \x ->
         (fromAdjacencyList . adjacencyList) x                  == x

    test "overlay (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)

    putStrLn "\n============ IntAdjacencyMap.isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ \x ->
          isSubgraphOf empty         x             == True

    test "isSubgraphOf (vertex x)    empty         == False" $ \x ->
          isSubgraphOf (vertex x)    empty         == False

    test "isSubgraphOf x             (overlay x y) == True" $ \x y ->
          isSubgraphOf x             (overlay x y) == True

    test "isSubgraphOf (overlay x y) (connect x y) == True" $ \x y ->
          isSubgraphOf (overlay x y) (connect x y) == True

    test "isSubgraphOf (path xs)     (circuit xs)  == True" $ \xs ->
          isSubgraphOf (path xs)     (circuit xs)  == True

    putStrLn "\n============ IntAdjacencyMap.isEmpty ============"
    test "isEmpty empty                       == True" $
          isEmpty empty                       == True

    test "isEmpty (overlay empty empty)       == True" $
          isEmpty (overlay empty empty)       == True

    test "isEmpty (vertex x)                  == False" $ \x ->
          isEmpty (vertex x)                  == False

    test "isEmpty (removeVertex x $ vertex x) == True" $ \x ->
          isEmpty (removeVertex x $ vertex x) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \x y ->
          isEmpty (removeEdge x y $ edge x y) == False

    putStrLn "\n============ IntAdjacencyMap.hasVertex ============"
    test "hasVertex x empty            == False" $ \x ->
          hasVertex x empty            == False

    test "hasVertex x (vertex x)       == True" $ \x ->
          hasVertex x (vertex x)       == True

    test "hasVertex x . removeVertex x == const False" $ \x y ->
          hasVertex x (removeVertex x y)==const False y

    putStrLn "\n============ IntAdjacencyMap.hasEdge ============"
    test "hasEdge x y empty            == False" $ \x y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \x y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \x y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \x y z ->
          hasEdge x y (removeEdge x y z)==const False z

    putStrLn "\n============ IntAdjacencyMap.vertexCount ============"
    test "vertexCount empty      == 0" $
          vertexCount empty      == 0

    test "vertexCount (vertex x) == 1" $ \x ->
          vertexCount (vertex x) == 1

    test "vertexCount            == length . vertexList" $ \x ->
          vertexCount x          == (length . vertexList) x

    putStrLn "\n============ IntAdjacencyMap.edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount empty      == 0

    test "edgeCount (vertex x) == 0" $ \x ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1" $ \x y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \x ->
          edgeCount x          == (length . edgeList) x

    putStrLn "\n============ IntAdjacencyMap.vertexList ============"
    test "vertexList empty      == []" $
          vertexList empty      == []

    test "vertexList (vertex x) == [x]" $ \x ->
          vertexList (vertex x) == [x]

    test "vertexList . vertices == nub . sort" $ \xs ->
         (vertexList . vertices) xs == (nubOrd . sort) xs

    putStrLn "\n============ IntAdjacencyMap.edgeList ============"
    test "edgeList empty          == []" $
          edgeList empty          == []

    test "edgeList (vertex x)     == []" $ \x ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]" $ \x y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [3,1]) == [(2,1), (2,3)]" $
          edgeList (star 2 [3,1]) == [(2,1), (2,3)]

    test "edgeList . edges        == nub . sort" $ \xs ->
         (edgeList . edges) xs    == (nubOrd . sort) xs

    putStrLn "\n============ IntAdjacencyMap.adjacencyList ============"
    test "adjacencyList empty          == []" $
          adjacencyList empty          == []

    test "adjacencyList (vertex x)     == [(x, [])]" $ \x ->
          adjacencyList (vertex x)     == [(x, [])]

    test "adjacencyList (edge 1 2)     == [(1, [2]), (2, [])]" $
          adjacencyList (edge 1 2)     == [(1, [2]), (2, [])]

    test "adjacencyList (star 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]" $
          adjacencyList (star 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]

    putStrLn "\n============ IntAdjacencyMap.vertexIntSet ============"
    test "vertexIntSet empty      == IntSet.empty" $
          vertexIntSet empty      == IntSet.empty

    test "vertexIntSet . vertex   == IntSet.singleton" $ \x ->
         (vertexIntSet . vertex) x== IntSet.singleton x

    test "vertexIntSet . vertices == IntSet.fromList" $ \xs ->
         (vertexIntSet . vertices) xs == IntSet.fromList xs

    test "vertexIntSet . clique   == IntSet.fromList" $ \xs ->
         (vertexIntSet . clique) xs == IntSet.fromList xs

    putStrLn "\n============ IntAdjacencyMap.edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet empty      == Set.empty

    test "edgeSet (vertex x) == Set.empty" $ \x ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \x y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges    == Set.fromList" $ \xs ->
         (edgeSet . edges) xs== Set.fromList xs

    putStrLn "\n============ IntAdjacencyMap.postIntSet ============"
    test "postIntSet x empty      == IntSet.empty" $ \x ->
          postIntSet x empty      == IntSet.empty

    test "postIntSet x (vertex x) == IntSet.empty" $ \x ->
          postIntSet x (vertex x) == IntSet.empty

    test "postIntSet x (edge x y) == IntSet.fromList [y]" $ \x y ->
          postIntSet x (edge x y) == IntSet.fromList [y]

    test "postIntSet 2 (edge 1 2) == IntSet.empty" $
          postIntSet 2 (edge 1 2) == IntSet.empty

    putStrLn "\n============ IntAdjacencyMap.path ============"
    test "path []    == empty" $
          path []    == empty

    test "path [x]   == vertex x" $ \x ->
          path [x]   == vertex x

    test "path [x,y] == edge x y" $ \x y ->
          path [x,y] == edge x y

    putStrLn "\n============ IntAdjacencyMap.circuit ============"
    test "circuit []    == empty" $
          circuit []    == empty

    test "circuit [x]   == edge x x" $ \x ->
          circuit [x]   == edge x x

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \x y ->
          circuit [x,y] == edges [(x,y), (y,x)]

    putStrLn "\n============ IntAdjacencyMap.clique ============"
    test "clique []         == empty" $
          clique []         == empty

    test "clique [x]        == vertex x" $ \x ->
          clique [x]        == vertex x

    test "clique [x,y]      == edge x y" $ \x y ->
          clique [x,y]      == edge x y

    test "clique [x,y,z]    == edges [(x,y), (x,z), (y,z)]" $ \x y z ->
          clique [x,y,z]    == edges [(x,y), (x,z), (y,z)]

    test "clique (xs ++ ys) == connect (clique xs) (clique ys)" $ \xs ys ->
          clique (xs ++ ys) == connect (clique xs) (clique ys)

    putStrLn "\n============ IntAdjacencyMap.biclique ============"
    test "biclique []      []      == empty" $
          biclique []      []      == empty

    test "biclique [x]     []      == vertex x" $ \x ->
          biclique [x]     []      == vertex x

    test "biclique []      [y]     == vertex y" $ \(y) ->
          biclique []      [y]     == vertex y

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \(x1) x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]

    test "biclique xs      ys      == connect (vertices xs) (vertices ys)" $ \xs ys ->
          biclique xs      ys      == connect (vertices xs) (vertices ys)

    putStrLn "\n============ IntAdjacencyMap.star ============"
    test "star x []    == vertex x" $ \x ->
          star x []    == vertex x

    test "star x [y]   == edge x y" $ \x y ->
          star x [y]   == edge x y

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \x y z ->
          star x [y,z] == edges [(x,y), (x,z)]

    putStrLn "\n============ IntAdjacencyMap.tree ============"
    test "tree (Node x [])                                         == vertex x" $ \x ->
          tree (Node x [])                                         == vertex x

    test "tree (Node x [Node y [Node z []]])                       == path [x,y,z]" $ \x y z ->
          tree (Node x [Node y [Node z []]])                       == path [x,y,z]

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \x y z ->
          tree (Node x [Node y [], Node z []])                     == star x [y,z]

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]

    putStrLn "\n============ IntAdjacencyMap.forest ============"
    test "forest []                                                  == empty" $
          forest []                                                  == empty

    test "forest [x]                                                 == tree x" $ \x ->
          forest [x]                                                 == tree x

    test "forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]" $
          forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]

    test "forest                                                     == overlays . map tree" $ \x ->
         (forest x)                                                  ==(overlays . map tree) x

    putStrLn "\n============ IntAdjacencyMap.removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \x ->
          removeVertex x (vertex x)       == empty

    test "removeVertex x . removeVertex x == removeVertex x" $ \x (y) ->
         (removeVertex x . removeVertex x)y==removeVertex x y

    putStrLn "\n============ IntAdjacencyMap.removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices [x, y]" $ \x y ->
          removeEdge x y (edge x y)       == vertices [x, y]

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \x y z ->
         (removeEdge x y . removeEdge x y)z==removeEdge x y z

    test "removeEdge x y . removeVertex x == removeVertex x" $ \x y z ->
         (removeEdge x y . removeVertex x)z==removeVertex x z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2

    putStrLn "\n============ IntAdjacencyMap.replaceVertex ============"
    test "replaceVertex x x            == id" $ \x (y) ->
          replaceVertex x x y          == y

    test "replaceVertex x y (vertex x) == vertex y" $ \x (y) ->
          replaceVertex x y (vertex x) == vertex y

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y z ->
          replaceVertex x y z          == mergeVertices (== x) y z

    putStrLn "\n============ IntAdjacencyMap.mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \x (y) ->
          mergeVertices (const False) x y  == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y (z) ->
          mergeVertices (== x) y z         == replaceVertex x y z

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 (0 * 2)     == 1 * 1

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 (3 + 4 * 5) == 4 * 1

    testTranspose empty

    putStrLn "\n============ IntAdjacencyMap.gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f) ->
          gmap f empty      == empty

    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f) x ->
          gmap f (vertex x) == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f) x y ->
          gmap f (edge x y) == edge (f x) (f y)

    test "gmap id           == id" $ \x ->
          gmap id x         == x

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f) (apply -> g) x ->
         (gmap f . gmap g) x== gmap (f . g) x

    putStrLn "\n============ IntAdjacencyMap.induce ============"
    test "induce (const True)  x      == x" $ \x ->
          induce (const True)  x      == x

    test "induce (const False) x      == empty" $ \x ->
          induce (const False) x      == empty

    test "induce (/= x)               == removeVertex x" $ \x (y) ->
          induce (/= x) y             == removeVertex x y

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) (y) ->
         (induce p . induce q) y      == induce (\x -> p x && q x) y

    test "isSubgraphOf (induce p x) x == True" $ \(apply -> p) x ->
          isSubgraphOf (induce p x) x == True

    putStrLn "\n============ IntAdjacencyMap.dfsForest ============"
    test "forest (dfsForest $ edge 1 1)         == vertex 1" $
          forest (dfsForest $ edge 1 1)         == vertex 1

    test "forest (dfsForest $ edge 1 2)         == edge 1 2" $
          forest (dfsForest $ edge 1 2)         == edge 1 2

    test "forest (dfsForest $ edge 2 1)         == vertices [1, 2]" $
          forest (dfsForest $ edge 2 1)         == vertices [1, 2]

    test "isSubgraphOf (forest $ dfsForest x) x == True" $ \x ->
          isSubgraphOf (forest $ dfsForest x) x == True

    test "dfsForest . forest . dfsForest        == dfsForest" $ \x ->
         (dfsForest . forest . dfsForest) x     == dfsForest x

    test "dfsForest $ 3 * (1 + 4) * (1 + 5)     == <correct result>" $
          dfsForest  (3 * (1 + 4) * (1 + 5))    == [ Node { rootLabel = 1
                                                   , subForest = [ Node { rootLabel = 5
                                                                        , subForest = [] }]}
                                                   , Node { rootLabel = 3
                                                   , subForest = [ Node { rootLabel = 4
                                                                        , subForest = [] }]}]

    putStrLn "\n============ IntAdjacencyMap.topSort ============"
    test "topSort (1 * 2 + 3 * 1)             == Just [3,1,2]" $
          topSort (1 * 2 + 3 * 1)             == Just [3,1,2]

    test "topSort (1 * 2 + 2 * 1)             == Nothing" $
          topSort (1 * 2 + 2 * 1)             == Nothing

    test "fmap (flip isTopSort x) (topSort x) /= Just False" $ \x ->
          fmap (flip isTopSort x) (topSort x) /= Just False

    putStrLn "\n============ IntAdjacencyMap.isTopSort  ============"
    test "isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True" $
          isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True

    test "isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False" $
          isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False

    test "isTopSort []        (1 * 2 + 3 * 1) == False" $
          isTopSort []        (1 * 2 + 3 * 1) == False

    test "isTopSort []        empty           == True" $
          isTopSort []        empty    == True

    test "isTopSort [x]       (vertex x)      == True" $ \x ->
          isTopSort [x]       (vertex x)      == True

    test "isTopSort [x]       (edge x x)      == False" $ \x ->
          isTopSort [x]       (edge x x)      == False

    putStrLn "\n============ IntAdjacencyMap.GraphKL ============"
    test "map (getVertex h) (vertices $ getGraph h) == IntSet.toAscList (vertexIntSet g)"
      $ \g -> let h = graphKL g in
        map (getVertex h) (KL.vertices $ getGraph h) == IntSet.toAscList (vertexIntSet g)

    test "map (\\(x, y) -> (getVertex h x, getVertex h y)) (edges $ getGraph h) == edgeList g"
      $ \g -> let h = graphKL g in
        map (\(x, y) -> (getVertex h x, getVertex h y)) (KL.edges $ getGraph h) == edgeList g

    test "fromGraphKL . graphKL == id" $ \x ->
        (fromGraphKL . graphKL) x == x
