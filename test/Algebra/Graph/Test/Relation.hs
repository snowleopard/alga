{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Relation
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'Relation'.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Relation (
    -- * Testsuite
    testRelation
  ) where

import Data.Tree
import Data.Tuple

import Algebra.Graph.Relation
import Algebra.Graph.Relation.Internal
import Algebra.Graph.Relation.Preorder
import Algebra.Graph.Relation.Reflexive
import Algebra.Graph.Relation.Symmetric
import Algebra.Graph.Relation.Transitive
import Algebra.Graph.Test

import qualified Algebra.Graph.Class as C
import qualified Data.Set            as Set

type RI = Relation Int
type II = Int -> Int
type IB = Int -> Bool

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testRelation :: IO ()
testRelation = do
    putStrLn "\n============ Relation ============"
    test "Axioms of graphs" $ sizeLimit $ (axioms :: GraphTestsuite RI)

    test "Consistency of arbitraryRelation" $ \(m :: RI) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs :: RI)

    putStrLn "\n============ Relation.Show ============"
    test "show (empty     :: Relation Int) == \"empty\"" $
          show (empty     :: Relation Int) == "empty"

    test "show (1         :: Relation Int) == \"vertex 1\"" $
          show (1         :: Relation Int) == "vertex 1"

    test "show (1 + 2     :: Relation Int) == \"vertices [1,2]\"" $
          show (1 + 2     :: Relation Int) == "vertices [1,2]"

    test "show (1 * 2     :: Relation Int) == \"edge 1 2\"" $
          show (1 * 2     :: Relation Int) == "edge 1 2"

    test "show (1 * 2 * 3 :: Relation Int) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 :: Relation Int) == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: Relation Int) == \"graph [1,2,3] [(1,2)]\"" $
          show (1 * 2 + 3 :: Relation Int) == "graph [1,2,3] [(1,2)]"

    putStrLn "\n============ Relation.empty ============"
    test "isEmpty     empty == True" $
          isEmpty    (empty :: RI) == True

    test "hasVertex x empty == False" $ \(x :: Int) ->
          hasVertex x empty == False

    test "vertexCount empty == 0" $
          vertexCount(empty :: RI) == 0

    test "edgeCount   empty == 0" $
          edgeCount  (empty :: RI) == 0

    putStrLn "\n============ Relation.vertex ============"
    test "isEmpty     (vertex x) == False" $ \(x :: Int) ->
          isEmpty     (vertex x) == False

    test "hasVertex x (vertex x) == True" $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    test "hasVertex 1 (vertex 2) == False" $
          hasVertex 1 (vertex 2 :: RI) == False

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \(x :: Int) ->
          edgeCount   (vertex x) == 0

    putStrLn "\n============ Relation.edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \(x :: Int) y ->
         (edge x y :: RI)        == connect (vertex x) (vertex y)

    test "hasEdge x y (edge x y) == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y) == True

    test "edgeCount   (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount   (edge x y) == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount (edge 1 1 :: RI) == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount (edge 1 2 :: RI) == 2

    putStrLn "\n============ Relation.overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \(x :: RI) y ->
          isEmpty     (overlay x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \(x :: RI) y z ->
          hasVertex z (overlay x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (overlay x y) >= vertexCount x" $ \(x :: RI) y ->
          vertexCount (overlay x y) >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \(x :: RI) y ->
          vertexCount (overlay x y) <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x" $ \(x :: RI) y ->
          edgeCount   (overlay x y) >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \(x :: RI) y ->
          edgeCount   (overlay x y) <= edgeCount x   + edgeCount y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount (overlay 1 2 :: RI) == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount   (overlay 1 2 :: RI) == 0

    putStrLn "\n============ Relation.connect ============"
    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \(x :: RI) y ->
          isEmpty     (connect x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \(x :: RI) y z ->
          hasVertex z (connect x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x" $ \(x :: RI) y ->
          vertexCount (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \(x :: RI) y ->
          vertexCount (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \(x :: RI) y ->
          edgeCount   (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \(x :: RI) y ->
          edgeCount   (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y" $ \(x :: RI) y ->
          edgeCount   (connect x y) >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \(x :: RI) y ->
          edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount (connect 1 2 :: RI) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount   (connect 1 2 :: RI) == 1

    putStrLn "\n============ Relation.vertices ============"
    test "vertices []            == empty" $
          vertices []            == (empty :: RI)

    test "vertices [x]           == vertex x" $ \(x :: Int) ->
          vertices [x]           == (vertex x :: RI)

    test "hasVertex x . vertices == elem x" $ \x (xs :: [Int]) ->
         (hasVertex x . vertices) xs == elem x xs

    test "vertexCount . vertices == length . nub" $ \(xs :: [Int]) ->
         (vertexCount . vertices) xs == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet   . vertices) xs == Set.fromList xs

    putStrLn "\n============ Relation.edges ============"
    test "edges []          == empty" $
          edges []          == (empty :: RI)

    test "edges [(x,y)]     == edge x y" $ \(x :: Int) y ->
          edges [(x,y)]     == (edge x y :: RI)

    test "edgeCount . edges == length . nub" $ \(xs :: [(Int, Int)]) ->
         (edgeCount . edges) xs == (length . nubOrd) xs

    putStrLn "\n============ Relation.overlays ============"
    test "overlays []        == empty" $
          overlays []        == (empty :: RI)

    test "overlays [x]       == x" $ \(x :: RI) ->
          overlays [x]       == x

    test "overlays [x,y]     == overlay x y" $ \(x :: RI) y ->
          overlays [x,y]     == overlay x y

    test "isEmpty . overlays == all isEmpty" $ mapSize (min 10) $ \(xs :: [RI]) ->
         (isEmpty . overlays) xs == all isEmpty xs

    putStrLn "\n============ Relation.connects ============"
    test "connects []        == empty" $
          connects []        == (empty :: RI)

    test "connects [x]       == x" $ \(x :: RI) ->
          connects [x]       == x

    test "connects [x,y]     == connect x y" $ \(x :: RI) y ->
          connects [x,y]     == connect x y

    test "isEmpty . connects == all isEmpty" $ mapSize (min 10) $ \(xs :: [RI]) ->
         (isEmpty . connects) xs == all isEmpty xs

    putStrLn "\n============ Relation.graph ============"
    test "graph []  []      == empty" $
          graph []  []      == (empty :: RI)

    test "graph [x] []      == vertex x" $ \(x :: Int) ->
          graph [x] []      == (vertex x :: RI)

    test "graph []  [(x,y)] == edge x y" $ \(x :: Int) y ->
          graph []  [(x,y)] == (edge x y :: RI)

    test "graph vs  es      == overlay (vertices vs) (edges es)" $ \(vs :: [Int]) es ->
          graph vs  es      == (overlay (vertices vs) (edges es) :: RI)

    putStrLn "\n============ Relation.fromAdjacencyList ============"
    test "fromAdjacencyList []                                  == empty" $
          fromAdjacencyList []                                  == (empty :: RI)

    test "fromAdjacencyList [(x, [])]                           == vertex x" $ \(x :: Int) ->
          fromAdjacencyList [(x, [])]                           == vertex x

    test "fromAdjacencyList [(x, [y])]                          == edge x y" $ \(x :: Int) y ->
          fromAdjacencyList [(x, [y])]                          == edge x y

    test "overlay (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyList xs) (fromAdjacencyList ys) ==(fromAdjacencyList (xs ++ ys) :: RI)

    putStrLn "\n============ Relation.isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ \(x :: RI) ->
          isSubgraphOf empty         x             == True

    test "isSubgraphOf (vertex x)    empty         == False" $ \x ->
          isSubgraphOf (vertex x)   (empty :: RI)   == False

    test "isSubgraphOf x             (overlay x y) == True" $ \(x :: RI) y ->
          isSubgraphOf x             (overlay x y) == True

    test "isSubgraphOf (overlay x y) (connect x y) == True" $ \(x :: RI) y ->
          isSubgraphOf (overlay x y) (connect x y) == True

    test "isSubgraphOf (path xs)     (circuit xs)  == True" $ \xs ->
          isSubgraphOf (path xs :: RI)(circuit xs)  == True

    putStrLn "\n============ Relation.isEmpty ============"
    test "isEmpty empty                       == True" $
          isEmpty (empty :: RI)                == True

    test "isEmpty (overlay empty empty)       == True" $
          isEmpty (overlay empty empty :: RI)  == True

    test "isEmpty (vertex x)                  == False" $ \(x :: Int) ->
          isEmpty (vertex x)                  == False

    test "isEmpty (removeVertex x $ vertex x) == True" $ \(x :: Int) ->
          isEmpty (removeVertex x $ vertex x) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \(x :: Int) y ->
          isEmpty (removeEdge x y $ edge x y) == False

    putStrLn "\n============ Relation.hasVertex ============"
    test "hasVertex x empty            == False" $ \(x :: Int) ->
          hasVertex x empty            == False

    test "hasVertex x (vertex x)       == True" $ \(x :: Int) ->
          hasVertex x (vertex x)       == True

    test "hasVertex x . removeVertex x == const False" $ \(x :: Int) y ->
          hasVertex x (removeVertex x y)==const False y

    putStrLn "\n============ Relation.hasEdge ============"
    test "hasEdge x y empty            == False" $ \(x :: Int) y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y z ->
          hasEdge x y (removeEdge x y z)==const False z

    putStrLn "\n============ Relation.vertexCount ============"
    test "vertexCount empty      == 0" $
          vertexCount (empty :: RI) == 0

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "vertexCount            == length . vertexList" $ \(x :: RI) ->
          vertexCount x          == (length . vertexList) x

    putStrLn "\n============ Relation.edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount (empty :: RI) == 0

    test "edgeCount (vertex x) == 0" $ \(x :: Int) ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \(x :: RI) ->
          edgeCount x          == (length . edgeList) x

    putStrLn "\n============ Relation.vertexList ============"
    test "vertexList empty      == []" $
          vertexList (empty :: RI) == []

    test "vertexList (vertex x) == [x]" $ \(x :: Int) ->
          vertexList (vertex x) == [x]

    test "vertexList . vertices == nub . sort" $ \(xs :: [Int]) ->
         (vertexList . vertices) xs == (nubOrd . sort) xs

    putStrLn "\n============ Relation.edgeList ============"
    test "edgeList empty          == []" $
          edgeList (empty :: RI )  == []

    test "edgeList (vertex x)     == []" $ \(x :: Int) ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]" $ \(x :: Int) y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [3,1]) == [(2,1), (2,3)]" $
          edgeList (star 2 [3,1]) == [(2,1), (2,3 :: Int)]

    test "edgeList . edges        == nub . sort" $ \(xs :: [(Int, Int)]) ->
         (edgeList . edges) xs    == (nubOrd . sort) xs

    putStrLn "\n============ Relation.vertexSet ============"
    test "vertexSet empty      == Set.empty" $
          vertexSet(empty :: RI)== Set.empty

    test "vertexSet . vertex   == Set.singleton" $ \(x :: Int) ->
         (vertexSet . vertex) x== Set.singleton x

    test "vertexSet . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet . vertices) xs == Set.fromList xs

    test "vertexSet . clique   == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet . clique) xs == Set.fromList xs

    putStrLn "\n============ Relation.edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet (empty :: RI) == Set.empty

    test "edgeSet (vertex x) == Set.empty" $ \(x :: Int) ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \(x :: Int) y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges    == Set.fromList" $ \(xs :: [(Int, Int)]) ->
         (edgeSet . edges) xs== Set.fromList xs

    putStrLn "\n============ Relation.preSet ============"
    test "preSet x empty      == Set.empty" $ \(x :: Int) ->
          preSet x empty      == Set.empty

    test "preSet x (vertex x) == Set.empty" $ \(x :: Int) ->
          preSet x (vertex x) == Set.empty

    test "preSet 1 (edge 1 2) == Set.empty" $
          preSet 1 (edge 1 2) ==(Set.empty :: Set.Set Int)

    test "preSet y (edge x y) == Set.fromList [x]" $ \(x :: Int) y ->
          preSet y (edge x y) ==(Set.fromList [x] :: Set.Set Int)

    putStrLn "\n============ Relation.postSet ============"
    test "postSet x empty      == Set.empty" $ \(x :: Int) ->
          postSet x empty      == Set.empty

    test "postSet x (vertex x) == Set.empty" $ \(x :: Int) ->
          postSet x (vertex x) == Set.empty

    test "postSet x (edge x y) == Set.fromList [y]" $ \(x :: Int) y ->
          postSet x (edge x y) == Set.fromList [y]

    test "postSet 2 (edge 1 2) == Set.empty" $
          postSet 2 (edge 1 2) ==(Set.empty :: Set.Set Int)

    putStrLn "\n============ Relation.path ============"
    test "path []    == empty" $
          path []    == (empty :: RI)

    test "path [x]   == vertex x" $ \(x :: Int) ->
          path [x]   == (vertex x :: RI)

    test "path [x,y] == edge x y" $ \(x :: Int) y ->
          path [x,y] == (edge x y :: RI)

    putStrLn "\n============ Relation.circuit ============"
    test "circuit []    == empty" $
          circuit []    == (empty :: RI)

    test "circuit [x]   == edge x x" $ \(x :: Int) ->
          circuit [x]   == (edge x x :: RI)

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \(x :: Int) y ->
          circuit [x,y] == (edges [(x,y), (y,x)] :: RI)

    putStrLn "\n============ Relation.clique ============"
    test "clique []         == empty" $
          clique []         == (empty :: RI)

    test "clique [x]        == vertex x" $ \(x :: Int) ->
          clique [x]        == (vertex x :: RI)

    test "clique [x,y]      == edge x y" $ \(x :: Int) y ->
          clique [x,y]      == (edge x y :: RI)

    test "clique [x,y,z]    == edges [(x,y), (x,z), (y,z)]" $ \(x :: Int) y z ->
          clique [x,y,z]    == (edges [(x,y), (x,z), (y,z)] :: RI)

    test "clique (xs ++ ys) == connect (clique xs) (clique ys)" $ \(xs :: [Int]) ys ->
          clique (xs ++ ys) == connect (clique xs) (clique ys)

    putStrLn "\n============ Relation.biclique ============"
    test "biclique []      []      == empty" $
          biclique []      []      == (empty :: RI)

    test "biclique [x]     []      == vertex x" $ \(x :: Int) ->
          biclique [x]     []      == (vertex x :: RI)

    test "biclique []      [y]     == vertex y" $ \(y :: Int) ->
          biclique []      [y]     == (vertex y :: RI)

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \(x1 :: Int) x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == (edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)] :: RI)

    test "biclique xs      ys      == connect (vertices xs) (vertices ys)" $ \(xs :: [Int]) ys ->
          biclique xs      ys      == connect (vertices xs) (vertices ys)

    putStrLn "\n============ Relation.star ============"
    test "star x []    == vertex x" $ \(x :: Int) ->
          star x []    == (vertex x :: RI)

    test "star x [y]   == edge x y" $ \(x :: Int) y ->
          star x [y]   == (edge x y :: RI)

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \(x :: Int) y z ->
          star x [y,z] == (edges [(x,y), (x,z)] :: RI)

    putStrLn "\n============ Relation.tree ============"
    test "tree (Node x [])                                         == vertex x" $ \(x :: Int) ->
          tree (Node x [])                                         == vertex x

    test "tree (Node x [Node y [Node z []]])                       == path [x,y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [Node z []]])                       == path [x,y,z]

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [], Node z []])                     == star x [y,z]

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5::Int)]

    putStrLn "\n============ Relation.forest ============"
    test "forest []                                                  == empty" $
          forest []                                                  == (empty :: RI)

    test "forest [x]                                                 == tree x" $ \(x :: Tree Int) ->
          forest [x]                                                 == tree x

    test "forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]" $
          forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5::Int)]

    test "forest                                                     == overlays . map tree" $ \(x :: Forest Int) ->
         (forest x)                                                  ==(overlays . map tree) x

    putStrLn "\n============ Relation.removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \(x :: Int) ->
          removeVertex x (vertex x)       == (empty :: RI)

    test "removeVertex x . removeVertex x == removeVertex x" $ \x (y :: RI) ->
         (removeVertex x . removeVertex x)y==(removeVertex x y :: RI)

    putStrLn "\n============ Relation.removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices [x, y]" $ \(x :: Int) y ->
          removeEdge x y (edge x y)       == (vertices [x, y] :: RI)

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \(x :: Int) y z ->
         (removeEdge x y . removeEdge x y)z==(removeEdge x y z :: RI)

    test "removeEdge x y . removeVertex x == removeVertex x" $ \(x :: Int) y z ->
         (removeEdge x y . removeVertex x)z==(removeVertex x z :: RI)

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == (1 * 2 * (2 :: RI))

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == (1 * 1 + 2 * (2 :: RI))

    putStrLn "\n============ Relation.replaceVertex ============"
    test "replaceVertex x x            == id" $ \x (y :: RI) ->
          replaceVertex x x y          == y

    test "replaceVertex x y (vertex x) == vertex y" $ \x (y :: Int) ->
          replaceVertex x y (vertex x) == (vertex y :: RI)

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y z ->
          replaceVertex x y z          == (mergeVertices (== x) y z :: RI)

    putStrLn "\n============ Relation.mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \x (y :: RI) ->
          mergeVertices (const False) x y  == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y (z :: RI) ->
          mergeVertices (== x) y z         == (replaceVertex x y z :: RI)

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 (0 * 2)     == (1 * 1 :: RI)

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 (3 + 4 * 5) == (4 * 1 :: RI)

    putStrLn "\n============ Relation.transpose ============"
    test "transpose empty       == empty" $
          transpose empty       ==(empty :: RI)

    test "transpose (vertex x)  == vertex x" $ \(x :: Int) ->
          transpose (vertex x)  == vertex x

    test "transpose (edge x y)  == edge y x" $ \(x :: Int) y ->
          transpose (edge x y)  == edge y x

    test "transpose . transpose == id" $ \(x :: RI) ->
         (transpose . transpose) x == x

    test "transpose . path      == path    . reverse" $ \(xs :: [Int]) ->
         (transpose . path) xs  == (path . reverse) xs

    test "transpose . circuit   == circuit . reverse" $ \(xs :: [Int]) ->
         (transpose . circuit) xs == (circuit . reverse) xs

    test "transpose . clique    == clique  . reverse" $ \(xs :: [Int]) ->
         (transpose . clique) xs == (clique . reverse) xs

    test "edgeList . transpose  == sort . map swap . edgeList" $ \(x :: RI) ->
         (edgeList . transpose) x == (sort . map swap . edgeList) x

    putStrLn "\n============ Relation.gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f :: II) ->
          gmap f empty      == empty

    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f :: II) x ->
          gmap f (vertex x) == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f :: II) x y ->
          gmap f (edge x y) == edge (f x) (f y)

    test "gmap id           == id" $ \x ->
          gmap id x         == (x :: RI)

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f :: II) (apply -> g :: II) x ->
         (gmap f . gmap g) x== gmap (f . g) x

    putStrLn "\n============ Relation.induce ============"
    test "induce (const True)  x      == x" $ \(x :: RI) ->
          induce (const True)  x      == x

    test "induce (const False) x      == empty" $ \(x :: RI) ->
          induce (const False) x      == (empty :: RI)

    test "induce (/= x)               == removeVertex x" $ \x (y :: RI) ->
          induce (/= x) y             == (removeVertex x y :: RI)

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p :: IB) (apply -> q :: IB) (y :: RI) ->
         (induce p . induce q) y      == (induce (\x -> p x && q x) y :: RI)

    test "isSubgraphOf (induce p x) x == True" $ \(apply -> p :: IB) (x :: RI) ->
          isSubgraphOf (induce p x) x == True

    putStrLn "\n============ Relation.compose ============"
    test "compose empty            x                == empty" $ \(x :: RI) ->
          compose empty            x                == empty

    test "compose x                empty            == empty" $ \(x :: RI) ->
          compose x                empty            == empty

    test "compose x                (compose y z)    == compose (compose x y) z" $ sizeLimit $ \(x :: RI) y z ->
          compose x                (compose y z)    == compose (compose x y) z

    test "compose (edge y z)       (edge x y)       == edge x z" $ \(x :: Int) y z ->
          compose (edge y z)       (edge x y)       == edge x z

    test "compose (path    [1..5]) (path    [1..5]) == edges [(1,3),(2,4),(3,5)]" $
          compose (path    [1..5]) (path    [1..5]) == edges [(1,3),(2,4),(3,5::Int)]

    test "compose (circuit [1..5]) (circuit [1..5]) == circuit [1,3,5,2,4]" $
          compose (circuit [1..5]) (circuit [1..5]) == circuit [1,3,5,2,4::Int]

    putStrLn "\n============ Relation.reflexiveClosure ============"
    test "reflexiveClosure empty      == empty" $
          reflexiveClosure empty      ==(empty :: RI)

    test "reflexiveClosure (vertex x) == edge x x" $ \(x :: Int) ->
          reflexiveClosure (vertex x) == edge x x

    putStrLn "\n============ Relation.symmetricClosure ============"

    test "symmetricClosure empty      == empty" $
          symmetricClosure empty      ==(empty :: RI)

    test "symmetricClosure (vertex x) == vertex x" $ \(x :: Int) ->
          symmetricClosure (vertex x) == vertex x

    test "symmetricClosure (edge x y) == edges [(x, y), (y, x)]" $ \(x :: Int) y ->
          symmetricClosure (edge x y) == edges [(x, y), (y, x)]

    putStrLn "\n============ Relation.transitiveClosure ============"
    test "transitiveClosure empty           == empty" $
          transitiveClosure empty           ==(empty :: RI)

    test "transitiveClosure (vertex x)      == vertex x" $ \(x :: Int) ->
          transitiveClosure (vertex x)      == vertex x

    test "transitiveClosure (path $ nub xs) == clique (nub $ xs)" $ \(xs :: [Int]) ->
          transitiveClosure (path $ nubOrd xs) == clique (nubOrd $ xs)

    putStrLn "\n============ Relation.preorderClosure ============"
    test "preorderClosure empty           == empty" $
          preorderClosure empty           ==(empty :: RI)

    test "preorderClosure (vertex x)      == edge x x" $ \(x :: Int) ->
          preorderClosure (vertex x)      == edge x x

    test "preorderClosure (path $ nub xs) == reflexiveClosure (clique $ nub xs)" $ \(xs :: [Int]) ->
          preorderClosure (path $ nubOrd xs) == reflexiveClosure (clique $ nubOrd xs)

    putStrLn "\n============ ReflexiveRelation ============"
    test "Axioms of reflexive graphs" $ sizeLimit
        (reflexiveAxioms :: GraphTestsuite (ReflexiveRelation Int))

    putStrLn "\n============ SymmetricRelation ============"
    test "Axioms of undirected graphs" $ sizeLimit
        (undirectedAxioms :: GraphTestsuite (SymmetricRelation Int))

    putStrLn "\n============ SymmetricRelation.neighbours ============"
    test "neighbours x empty      == Set.empty" $ \(x :: Int) ->
          neighbours x C.empty      == Set.empty

    test "neighbours x (vertex x) == Set.empty" $ \(x :: Int) ->
          neighbours x (C.vertex x) == Set.empty

    test "neighbours x (edge x y) == Set.fromList [y]" $ \(x :: Int) y ->
          neighbours x (C.edge x y) == Set.fromList [y]

    test "neighbours y (edge x y) == Set.fromList [x]" $ \(x :: Int) y ->
          neighbours y (C.edge x y) == Set.fromList [x]

    putStrLn "\n============ TransitiveRelation ============"
    test "Axioms of transitive graphs" $ sizeLimit
        (transitiveAxioms :: GraphTestsuite (TransitiveRelation Int))

    test "path xs == (clique xs :: TransitiveRelation Int)" $ sizeLimit $ \xs ->
          C.path xs == (C.clique xs :: TransitiveRelation Int)

    putStrLn "\n============ PreorderRelation ============"
    test "Axioms of preorder graphs" $ sizeLimit
        (preorderAxioms :: GraphTestsuite (PreorderRelation Int))

    test "path xs == (clique xs :: PreorderRelation Int)" $ sizeLimit $ \xs ->
          C.path xs == (C.clique xs :: PreorderRelation Int)
