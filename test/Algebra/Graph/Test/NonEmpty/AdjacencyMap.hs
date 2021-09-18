{-# LANGUAGE OverloadedLists, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.NonEmpty.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.NonEmpty.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.NonEmpty.AdjacencyMap (
    -- * Testsuite
    testNonEmptyAdjacencyMap
    ) where

import Control.Monad
import Data.Tree
import Data.Tuple

import Algebra.Graph.NonEmpty.AdjacencyMap
import Algebra.Graph.Test hiding (axioms, theorems)
import Algebra.Graph.ToGraph (reachable)

import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.List.NonEmpty                  as NonEmpty
import qualified Data.Set                            as Set

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

type G = NonEmpty.AdjacencyMap Int

axioms :: G -> G -> G -> Property
axioms x y z = conjoin
    [       x + y == y + x                      // "Overlay commutativity"
    , x + (y + z) == (x + y) + z                // "Overlay associativity"
    , x * (y * z) == (x * y) * z                // "Connect associativity"
    , x * (y + z) == x * y + x * z              // "Left distributivity"
    , (x + y) * z == x * z + y * z              // "Right distributivity"
    ,   x * y * z == x * y + x * z + y * z      // "Decomposition" ]

theorems :: G -> G -> Property
theorems x y = conjoin
    [         x + x == x                        // "Overlay idempotence"
    , x + y + x * y == x * y                    // "Absorption"
    ,         x * x == x * x * x                // "Connect saturation"
    ,             x <= x + y                    // "Overlay order"
    ,         x + y <= x * y                    // "Overlay-connect order" ]

testNonEmptyAdjacencyMap :: IO ()
testNonEmptyAdjacencyMap = do
    putStrLn "\n============ NonEmpty.AdjacencyMap ============"
    test "Axioms of non-empty graphs"   axioms
    test "Theorems of non-empty graphs" theorems

    putStrLn $ "\n============ Ord (NonEmpty.AdjacencyMap a) ============"
    test "vertex 1 <  vertex 2" $
          vertex 1 <  vertex (2 :: Int)

    test "vertex 3 <  edge 1 2" $
          vertex 3 <  edge 1 (2 :: Int)

    test "vertex 1 <  edge 1 1" $
          vertex 1 <  edge 1 (1 :: Int)

    test "edge 1 1 <  edge 1 2" $
          edge 1 1 <  edge 1 (2 :: Int)

    test "edge 1 2 <  edge 1 1 + edge 2 2" $
          edge 1 2 <  edge 1 1 + edge 2 (2 :: Int)

    test "edge 1 2 <  edge 1 3" $
          edge 1 2 <  edge 1 (3 :: Int)

    test "x        <= x + y" $ \(x :: G) y ->
          x        <= x + y

    test "x + y    <= x * y" $ \(x :: G) y ->
          x + y    <= x * y

    putStrLn $ "\n============ Show (NonEmpty.AdjacencyMap a) ============"
    test "show (1         :: AdjacencyMap Int) == \"vertex 1\"" $
          show (1         :: AdjacencyMap Int) == "vertex 1"

    test "show (1 + 2     :: AdjacencyMap Int) == \"vertices1 [1,2]\"" $
          show (1 + 2     :: AdjacencyMap Int) == "vertices1 [1,2]"

    test "show (1 * 2     :: AdjacencyMap Int) == \"edge 1 2\"" $
          show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"

    test "show (1 * 2 * 3 :: AdjacencyMap Int) == \"edges1 [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 :: AdjacencyMap Int) == "edges1 [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: AdjacencyMap Int) == \"overlay (vertex 3) (edge 1 2)\"" $
          show (1 * 2 + 3 :: AdjacencyMap Int) == "overlay (vertex 3) (edge 1 2)"

    test "show (vertex (-1)                             :: AdjacencyMap Int) == \"vertex (-1)\"" $
          show (vertex (-1)                             :: AdjacencyMap Int) == "vertex (-1)"

    test "show (vertex (-1) + vertex (-2)               :: AdjacencyMap Int) == \"vertices1 [-2,-1]\"" $
          show (vertex (-1) + vertex (-2)               :: AdjacencyMap Int) == "vertices1 [-2,-1]"

    test "show (vertex (-1) * vertex (-2)               :: AdjacencyMap Int) == \"edge (-1) (-2)\"" $
          show (vertex (-1) * vertex (-2)               :: AdjacencyMap Int) == "edge (-1) (-2)"

    test "show (vertex (-1) * vertex (-2) * vertex (-3) :: AdjacencyMap Int) == \"edges1 [(-2,-3),(-1,-3),(-1,-2)]\"" $
          show (vertex (-1) * vertex (-2) * vertex (-3) :: AdjacencyMap Int) == "edges1 [(-2,-3),(-1,-3),(-1,-2)]"

    test "show (vertex (-1) * vertex (-2) + vertex (-3) :: AdjacencyMap Int) == \"overlay (vertex (-3)) (edge (-1) (-2))\"" $
          show (vertex (-1) * vertex (-2) + vertex (-3) :: AdjacencyMap Int) == "overlay (vertex (-3)) (edge (-1) (-2))"

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.toNonEmpty ============"
    test "toNonEmpty empty          == Nothing" $
          toNonEmpty (AM.empty :: AM.AdjacencyMap Int) == Nothing

    test "toNonEmpty . fromNonEmpty == Just" $ \(x :: G) ->
         (toNonEmpty . fromNonEmpty) x == Just x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.fromNonEmpty ============"
    test "isEmpty . fromNonEmpty    == const False" $ \(x :: G) ->
         (AM.isEmpty . fromNonEmpty) x == const False x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.vertex ============"
    test "hasVertex x (vertex y) == (x == y)" $ \(x :: Int) y ->
          hasVertex x (vertex y) == (x == y)

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \(x :: Int) ->
          edgeCount   (vertex x) == 0

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \(x :: Int) y ->
          edge x y               == connect (vertex x) (vertex y)

    test "hasEdge x y (edge x y) == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y) == True

    test "edgeCount   (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount   (edge x y) == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount (edge 1 1 :: G) == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount (edge 1 2 :: G) == 2

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.overlay ============"
    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \(x :: G) y z ->
          hasVertex z (overlay x y) == hasVertex z x || hasVertex z y

    test "vertexCount (overlay x y) >= vertexCount x" $ \(x :: G) y ->
          vertexCount (overlay x y) >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \(x :: G) y ->
          vertexCount (overlay x y) <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x" $ \(x :: G) y ->
          edgeCount   (overlay x y) >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \(x :: G) y ->
          edgeCount   (overlay x y) <= edgeCount x   + edgeCount y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount (overlay 1 2 :: G) == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount   (overlay 1 2 :: G) == 0

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.connect ============"
    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \(x :: G) y z ->
          hasVertex z (connect x y) == hasVertex z x || hasVertex z y

    test "vertexCount (connect x y) >= vertexCount x" $ \(x :: G) y ->
          vertexCount (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \(x :: G) y ->
          vertexCount (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \(x :: G) y ->
          edgeCount   (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \(x :: G) y ->
          edgeCount   (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y" $ \(x :: G) y ->
          edgeCount   (connect x y) >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \(x :: G) y ->
          edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount (connect 1 2 :: G) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount   (connect 1 2 :: G) == 1

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.vertices1 ============"
    test "vertices1 [x]           == vertex x" $ \(x :: Int) ->
          vertices1 [x]           == vertex x

    test "hasVertex x . vertices1 == elem x" $ \(x :: Int) (xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (hasVertex x . vertices1) xs == elem x (NonEmpty.toList xs)

    test "vertexCount . vertices1 == length . nub" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexCount . vertices1) xs == (NonEmpty.length . NonEmpty.nub) xs

    test "vertexSet   . vertices1 == Set.fromList . toList" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexSet   . vertices1) xs == (Set.fromList . NonEmpty.toList) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.edges1 ============"
    test "edges1 [(x,y)]     == edge x y" $ \(x :: Int) y ->
          edges1 [(x,y)]     == edge x y

    test "edges1             == overlays1 . fmap (uncurry edge)" $ \(xs' :: NonEmptyList (Int, Int)) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in edges1 xs         == (overlays1 . fmap (uncurry edge)) xs

    test "edgeCount . edges1 == length . nub" $ \(xs' :: NonEmptyList (Int, Int)) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (edgeCount . edges1) xs == (NonEmpty.length . NonEmpty.nub) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.overlays1 ============"
    test "overlays1 [x]   == x" $ \(x :: G) ->
          overlays1 [x]   == x

    test "overlays1 [x,y] == overlay x y" $ \(x :: G) y ->
          overlays1 [x,y] == overlay x y

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.connects1 ============"
    test "connects1 [x]   == x" $ \(x :: G) ->
          connects1 [x]   == x

    test "connects1 [x,y] == connect x y" $ \(x :: G) y ->
          connects1 [x,y] == connect x y

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.isSubgraphOf ============"
    test "isSubgraphOf x             (overlay x y) ==  True" $ \(x :: G) y ->
          isSubgraphOf x             (overlay x y) ==  True

    test "isSubgraphOf (overlay x y) (connect x y) ==  True" $ \(x :: G) y ->
          isSubgraphOf (overlay x y) (connect x y) ==  True

    test "isSubgraphOf (path1 xs)    (circuit1 xs) ==  True" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in isSubgraphOf (path1 xs)    (circuit1 xs) == True

    test "isSubgraphOf x y                         ==> x <= y" $ \(x :: G) z ->
        let y = x + z -- Make sure we hit the precondition
        in isSubgraphOf x y                        ==> x <= y

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.hasVertex ============"
    test "hasVertex x (vertex y) == (x == y)" $ \(x :: Int) y ->
          hasVertex x (vertex y) == (x == y)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.hasEdge ============"
    test "hasEdge x y (vertex z)       == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y z ->
         (hasEdge x y . removeEdge x y) z == False

    test "hasEdge x y                  == elem (x,y) . edgeList" $ \(x :: Int) y z -> do
        (u, v) <- elements ((x, y) : edgeList z)
        return $ hasEdge u v z == elem (u, v) (edgeList z)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.vertexCount ============"
    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "vertexCount x          >= 1" $ \(x :: G) ->
          vertexCount x          >= 1

    test "vertexCount            == length . vertexList1" $ \(x :: G) ->
          vertexCount x          == (NonEmpty.length . vertexList1) x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.edgeCount ============"
    test "edgeCount (vertex x) == 0" $ \(x :: Int) ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \(x :: G) ->
          edgeCount x          == (length . edgeList) x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.vertexList1 ============"
    test "vertexList1 (vertex x)  == [x]" $ \(x :: Int) ->
          vertexList1 (vertex x)  == [x]

    test "vertexList1 . vertices1 == nub . sort" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexList1 . vertices1) xs == (NonEmpty.nub . NonEmpty.sort) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.edgeList ============"
    test "edgeList (vertex x)     == []" $ \(x :: Int) ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]" $ \(x :: Int) y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [3,1]) == [(2,1), (2,3)]" $
          edgeList (star 2 [3,1]) == [(2,1), (2,3 :: Int)]

    test "edgeList . edges1       == nub . sort . toList" $ \(xs' :: NonEmptyList (Int, Int)) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (edgeList . edges1) xs   == (nubOrd . sort . NonEmpty.toList) xs

    test "edgeList . transpose    == sort . map swap . edgeList" $ \(x :: G) ->
         (edgeList . transpose) x == (sort . map swap . edgeList) x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.vertexSet ============"
    test "vertexSet . vertex    == Set.singleton" $ \(x :: Int) ->
         (vertexSet . vertex) x == Set.singleton x

    test "vertexSet . vertices1 == Set.fromList . toList" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexSet . vertices1) xs == (Set.fromList . NonEmpty.toList) xs

    test "vertexSet . clique1   == Set.fromList . toList" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexSet . clique1) xs == (Set.fromList . NonEmpty.toList) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.edgeSet ============"
    test "edgeSet (vertex x) == Set.empty" $ \(x :: Int) ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \(x :: Int) y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges1   == Set.fromList . toList" $ \(xs' :: NonEmptyList (Int, Int)) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (edgeSet . edges1) xs == (Set.fromList . NonEmpty.toList) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.preSet ============"
    test "preSet x (vertex x) == Set.empty" $ \(x :: G) ->
          preSet x (vertex x) == Set.empty

    test "preSet 1 (edge 1 2) == Set.empty" $
          preSet 1 (edge 1 2 :: G) == Set.empty

    test "preSet y (edge x y) == Set.fromList [x]" $ \(x :: G) y ->
          preSet y (edge x y) == Set.fromList [x]

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.postSet ============"
    test "postSet x (vertex x) == Set.empty" $ \(x :: G) ->
          postSet x (vertex x) == Set.empty

    test "postSet x (edge x y) == Set.fromList [y]" $ \(x :: G) y ->
          postSet x (edge x y) == Set.fromList [y]

    test "postSet 2 (edge 1 2) == Set.empty" $
          postSet 2 (edge 1 2 :: G) == Set.empty

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.path1 ============"
    test "path1 [x]       == vertex x" $ \(x :: Int) ->
          path1 [x]       == vertex x

    test "path1 [x,y]     == edge x y" $ \(x :: Int) y ->
          path1 [x,y]     == edge x y

    test "path1 . reverse == transpose . path1" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (path1 . NonEmpty.reverse) xs == (transpose . path1) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.circuit1 ============"
    test "circuit1 [x]       == edge x x" $ \(x :: Int) ->
          circuit1 [x]       == edge x x

    test "circuit1 [x,y]     == edges1 [(x,y), (y,x)]" $ \(x :: Int) y ->
          circuit1 [x,y]     == edges1 [(x,y), (y,x)]

    test "circuit1 . reverse == transpose . circuit1" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (circuit1 . NonEmpty.reverse) xs == (transpose . circuit1) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.clique1 ============"
    test "clique1 [x]        == vertex x" $ \(x :: Int) ->
          clique1 [x]        == vertex x

    test "clique1 [x,y]      == edge x y" $ \(x :: Int) y ->
          clique1 [x,y]      == edge x y

    test "clique1 [x,y,z]    == edges1 [(x,y), (x,z), (y,z)]" $ \(x :: Int) y z ->
          clique1 [x,y,z]    == edges1 [(x,y), (x,z), (y,z)]

    test "clique1 (xs <> ys) == connect (clique1 xs) (clique1 ys)" $ \(xs' :: NonEmptyList Int) ys' ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
            ys = NonEmpty.fromList (getNonEmpty ys')
        in clique1 (xs <> ys)   == connect (clique1 xs) (clique1 ys)

    test "clique1 . reverse  == transpose . clique1" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (clique1 . NonEmpty.reverse) xs == (transpose . clique1) xs

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.biclique1 ============"
    test "biclique1 [x1,x2] [y1,y2] == edges1 [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \(x1 :: Int) x2 y1 y2 ->
          biclique1 [x1,x2] [y1,y2] == edges1 [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]

    test "biclique1 xs      ys      == connect (vertices1 xs) (vertices1 ys)" $ \(xs' :: NonEmptyList Int) ys' ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
            ys = NonEmpty.fromList (getNonEmpty ys')
        in biclique1 xs      ys      == connect (vertices1 xs) (vertices1 ys)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.star ============"
    test "star x []    == vertex x" $ \(x :: Int) ->
          star x []    == vertex x

    test "star x [y]   == edge x y" $ \(x :: Int) y ->
          star x [y]   == edge x y

    test "star x [y,z] == edges1 [(x,y), (x,z)]" $ \(x :: Int) y z ->
          star x [y,z] == edges1 [(x,y), (x,z)]

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.stars1 ============"
    test "stars1 [(x, [] )]               == vertex x" $ \(x :: Int) ->
          stars1 [(x, [] )]               == vertex x

    test "stars1 [(x, [y])]               == edge x y" $ \(x :: Int) y ->
          stars1 [(x, [y])]               == edge x y

    test "stars1 [(x, ys )]               == star x ys" $ \(x :: Int) ys ->
          stars1 [(x, ys )]               == star x ys

    test "stars1                          == overlays1 . fmap (uncurry star)" $ \(xs' :: NonEmptyList (Int, [Int])) ->
      let xs = NonEmpty.fromList (getNonEmpty xs')
      in  stars1 xs                       == overlays1 (fmap (uncurry star) xs)

    test "overlay (stars1 xs) (stars1 ys) == stars1 (xs <> ys)" $ \(xs' :: NonEmptyList (Int, [Int])) (ys' :: NonEmptyList (Int, [Int])) ->
      let xs = NonEmpty.fromList (getNonEmpty xs')
          ys = NonEmpty.fromList (getNonEmpty ys')
      in  overlay (stars1 xs) (stars1 ys) == stars1 (xs <> ys)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.tree ============"
    test "tree (Node x [])                                         == vertex x" $ \(x :: Int) ->
          tree (Node x [])                                         == vertex x

    test "tree (Node x [Node y [Node z []]])                       == path1 [x,y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [Node z []]])                       == path1 [x,y,z]

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [], Node z []])                     == star x [y,z]

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges1 [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges1 [(1,2), (1,3), (3,4), (3,5::Int)]

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.removeVertex1 ============"
    test "removeVertex1 x (vertex x)          == Nothing" $ \(x :: Int) ->
          removeVertex1 x (vertex x)          == Nothing

    test "removeVertex1 1 (vertex 2)          == Just (vertex 2)" $
          removeVertex1 1 (vertex 2)          == Just (vertex 2 :: G)

    test "removeVertex1 x (edge x x)          == Nothing" $ \(x :: Int) ->
          removeVertex1 x (edge x x)          == Nothing

    test "removeVertex1 1 (edge 1 2)          == Just (vertex 2)" $
          removeVertex1 1 (edge 1 2)          == Just (vertex 2 :: G)

    test "removeVertex1 x >=> removeVertex1 x == removeVertex1 x" $ \(x :: Int) y ->
         (removeVertex1 x >=> removeVertex1 x) y == removeVertex1 x y

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices1 [x,y]" $ \(x :: Int) y ->
          removeEdge x y (edge x y)       == vertices1 [x,y]

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \(x :: Int) y z ->
         (removeEdge x y . removeEdge x y) z == removeEdge x y z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * (2 :: G)

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * (2 :: G)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.replaceVertex ============"
    test "replaceVertex x x            == id" $ \(x :: Int) y ->
          replaceVertex x x y          == y

    test "replaceVertex x y (vertex x) == vertex y" $ \(x :: Int) y ->
          replaceVertex x y (vertex x) == vertex y

    test "replaceVertex x y            == mergeVertices (== x) y" $ \(x :: Int) y z ->
          replaceVertex x y z          == mergeVertices (== x) y z

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \(x :: Int) y ->
          mergeVertices (const False) x y  == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \(x :: Int) y z ->
          mergeVertices (== x) y z         == replaceVertex x y z

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 (0 * 2)     == (1 * 1 :: G)

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 (3 + 4 * 5) == (4 * 1 :: G)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.transpose ============"
    test "transpose (vertex x)  == vertex x" $ \(x :: Int) ->
          transpose (vertex x)  == vertex x

    test "transpose (edge x y)  == edge y x" $ \(x :: Int) y ->
          transpose (edge x y)  == edge y x

    test "transpose . transpose == id" $ \(x :: G) ->
         (transpose . transpose) x == x

    test "edgeList . transpose  == sort . map swap . edgeList" $ \(x :: G) ->
         (edgeList . transpose) x == (sort . map swap . edgeList) x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.gmap ============"
    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f) (x :: Int) ->
          gmap f (vertex x) == vertex (f x :: Int)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f) (x :: Int) y ->
          gmap f (edge x y) == edge (f x) (f y :: Int)

    test "gmap id           == id" $ \(x :: G) ->
          gmap id x         == x

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f) (apply -> g) (x :: G) ->
         (gmap f . gmap g) x == (gmap (f . (g :: Int -> Int)) x :: G)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.induce1 ============"
    test "induce1 (const True ) x == Just x" $ \(x :: G) ->
          induce1 (const True ) x == Just x

    test "induce1 (const False) x == Nothing" $ \(x :: G) ->
          induce1 (const False) x == Nothing

    test "induce1 (/= x)          == removeVertex1 x" $ \(x :: Int) y ->
          induce1 (/= x) y        == removeVertex1 x y

    test "induce1 p >=> induce1 q == induce1 (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) (y :: G) ->
         (induce1 p >=> induce1 q) y == induce1 (\x -> p x && q x) y

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.induceJust1 ============"
    test "induceJust1 (vertex Nothing)                               == Nothing" $
          induceJust1 (vertex (Nothing :: Maybe Int))                == Nothing

    test "induceJust1 (edge (Just x) Nothing)                        == Just (vertex x)" $ \(x :: G) ->
          induceJust1 (edge (Just x) Nothing)                        == Just (vertex x)

    test "induceJust1 . gmap Just                                    == Just" $ \(x :: G) ->
         (induceJust1 . gmap Just) x                                 == Just x

    test "induceJust1 . gmap (\\x -> if p x then Just x else Nothing) == induce1 p" $ \(x :: G) (apply -> p) ->
         (induceJust1 . gmap (\x -> if p x then Just x else Nothing)) x == induce1 p x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.closure ============"
    test "closure (vertex x)      == edge x x" $ \(x :: Int) ->
          closure (vertex x)      == edge x x

    test "closure (edge x x)      == edge x x" $ \(x :: Int) ->
          closure (edge x x)      == edge x x

    test "closure (edge x y)      == edges1 [(x,x), (x,y), (y,y)]" $ \(x :: Int) y ->
          closure (edge x y)      == edges1 [(x,x), (x,y), (y,y)]

    test "closure (path1 $ nub xs) == reflexiveClosure (clique1 $ nub xs)" $ \(xs :: NonEmptyList Int) ->
        let ys = NonEmpty.fromList (nubOrd $ getNonEmpty xs)
        in closure (path1 $ ys) == reflexiveClosure (clique1 $ ys)

    test "closure                 == reflexiveClosure . transitiveClosure" $ sizeLimit $ \(x :: G) ->
          closure x               == (reflexiveClosure . transitiveClosure) x

    test "closure                 == transitiveClosure . reflexiveClosure" $ sizeLimit $ \(x :: G) ->
          closure x               == (transitiveClosure . reflexiveClosure) x

    test "closure . closure       == closure" $ sizeLimit $ \(x :: G) ->
         (closure . closure) x    == closure x

    test "postSet x (closure y)   == Set.fromList (reachable x y)" $ sizeLimit $ \x (y :: G) ->
          postSet x (closure y)   == Set.fromList (reachable x y)

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.reflexiveClosure ============"
    test "reflexiveClosure (vertex x)         == edge x x" $ \(x :: Int) ->
          reflexiveClosure (vertex x)         == edge x x

    test "reflexiveClosure (edge x x)         == edge x x" $ \(x :: Int) ->
          reflexiveClosure (edge x x)         == edge x x

    test "reflexiveClosure (edge x y)         == edges1 [(x,x), (x,y), (y,y)]" $ \(x :: Int) y ->
          reflexiveClosure (edge x y)         == edges1 [(x,x), (x,y), (y,y)]

    test "reflexiveClosure . reflexiveClosure == reflexiveClosure" $ \(x :: G) ->
         (reflexiveClosure . reflexiveClosure) x == reflexiveClosure x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.symmetricClosure ============"
    test "symmetricClosure (vertex x)         == vertex x" $ \(x :: Int) ->
          symmetricClosure (vertex x)         == vertex x

    test "symmetricClosure (edge x y)         == edges1 [(x,y), (y,x)]" $ \(x :: G) y ->
          symmetricClosure (edge x y)         == edges1 [(x,y), (y,x)]

    test "symmetricClosure x                  == overlay x (transpose x)" $ \(x :: G) ->
          symmetricClosure x                  == overlay x (transpose x)

    test "symmetricClosure . symmetricClosure == symmetricClosure" $ \(x :: G) ->
         (symmetricClosure . symmetricClosure) x == symmetricClosure x

    putStrLn $ "\n============ NonEmpty.AdjacencyMap.transitiveClosure ============"
    test "transitiveClosure (vertex x)          == vertex x" $ \(x :: Int) ->
          transitiveClosure (vertex x)          == vertex x

    test "transitiveClosure (edge x y)          == edge x y" $ \(x :: G) y ->
          transitiveClosure (edge x y)          == edge x y

    test "transitiveClosure (path1 $ nub xs)    == clique1 (nub $ xs)" $ \(xs :: NonEmptyList Int) ->
        let ys = NonEmpty.fromList (nubOrd $ getNonEmpty xs)
        in transitiveClosure (path1 ys) == clique1 ys

    test "transitiveClosure . transitiveClosure == transitiveClosure" $ sizeLimit $ \(x :: G) ->
         (transitiveClosure . transitiveClosure) x == transitiveClosure x
