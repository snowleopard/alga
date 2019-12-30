{-# LANGUAGE CPP, OverloadedLists, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.NonEmpty.Graph
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.NonEmpty".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.NonEmpty.Graph (
    -- * Testsuite
    testNonEmptyGraph
    ) where

import Control.Monad
import Data.Either
import Data.Maybe
import Data.Semigroup
import Data.Tree
import Data.Tuple

import Algebra.Graph.NonEmpty hiding (Graph)
import Algebra.Graph.Test hiding (axioms, theorems)
import Algebra.Graph.ToGraph (reachable, toGraph)

import qualified Algebra.Graph          as G
import qualified Algebra.Graph.NonEmpty as NonEmpty
import qualified Data.Graph             as KL
import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Set               as Set

type G = NonEmpty.Graph Int

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

testNonEmptyGraph :: IO ()
testNonEmptyGraph = do
    putStrLn "\n============ NonEmpty.Graph.============"
    test "Axioms of non-empty graphs"   axioms
    test "Theorems of non-empty graphs" theorems

    putStrLn $ "\n============ Ord (NonEmpty.Graph a) ============"
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

    putStrLn $ "\n============ Functor (NonEmpty.Graph a) ============"
    test "fmap f (vertex x) == vertex (f x)" $ \(apply -> f) (x :: Int) ->
          fmap f (vertex x) == vertex (f x :: Int)

    test "fmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f) (x :: Int) y ->
          fmap f (edge x y) == edge (f x) (f y :: Int)

    test "fmap id           == id" $ \(x :: G) ->
          fmap id x         == x

    test "fmap f . fmap g   == fmap (f . g)" $ \(apply -> f) (apply -> g) (x :: G) ->
         (fmap f . fmap g) x == (fmap (f . (g :: Int -> Int)) x :: G)

    putStrLn $ "\n============ Monad (NonEmpty.Graph a) ============"
    test "(vertex x >>= f)     == f x" $ \(apply -> f) (x :: Int) ->
          (vertex x >>= f)     == (f x :: G)

    test "(edge x y >>= f)     == connect (f x) (f y)" $ \(apply -> f) (x :: Int) y ->
          (edge x y >>= f)     == connect (f x) (f y :: G)

    test "(vertices1 xs >>= f) == overlays1 (fmap f xs)" $ mapSize (min 10) $ \(xs' :: NonEmptyList Int) (apply -> f) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertices1 xs >>= f) == (overlays1 (fmap f xs) :: G)

    test "(x >>= vertex)       == x" $ \(x :: G) ->
          (x >>= vertex)       == x

    test "((x >>= f) >>= g)    == (x >>= (\\y -> (f y) >>= g))" $ mapSize (min 10) $ \(x :: G) (apply -> f) (apply -> g) ->
          ((x >>= f) >>= g)    == (x >>= (\(y :: Int) -> (f y) >>= (g :: Int -> G)))

    putStrLn $ "\n============ NonEmpty.Graph.toNonEmpty ============"
    test "toNonEmpty empty       == Nothing" $
          toNonEmpty (G.empty :: G.Graph Int) == Nothing

    test "toNonEmpty (toGraph x) == Just (x :: NonEmpty.Graph a)" $ \x ->
          toNonEmpty (toGraph x) == Just (x :: G)

    putStrLn $ "\n============ NonEmpty.Graph.vertex ============"
    test "hasVertex x (vertex x) == True" $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \(x :: Int) ->
          edgeCount   (vertex x) == 0

    test "size        (vertex x) == 1" $ \(x :: Int) ->
          size        (vertex x) == 1

    putStrLn $ "\n============ NonEmpty.Graph.edge ============"
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

    putStrLn $ "\n============ NonEmpty.Graph.overlay ============"
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

    test "size        (overlay x y) == size x        + size y" $ \(x :: G) y ->
          size        (overlay x y) == size x        + size y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount (overlay 1 2 :: G) == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount   (overlay 1 2 :: G) == 0

    putStrLn $ "\n============ NonEmpty.Graph.overlay1 ============"
    test "               overlay1 empty x == x" $ \(x :: G) ->
                         overlay1 G.empty x == x

    test "x /= empty ==> overlay1 x     y == overlay (fromJust $ toNonEmpty x) y" $ \(x :: G.Graph Int) (y :: G) ->
          x /= G.empty ==> overlay1 x   y == overlay (fromJust $ toNonEmpty x) y


    putStrLn $ "\n============ NonEmpty.Graph.connect ============"
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

    test "size        (connect x y) == size x        + size y" $ \(x :: G) y ->
          size        (connect x y) == size x        + size y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount (connect 1 2 :: G) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount   (connect 1 2 :: G) == 1

    putStrLn $ "\n============ NonEmpty.Graph.vertices1 ============"
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

    putStrLn $ "\n============ NonEmpty.Graph.edges1 ============"
    test "edges1 [(x,y)]     == edge x y" $ \(x :: Int) y ->
          edges1 [(x,y)]     == edge x y

    test "edgeCount . edges1 == length . nub" $ \(xs' :: NonEmptyList (Int, Int)) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (edgeCount . edges1) xs == (NonEmpty.length . NonEmpty.nub) xs

    putStrLn $ "\n============ NonEmpty.Graph.overlays1 ============"
    test "overlays1 [x]   == x" $ \(x :: G) ->
          overlays1 [x]   == x

    test "overlays1 [x,y] == overlay x y" $ \(x :: G) y ->
          overlays1 [x,y] == overlay x y

    putStrLn $ "\n============ NonEmpty.Graph.connects1 ============"
    test "connects1 [x]   == x" $ \(x :: G) ->
          connects1 [x]   == x

    test "connects1 [x,y] == connect x y" $ \(x :: G) y ->
          connects1 [x,y] == connect x y

    putStrLn $ "\n============ NonEmpty.Graph.foldg1 ============"
    test "foldg1 vertex    overlay connect        == id" $ \(x :: G) ->
          foldg1 vertex    overlay connect x      == id x

    test "foldg1 vertex    overlay (flip connect) == transpose" $ \(x :: G) ->
          foldg1 vertex    overlay (flip connect) x == transpose x

    test "foldg1 (const 1) (+)     (+)            == size" $ \(x :: G) ->
          foldg1 (const 1) (+)     (+) x          == size x

    test "foldg1 (== x)    (||)    (||)           == hasVertex x" $ \(x :: Int) y ->
          foldg1 (== x)    (||)    (||) y         == hasVertex x y

    putStrLn $ "\n============ NonEmpty.Graph.isSubgraphOf ============"
    test "isSubgraphOf x             (overlay x y) ==  True" $ \(x :: G) y ->
          isSubgraphOf x             (overlay x y) ==  True

    test "isSubgraphOf (overlay x y) (connect x y) ==  True" $ \(x :: G) y ->
          isSubgraphOf (overlay x y) (connect x y) ==  True

    test "isSubgraphOf (path1 xs)    (circuit1 xs) ==  True" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in isSubgraphOf (path1 xs)    (circuit1 xs) ==  True

    test "isSubgraphOf x y                         ==> x <= y" $ \(x :: G) z ->
        let y = x + z -- Make sure we hit the precondition
        in isSubgraphOf x y                        ==> x <= y

    putStrLn "\n============ NonEmpty.Graph.(===) ============"
    test "    x === x     == True" $ \(x :: G) ->
             (x === x)    == True

    test "x + y === x + y == True" $ \(x :: G) y ->
         (x + y === x + y) == True

    test "1 + 2 === 2 + 1 == False" $
         (1 + 2 === 2 + (1 :: G)) == False

    test "x + y === x * y == False" $ \(x :: G) y ->
         (x + y === x * y) == False

    putStrLn $ "\n============ NonEmpty.Graph.size ============"
    test "size (vertex x)    == 1" $ \(x :: Int) ->
          size (vertex x)    == 1

    test "size (overlay x y) == size x + size y" $ \(x :: G) y ->
          size (overlay x y) == size x + size y

    test "size (connect x y) == size x + size y" $ \(x :: G) y ->
          size (connect x y) == size x + size y

    test "size x             >= 1" $ \(x :: G) ->
          size x             >= 1

    test "size x             >= vertexCount x" $ \(x :: G) ->
          size x             >= vertexCount x

    putStrLn $ "\n============ NonEmpty.Graph.hasVertex ============"
    test "hasVertex x (vertex x) == True" $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    test "hasVertex 1 (vertex 2) == False" $
          hasVertex 1 (vertex 2 :: G) == False

    putStrLn $ "\n============ NonEmpty.Graph.hasEdge ============"
    test "hasEdge x y (vertex z)       == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y z ->
         (hasEdge x y . removeEdge x y) z == False

    test "hasEdge x y                  == elem (x,y) . edgeList" $ \(x :: Int) y z -> do
        (u, v) <- elements ((x, y) : edgeList z)
        return $ hasEdge u v z == elem (u, v) (edgeList z)

    putStrLn $ "\n============ NonEmpty.Graph.vertexCount ============"
    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "vertexCount x          >= 1" $ \(x :: G) ->
          vertexCount x          >= 1

    test "vertexCount            == length . vertexList1" $ \(x :: G) ->
          vertexCount x          == (NonEmpty.length . vertexList1) x

    putStrLn $ "\n============ NonEmpty.Graph.edgeCount ============"
    test "edgeCount (vertex x) == 0" $ \(x :: Int) ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \(x :: G) ->
          edgeCount x          == (length . edgeList) x

    putStrLn $ "\n============ NonEmpty.Graph.vertexList1 ============"
    test "vertexList1 (vertex x)  == [x]" $ \(x :: Int) ->
          vertexList1 (vertex x)  == [x]

    test "vertexList1 . vertices1 == nub . sort" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexList1 . vertices1) xs == (NonEmpty.nub . NonEmpty.sort) xs

    putStrLn $ "\n============ NonEmpty.Graph.edgeList ============"
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

    putStrLn $ "\n============ NonEmpty.Graph.vertexSet ============"
    test "vertexSet . vertex    == Set.singleton" $ \(x :: Int) ->
         (vertexSet . vertex) x == Set.singleton x

    test "vertexSet . vertices1 == Set.fromList . toList" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexSet . vertices1) xs == (Set.fromList . NonEmpty.toList) xs

    test "vertexSet . clique1   == Set.fromList . toList" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (vertexSet . clique1) xs == (Set.fromList . NonEmpty.toList) xs

    putStrLn $ "\n============ NonEmpty.Graph.edgeSet ============"
    test "edgeSet (vertex x) == Set.empty" $ \(x :: Int) ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \(x :: Int) y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges1   == Set.fromList . toList" $ \(xs' :: NonEmptyList (Int, Int)) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (edgeSet . edges1) xs == (Set.fromList . NonEmpty.toList) xs

    putStrLn $ "\n============ NonEmpty.Graph.path1 ============"
    test "path1 [x]       == vertex x" $ \(x :: Int) ->
          path1 [x]       == vertex x

    test "path1 [x,y]     == edge x y" $ \(x :: Int) y ->
          path1 [x,y]     == edge x y

    test "path1 . reverse == transpose . path1" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (path1 . NonEmpty.reverse) xs == (transpose . path1) xs

    putStrLn $ "\n============ NonEmpty.Graph.circuit1 ============"
    test "circuit1 [x]       == edge x x" $ \(x :: Int) ->
          circuit1 [x]       == edge x x

    test "circuit1 [x,y]     == edges1 [(x,y), (y,x)]" $ \(x :: Int) y ->
          circuit1 [x,y]     == edges1 [(x,y), (y,x)]

    test "circuit1 . reverse == transpose . circuit1" $ \(xs' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
        in (circuit1 . NonEmpty.reverse) xs == (transpose . circuit1) xs

    putStrLn $ "\n============ NonEmpty.Graph.clique1 ============"
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

    putStrLn $ "\n============ NonEmpty.Graph.biclique1 ============"
    test "biclique1 [x1,x2] [y1,y2] == edges1 [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \(x1 :: Int) x2 y1 y2 ->
          biclique1 [x1,x2] [y1,y2] == edges1 [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]

    test "biclique1 xs      ys      == connect (vertices1 xs) (vertices1 ys)" $ \(xs' :: NonEmptyList Int) ys' ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
            ys = NonEmpty.fromList (getNonEmpty ys')
        in biclique1 xs      ys      == connect (vertices1 xs) (vertices1 ys)

    putStrLn $ "\n============ NonEmpty.Graph.star ============"
    test "star x []    == vertex x" $ \(x :: Int) ->
          star x []    == vertex x

    test "star x [y]   == edge x y" $ \(x :: Int) y ->
          star x [y]   == edge x y

    test "star x [y,z] == edges1 [(x,y), (x,z)]" $ \(x :: Int) y z ->
          star x [y,z] == edges1 [(x,y), (x,z)]

    putStrLn $ "\n============ NonEmpty.Graph.stars1 ============"
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

    putStrLn $ "\n============ NonEmpty.Graph.tree ============"
    test "tree (Node x [])                                         == vertex x" $ \(x :: Int) ->
          tree (Node x [])                                         == vertex x

    test "tree (Node x [Node y [Node z []]])                       == path1 [x,y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [Node z []]])                       == path1 [x,y,z]

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \(x :: Int) y z ->
          tree (Node x [Node y [], Node z []])                     == star x [y,z]

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges1 [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges1 [(1,2), (1,3), (3,4), (3,5::Int)]

    putStrLn $ "\n============ NonEmpty.Graph.mesh1 ============"
    test "mesh1 [x]     [y]        == vertex (x, y)" $ \(x :: Int) (y :: Int) ->
          mesh1 [x]     [y]        == vertex (x, y)

    test "mesh1 xs      ys         == box (path1 xs) (path1 ys)" $ \(xs' :: NonEmptyList Int) (ys' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
            ys = NonEmpty.fromList (getNonEmpty ys')
        in mesh1 xs      ys         == box (path1 xs) (path1 ys)

    test "mesh1 [1,2,3] ['a', 'b'] == <correct result>" $
          mesh1 [1,2,3] ['a', 'b'] == edges1 [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a'))
                                             , ((1,'b'),(2,'b')), ((2,'a'),(2,'b'))
                                             , ((2,'a'),(3,'a')), ((2,'b'),(3,'b'))
                                             , ((3,'a'),(3 :: Int,'b')) ]

    test "size (mesh xs ys)        == max 1 (3 * length xs * length ys - length xs - length ys -1)" $ \(xs' :: NonEmptyList Int) (ys' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
            ys = NonEmpty.fromList (getNonEmpty ys')
         in size (mesh1 xs ys) == max 1 (3 * length xs * length ys - length xs - length ys -1)

    putStrLn $ "\n============ NonEmpty.Graph.torus1 ============"
    test "torus1 [x]   [y]        == edge (x,y) (x,y)" $ \(x :: Int) (y :: Int) ->
          torus1 [x]   [y]        == edge (x,y) (x,y)

    test "torus1 xs    ys         == box (circuit1 xs) (circuit1 ys)" $ \(xs' :: NonEmptyList Int) (ys' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
            ys = NonEmpty.fromList (getNonEmpty ys')
        in torus1 xs    ys         == box (circuit1 xs) (circuit1 ys)

    test "torus1 [1,2] ['a', 'b'] == <correct result>" $
          torus1 [1,2] ['a', 'b'] == edges1 [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a'))
                                            , ((1,'b'),(1,'a')), ((1,'b'),(2,'b'))
                                            , ((2,'a'),(1,'a')), ((2,'a'),(2,'b'))
                                            , ((2,'b'),(1,'b')), ((2,'b'),(2 :: Int,'a')) ]

    test "size (torus1 xs ys)     == max 1 (3 * length xs * length ys)" $ \(xs' :: NonEmptyList Int) (ys' :: NonEmptyList Int) ->
        let xs = NonEmpty.fromList (getNonEmpty xs')
            ys = NonEmpty.fromList (getNonEmpty ys')
        in size (torus1 xs ys) == max 1 (3 * length xs * length ys)

    putStrLn $ "\n============ NonEmpty.Graph.removeVertex1 ============"
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

    putStrLn $ "\n============ NonEmpty.Graph.removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices1 [x,y]" $ \(x :: Int) y ->
          removeEdge x y (edge x y)       == vertices1 [x,y]

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \(x :: Int) y z ->
         (removeEdge x y . removeEdge x y) z == removeEdge x y z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * (2 :: G)

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * (2 :: G)

    test "size (removeEdge x y z)         <= 3 * size z" $ \(x :: Int) y z ->
          size (removeEdge x y z)         <= 3 * size z

    putStrLn $ "\n============ NonEmpty.Graph.replaceVertex ============"
    test "replaceVertex x x            == id" $ \(x :: Int) y ->
          replaceVertex x x y          == y

    test "replaceVertex x y (vertex x) == vertex y" $ \(x :: Int) y ->
          replaceVertex x y (vertex x) == vertex y

    test "replaceVertex x y            == mergeVertices (== x) y" $ \(x :: Int) y z ->
          replaceVertex x y z          == mergeVertices (== x) y z

    putStrLn $ "\n============ NonEmpty.Graph.mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \(x :: Int) y ->
          mergeVertices (const False) x y  == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \(x :: Int) y z ->
          mergeVertices (== x) y z         == replaceVertex x y z

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 (0 * 2)     == (1 * 1 :: G)

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 (3 + 4 * 5) == (4 * 1 :: G)

    putStrLn $ "\n============ NonEmpty.Graph.splitVertex1 ============"
    test "splitVertex1 x [x]                 == id" $ \x (y :: G) ->
          splitVertex1 x [x] y               == y

    test "splitVertex1 x [y]                 == replaceVertex x y" $ \x y (z :: G) ->
          splitVertex1 x [y] z               == replaceVertex x y z

    test "splitVertex1 1 [0,1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)" $
          splitVertex1 1 [0,1] (1 * (2 + 3)) == (0 + 1) * (2 + 3 :: G)

    putStrLn $ "\n============ NonEmpty.Graph.transpose ============"
    test "transpose (vertex x)  == vertex x" $ \(x :: Int) ->
          transpose (vertex x)  == vertex x

    test "transpose (edge x y)  == edge y x" $ \(x :: Int) y ->
          transpose (edge x y)  == edge y x

    test "transpose . transpose == id" $ \(x :: G) ->
         (transpose . transpose) x == x

    test "transpose (box x y)   == box (transpose x) (transpose y)" $ mapSize (min 10) $ \(x :: G) (y :: G) ->
          transpose (box x y)   == box (transpose x) (transpose y)

    test "edgeList . transpose  == sort . map swap . edgeList" $ \(x :: G) ->
         (edgeList . transpose) x == (sort . map swap . edgeList) x

    putStrLn $ "\n============ NonEmpty.Graph.induce1 ============"
    test "induce1 (const True ) x == Just x" $ \(x :: G) ->
          induce1 (const True ) x == Just x

    test "induce1 (const False) x == Nothing" $ \(x :: G) ->
          induce1 (const False) x == Nothing

    test "induce1 (/= x)          == removeVertex1 x" $ \(x :: Int) y ->
          induce1 (/= x) y        == removeVertex1 x y

    test "induce1 p >=> induce1 q == induce1 (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) (y :: G) ->
         (induce1 p >=> induce1 q) y == induce1 (\x -> p x && q x) y

    putStrLn $ "\n============ NonEmpty.Graph.induceJust1 ============"
    test "induceJust1 (vertex Nothing)                               == Nothing" $
          induceJust1 (vertex (Nothing :: Maybe Int))                == Nothing

    test "induceJust1 (edge (Just x) Nothing)                        == Just (vertex x)" $ \(x :: G) ->
          induceJust1 (edge (Just x) Nothing)                        == Just (vertex x)

    test "induceJust1 . fmap Just                                    == Just" $ \(x :: G) ->
         (induceJust1 . fmap Just) x                                 == Just x

    test "induceJust1 . fmap (\\x -> if p x then Just x else Nothing) == induce1 p" $ \(x :: G) (apply -> p) ->
         (induceJust1 . fmap (\x -> if p x then Just x else Nothing)) x == induce1 p x

    putStrLn $ "\n============ NonEmpty.Graph.simplify ============"
    test "simplify             ==  id" $ \(x :: G) ->
          simplify x           ==  x

    test "size (simplify x)    <=  size x" $ \(x :: G) ->
          size (simplify x)    <=  size x

    test "simplify 1           === 1" $
          simplify 1           === (1 :: G)

    test "simplify (1 + 1)     === 1" $
          simplify (1 + 1)     === (1 :: G)

    test "simplify (1 + 2 + 1) === 1 + 2" $
          simplify (1 + 2 + 1) === (1 + 2 :: G)

    test "simplify (1 * 1 * 1) === 1 * 1" $
          simplify (1 * 1 * 1) === (1 * 1 :: G)

    putStrLn "\n============ NonEmpty.Graph.sparsify ============"
    test "sort . reachable x       == sort . rights . reachable (Right x) . sparsify" $ \x (y :: G) ->
         (sort . reachable x) y    == (sort . rights . reachable (Right x) . sparsify) y

    test "vertexCount (sparsify x) <= vertexCount x + size x + 1" $ \(x :: G) ->
          vertexCount (sparsify x) <= vertexCount x + size x + 1

    test "edgeCount   (sparsify x) <= 3 * size x" $ \(x :: G) ->
          edgeCount   (sparsify x) <= 3 * size x

    test "size        (sparsify x) <= 3 * size x" $ \(x :: G) ->
          size        (sparsify x) <= 3 * size x

    putStrLn "\n============ NonEmpty.Graph.sparsify ============"
    test "sort . reachable k                 == sort . filter (<= n) . flip reachable k . sparsifyKL n" $ \(Positive n) -> do
        let pairs = (,) <$> choose (1, n) <*> choose (1, n)
        k  <- choose (1, n)
        es <- listOf pairs
        let x = G.edges es `overlay1` vertices1 [1..n]
        return $ (sort . reachable k) x == (sort . filter (<= n) . flip KL.reachable k . sparsifyKL n) x

    test "length (vertices $ sparsifyKL n x) <= vertexCount x + size x + 1" $ \(Positive n) -> do
        let pairs = (,) <$> choose (1, n) <*> choose (1, n)
        es <- listOf pairs
        let x = G.edges es `overlay1` vertices1 [1..n]
        return $ length (KL.vertices $ sparsifyKL n x) <= vertexCount x + size x + 1

    test "length (edges    $ sparsifyKL n x) <= 3 * size x" $ \(Positive n) -> do
        let pairs = (,) <$> choose (1, n) <*> choose (1, n)
        es <- listOf pairs
        let x = G.edges es `overlay1` vertices1 [1..n]
        return $ length (KL.edges $ sparsifyKL n x) <= 3 * size x

    putStrLn "\n============ NonEmpty.Graph.box ============"
    test "box (path1 [0,1]) (path1 ['a','b']) == <correct result>" $ mapSize (min 10) $
          box (path1 [0,1]) (path1 ['a','b']) == edges1 [ ((0,'a'), (0,'b'))
                                                        , ((0,'a'), (1,'a'))
                                                        , ((0,'b'), (1,'b'))
                                                        , ((1,'a'), (1::Int,'b')) ]

    let unit = fmap $ \(a, ()) -> a
        comm = fmap $ \(a,  b) -> (b, a)
    test "box x y                             ~~ box y x" $ mapSize (min 10) $ \(x :: G) (y :: G) ->
          comm (box x y)                      == box y x

    test "box x (overlay y z)                 == overlay (box x y) (box x z)" $ mapSize (min 10) $ \(x :: G) (y :: G) z ->
          box x (overlay y z)                 == overlay (box x y) (box x z)

    test "box x (vertex ())                   ~~ x" $ mapSize (min 10) $ \(x :: G) ->
     unit(box x (vertex ()))                  == x

    let assoc = fmap $ \(a, (b, c)) -> ((a, b), c)
    test "box x (box y z)                     ~~ box (box x y) z" $ mapSize (min 5) $ \(x :: G) (y :: G) (z :: G) ->
      assoc (box x (box y z))                 == box (box x y) z

    test "transpose   (box x y)               == box (transpose x) (transpose y)" $ mapSize (min 10) $ \(x :: G) (y :: G) ->
          transpose   (box x y)               == box (transpose x) (transpose y)

    test "vertexCount (box x y)               == vertexCount x * vertexCount y" $ mapSize (min 10) $ \(x :: G) (y :: G) ->
          vertexCount (box x y)               == vertexCount x * vertexCount y

    test "edgeCount   (box x y)               <= vertexCount x * edgeCount y + edgeCount x * vertexCount y" $ mapSize (min 10) $ \(x :: G) (y :: G) ->
          edgeCount   (box x y)               <= vertexCount x * edgeCount y + edgeCount x * vertexCount y
