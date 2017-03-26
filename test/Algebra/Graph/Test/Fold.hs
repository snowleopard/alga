{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Fold
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'Fold' and polymorphic functions defined in
-- "Algebra.Graph.Class".
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Fold (
    -- * Testsuite
    testFold
  ) where

import Data.Foldable
import Data.Tree
import Data.Tuple

import Algebra.Graph.Fold
import Algebra.Graph.Test

import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet

type F  = Fold Int
type II = Int -> Int
type IB = Int -> Bool
type IF = Int -> F

testFold :: IO ()
testFold = do
    putStrLn "\n============ Fold ============"
    test "Axioms of graphs"   $ (axioms   :: GraphTestsuite F)

    putStrLn "\n============ Fold.Show ============"
    test "show (empty     :: Fold Int) == \"empty\"" $
          show (empty     :: Fold Int) == "empty"

    test "show (1         :: Fold Int) == \"vertex 1\"" $
          show (1         :: Fold Int) == "vertex 1"

    test "show (1 + 2     :: Fold Int) == \"vertices [1,2]\"" $
          show (1 + 2     :: Fold Int) == "vertices [1,2]"

    test "show (1 * 2     :: Fold Int) == \"edge 1 2\"" $
          show (1 * 2     :: Fold Int) == "edge 1 2"

    test "show (1 * 2 * 3 :: Fold Int) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 :: Fold Int) == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: Fold Int) == \"graph [1,2,3] [(1,2)]\"" $
          show (1 * 2 + 3 :: Fold Int) == "graph [1,2,3] [(1,2)]"

    putStrLn "\n============ Fold.empty ============"
    test "isEmpty     empty == True" $
          isEmpty    (empty :: F) == True

    test "hasVertex x empty == False" $ \(x :: Int) ->
          hasVertex x empty == False

    test "vertexCount empty == 0" $
          vertexCount(empty :: F) == 0

    test "edgeCount   empty == 0" $
          edgeCount  (empty :: F) == 0

    test "size        empty == 1" $
          size       (empty :: F) == 1

    putStrLn "\n============ Fold.vertex ============"
    test "isEmpty     (vertex x) == False" $ \(x :: Int) ->
          isEmpty     (vertex x) == False

    test "hasVertex x (vertex x) == True" $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    test "hasVertex 1 (vertex 2) == False" $
          hasVertex 1 (vertex 2 :: F) == False

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \(x :: Int) ->
          edgeCount   (vertex x) == 0

    test "size        (vertex x) == 1" $ \(x :: Int) ->
          size        (vertex x) == 1

    putStrLn "\n============ Fold.edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \(x :: Int) y ->
         (edge x y :: F)         == connect (vertex x) (vertex y)

    test "hasEdge x y (edge x y) == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y) == True

    test "edgeCount   (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount   (edge x y) == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount (edge 1 1 :: F) == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount (edge 1 2 :: F) == 2

    putStrLn "\n============ Fold.overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \(x :: F) y ->
          isEmpty     (overlay x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \(x :: F) y z ->
          hasVertex z (overlay x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (overlay x y) >= vertexCount x" $ \(x :: F) y ->
          vertexCount (overlay x y) >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \(x :: F) y ->
          vertexCount (overlay x y) <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x" $ \(x :: F) y ->
          edgeCount   (overlay x y) >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \(x :: F) y ->
          edgeCount   (overlay x y) <= edgeCount x   + edgeCount y

    test "size        (overlay x y) == size x        + size y" $ \(x :: F) y ->
          size        (overlay x y) == size x        + size y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount (overlay 1 2 :: F) == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount   (overlay 1 2 :: F) == 0

    putStrLn "\n============ Fold.connect ============"
    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \(x :: F) y ->
          isEmpty     (connect x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \(x :: F) y z ->
          hasVertex z (connect x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x" $ \(x :: F) y ->
          vertexCount (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \(x :: F) y ->
          vertexCount (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \(x :: F) y ->
          edgeCount   (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \(x :: F) y ->
          edgeCount   (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y" $ \(x :: F) y ->
          edgeCount   (connect x y) >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \(x :: F) y ->
          edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "size        (connect x y) == size x        + size y" $ \(x :: F) y ->
          size        (connect x y) == size x        + size y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount (connect 1 2 :: F) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount   (connect 1 2 :: F) == 1

    putStrLn "\n============ Fold.vertices ============"
    test "vertices []            == empty" $
          vertices []            == (empty :: F)

    test "vertices [x]           == vertex x" $ \(x :: Int) ->
          vertices [x]           == (vertex x :: F)

    test "hasVertex x . vertices == elem x" $ \x (xs :: [Int]) ->
         (hasVertex x . vertices) xs == elem x xs

    test "vertexCount . vertices == length . nub" $ \(xs :: [Int]) ->
         (vertexCount . vertices) xs == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet   . vertices) xs == Set.fromList xs

    putStrLn "\n============ Fold.edges ============"
    test "edges []          == empty" $
          edges []          == (empty :: F)

    test "edges [(x,y)]     == edge x y" $ \(x :: Int) y ->
          edges [(x,y)]     == (edge x y :: F)

    test "edgeCount . edges == length . nub" $ \(xs :: [(Int, Int)]) ->
         (edgeCount . edges) xs == (length . nubOrd) xs

    putStrLn "\n============ Fold.overlays ============"
    test "overlays []        == empty" $
          overlays []        == (empty :: F)

    test "overlays [x]       == x" $ \(x :: F) ->
          overlays [x]       == x

    test "overlays [x,y]     == overlay x y" $ \(x :: F) y ->
          overlays [x,y]     == overlay x y

    test "isEmpty . overlays == all isEmpty" $ \(xs :: [F]) ->
         (isEmpty . overlays) xs == all isEmpty xs

    putStrLn "\n============ Fold.connects ============"
    test "connects []        == empty" $
          connects []        == (empty :: F)

    test "connects [x]       == x" $ \(x :: F) ->
          connects [x]       == x

    test "connects [x,y]     == connect x y" $ \(x :: F) y ->
          connects [x,y]     == connect x y

    test "isEmpty . connects == all isEmpty" $ \(xs :: [F]) ->
         (isEmpty . connects) xs == all isEmpty xs

    putStrLn "\n============ Fold.graph ============"
    test "graph []  []      == empty" $
          graph []  []      == (empty :: F)

    test "graph [x] []      == vertex x" $ \(x :: Int) ->
          graph [x] []      == (vertex x :: F)

    test "graph []  [(x,y)] == edge x y" $ \(x :: Int) y ->
          graph []  [(x,y)] == (edge x y :: F)

    test "graph vs  es      == overlay (vertices vs) (edges es)" $ \(vs :: [Int]) es ->
          graph vs  es      == (overlay (vertices vs) (edges es) :: F)

    putStrLn "\n============ Fold.foldg ============"
    test "foldg empty vertex        overlay connect        == id" $ \(x :: F) ->
          foldg empty vertex        overlay connect x      == x

    test "foldg empty vertex        overlay (flip connect) == transpose" $ \(x :: F) ->
          foldg empty vertex        overlay (flip connect)x== (transpose x :: F)

    test "foldg []    return        (++)    (++)           == toList" $ \(x :: F) ->
          foldg []    return        (++)    (++) x         == toList x

    test "foldg 0     (const 1)     (+)     (+)            == length" $ \(x :: F) ->
          foldg 0     (const 1)     (+)     (+) x          == length x

    test "foldg 1     (const 1)     (+)     (+)            == size" $ \(x :: F) ->
          foldg 1     (const 1)     (+)     (+) x          == size x

    test "foldg True  (const False) (&&)    (&&)           == isEmpty" $ \(x :: F) ->
          foldg True  (const False) (&&)    (&&) x         == isEmpty x

    putStrLn "\n============ Fold.isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ \(x :: F) ->
          isSubgraphOf empty         x             == True

    test "isSubgraphOf (vertex x)    empty         == False" $ \x ->
          isSubgraphOf (vertex x)   (empty :: F)   == False

    test "isSubgraphOf x             (overlay x y) == True" $ \(x :: F) y ->
          isSubgraphOf x             (overlay x y) == True

    test "isSubgraphOf (overlay x y) (connect x y) == True" $ \(x :: F) y ->
          isSubgraphOf (overlay x y) (connect x y) == True

    test "isSubgraphOf (path xs)     (circuit xs)  == True" $ \xs ->
          isSubgraphOf (path xs :: F)(circuit xs)  == True

    putStrLn "\n============ Fold.isEmpty ============"
    test "isEmpty empty                       == True" $
          isEmpty (empty :: F)                == True

    test "isEmpty (overlay empty empty)       == True" $
          isEmpty (overlay empty empty :: F)  == True

    test "isEmpty (vertex x)                  == False" $ \(x :: Int) ->
          isEmpty (vertex x)                  == False

    test "isEmpty (removeVertex x $ vertex x) == True" $ \(x :: Int) ->
          isEmpty (removeVertex x $ vertex x) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \(x :: Int) y ->
          isEmpty (removeEdge x y $ edge x y) == False

    putStrLn "\n============ Fold.size ============"
    test "size empty         == 1" $
          size (empty :: F)  == 1

    test "size (vertex x)    == 1" $ \(x :: Int) ->
          size (vertex x)    == 1

    test "size (overlay x y) == size x + size y" $ \(x :: F) y ->
          size (overlay x y) == size x + size y

    test "size (connect x y) == size x + size y" $ \(x :: F) y ->
          size (connect x y) == size x + size y

    test "size x             >= 1" $ \(x :: F) ->
          size x             >= 1

    test "size x             >= vertexCount x" $ \(x :: F) ->
          size x             >= vertexCount x

    putStrLn "\n============ Fold.hasVertex ============"
    test "hasVertex x empty            == False" $ \(x :: Int) ->
          hasVertex x empty            == False

    test "hasVertex x (vertex x)       == True" $ \(x :: Int) ->
          hasVertex x (vertex x)       == True

    test "hasVertex x . removeVertex x == const False" $ \(x :: Int) y ->
          hasVertex x (removeVertex x y)==const False y

    putStrLn "\n============ Fold.hasEdge ============"
    test "hasEdge x y empty            == False" $ \(x :: Int) y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y z ->
          hasEdge x y (removeEdge x y z)==const False z

    putStrLn "\n============ Fold.vertexCount ============"
    test "vertexCount empty      == 0" $
          vertexCount (empty :: F) == 0

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "vertexCount            == length . vertexList" $ \(x :: F) ->
          vertexCount x          == (length . vertexList) x

    putStrLn "\n============ Fold.edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount (empty :: F) == 0

    test "edgeCount (vertex x) == 0" $ \(x :: Int) ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1" $ \(x :: Int) y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \(x :: F) ->
          edgeCount x          == (length . edgeList) x

    putStrLn "\n============ Fold.vertexList ============"
    test "vertexList empty      == []" $
          vertexList (empty :: F) == []

    test "vertexList (vertex x) == [x]" $ \(x :: Int) ->
          vertexList (vertex x) == [x]

    test "vertexList . vertices == nub . sort" $ \(xs :: [Int]) ->
         (vertexList . vertices) xs == (nubOrd . sort) xs

    putStrLn "\n============ Fold.edgeList ============"
    test "edgeList empty          == []" $
          edgeList (empty :: F )  == []

    test "edgeList (vertex x)     == []" $ \(x :: Int) ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]" $ \(x :: Int) y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [3,1]) == [(2,1), (2,3)]" $
          edgeList (star 2 [3,1]) == [(2,1), (2,3 :: Int)]

    test "edgeList . edges        == nub . sort" $ \(xs :: [(Int, Int)]) ->
         (edgeList . edges) xs    == (nubOrd . sort) xs

    putStrLn "\n============ Fold.vertexSet ============"
    test "vertexSet empty      == Set.empty" $
          vertexSet(empty :: F)== Set.empty

    test "vertexSet . vertex   == Set.singleton" $ \(x :: Int) ->
         (vertexSet . vertex) x== Set.singleton x

    test "vertexSet . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet . vertices) xs == Set.fromList xs

    test "vertexSet . clique   == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet . clique) xs == Set.fromList xs

    putStrLn "\n============ Fold.vertexIntSet ============"
    test "vertexIntSet empty      == IntSet.empty" $
          vertexIntSet(empty :: F)== IntSet.empty

    test "vertexIntSet . vertex   == IntSet.singleton" $ \(x :: Int) ->
         (vertexIntSet . vertex) x== IntSet.singleton x

    test "vertexIntSet . vertices == IntSet.fromList" $ \(xs :: [Int]) ->
         (vertexIntSet . vertices) xs == IntSet.fromList xs

    test "vertexIntSet . clique   == IntSet.fromList" $ \(xs :: [Int]) ->
         (vertexIntSet . clique) xs == IntSet.fromList xs

    putStrLn "\n============ Fold.edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet (empty :: F) == Set.empty

    test "edgeSet (vertex x) == Set.empty" $ \(x :: Int) ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \(x :: Int) y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges    == Set.fromList" $ \(xs :: [(Int, Int)]) ->
         (edgeSet . edges) xs== Set.fromList xs

    putStrLn "\n============ Fold.path ============"
    test "path []    == empty" $
          path []    == (empty :: F)

    test "path [x]   == vertex x" $ \(x :: Int) ->
          path [x]   == (vertex x :: F)

    test "path [x,y] == edge x y" $ \(x :: Int) y ->
          path [x,y] == (edge x y :: F)

    putStrLn "\n============ Fold.circuit ============"
    test "circuit []    == empty" $
          circuit []    == (empty :: F)

    test "circuit [x]   == edge x x" $ \(x :: Int) ->
          circuit [x]   == (edge x x :: F)

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \(x :: Int) y ->
          circuit [x,y] == (edges [(x,y), (y,x)] :: F)

    putStrLn "\n============ Fold.clique ============"
    test "clique []      == empty" $
          clique []      == (empty :: F)

    test "clique [x]     == vertex x" $ \(x :: Int) ->
          clique [x]     == (vertex x :: F)

    test "clique [x,y]   == edge x y" $ \(x :: Int) y ->
          clique [x,y]   == (edge x y :: F)

    test "clique [x,y,z] == edges [(x,y), (x,z), (y,z)]" $ \(x :: Int) y z ->
          clique [x,y,z] == (edges [(x,y), (x,z), (y,z)] :: F)

    putStrLn "\n============ Fold.biclique ============"
    test "biclique []      []      == empty" $
          biclique []      []      == (empty :: F)

    test "biclique [x]     []      == vertex x" $ \(x :: Int) ->
          biclique [x]     []      == (vertex x :: F)

    test "biclique []      [y]     == vertex y" $ \(y :: Int) ->
          biclique []      [y]     == (vertex y :: F)

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \(x1 :: Int) x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == (edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)] :: F)

    putStrLn "\n============ Fold.star ============"
    test "star x []    == vertex x" $ \(x :: Int) ->
          star x []    == (vertex x :: F)

    test "star x [y]   == edge x y" $ \(x :: Int) y ->
          star x [y]   == (edge x y :: F)

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \(x :: Int) y z ->
          star x [y,z] == (edges [(x,y), (x,z)] :: F)

    putStrLn "\n============ Fold.tree ============"
    test "tree (Node x [])                                         == vertex x" $ \x ->
          tree (Node x [])                                         ==(vertex x :: F)

    test "tree (Node x [Node y [Node z []]])                       == path [x,y,z]" $ \x y z ->
          tree (Node x [Node y [Node z []]])                       ==(path [x,y,z] :: F)

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \x y z ->
          tree (Node x [Node y [], Node z []])                     ==(star x [y,z] :: F)

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) ==(edges [(1,2), (1,3), (3,4), (3,5)] :: F)

    putStrLn "\n============ Fold.forest ============"
    test "forest []                                                  == empty" $
          forest []                                                  == (empty :: F)

    test "forest [x]                                                 == tree x" $ \x ->
          forest [x]                                                 == (tree x :: F)

    test "forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]" $
          forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] ==(edges [(1,2), (1,3), (4,5)] :: F)

    test "forest                                                     == overlays . map tree" $ \x ->
         (forest x)                                                  ==((overlays . map tree) x :: F)

    putStrLn "\n============ Fold.mesh ============"
    test "mesh xs     []   == empty" $ \xs ->
          mesh xs     []   == (empty :: Fold (Int, Int))

    test "mesh []     ys   == empty" $ \ys ->
          mesh []     ys   == (empty :: Fold (Int, Int))

    test "mesh [x]    [y]  == vertex (x, y)" $ \(x :: Int) (y :: Int) ->
          mesh [x]    [y]  == (vertex (x, y) :: Fold (Int, Int))

    test "mesh xs     ys   == box (path xs) (path ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          mesh xs     ys   == (box (path xs) (path ys) :: Fold (Int, Int))

    test ("mesh [1..3] \"ab\" == <correct result>") $
         (mesh [1..3] "ab" :: Fold (Int, Char)) == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(2,'b')), ((2,'a'),(2,'b'))
                                                         , ((2,'a'),(3,'a')), ((2,'b'),(3,'b')), ((3,'a'),(3,'b')) ]

    putStrLn "\n============ Fold.torus ============"
    test "torus xs    []   == empty" $ \xs ->
          torus xs    []   == (empty :: Fold (Int, Int))

    test "torus []    ys   == empty" $ \ys ->
          torus []    ys   == (empty :: Fold (Int, Int))

    test "torus [x]   [y]  == edge (x, y) (x, y)" $ \(x :: Int) (y :: Int) ->
          torus [x]   [y]  == (edge (x, y) (x, y) :: Fold (Int, Int))

    test "torus xs    ys   == box (circuit xs) (circuit ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          torus xs    ys   == (box (circuit xs) (circuit ys) :: Fold (Int, Int))

    test ("torus [1,2] \"ab\" == <correct result>") $
         (torus [1,2] "ab" :: Fold (Int, Char)) == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(1,'a')), ((1,'b'),(2,'b'))
                                                         , ((2,'a'),(1,'a')), ((2,'a'),(2,'b')), ((2,'b'),(1,'b')), ((2,'b'),(2,'a')) ]

    putStrLn "\n============ Fold.deBruijn ============"
    test "deBruijn k []    == empty" $ \k ->
          deBruijn k []    == (empty :: Fold [Int])

    test "deBruijn 1 [0,1] == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]" $
          deBruijn 1 [0,1] == (edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ] :: Fold [Int])

    test "deBruijn 2 \"0\"   == edge \"00\" \"00\"" $
          deBruijn 2 "0"   == (edge "00" "00" :: Fold String)

    test ("deBruijn 2 \"01\"  == <correct result>") $
          (deBruijn 2 "01" :: Fold String) == edges [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
                                                    , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]

    putStrLn "\n============ Fold.removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \(x :: Int) ->
          removeVertex x (vertex x)       == (empty :: F)

    test "removeVertex x . removeVertex x == removeVertex x" $ \x (y :: F) ->
         (removeVertex x . removeVertex x)y==(removeVertex x y :: F)

    putStrLn "\n============ Fold.removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices [x, y]" $ \(x :: Int) y ->
          removeEdge x y (edge x y)       == (vertices [x, y] :: F)

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \(x :: Int) y z ->
         (removeEdge x y . removeEdge x y)z==(removeEdge x y z :: F)

    test "removeEdge x y . removeVertex x == removeVertex x" $ \(x :: Int) y z ->
         (removeEdge x y . removeVertex x)z==(removeVertex x z :: F)

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == (1 * 2 * (2 :: F))

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == (1 * 1 + 2 * (2 :: F))

    putStrLn "\n============ Fold.replaceVertex ============"
    test "replaceVertex x x            == id" $ \x (y :: F) ->
          replaceVertex x x y          == y

    test "replaceVertex x y (vertex x) == vertex y" $ \x (y :: Int) ->
          replaceVertex x y (vertex x) == (vertex y :: F)

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y z ->
          replaceVertex x y z          == (mergeVertices (== x) y z :: F)

    putStrLn "\n============ Fold.mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \x (y :: F) ->
          mergeVertices (const False) x y  == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y (z :: F) ->
          mergeVertices (== x) y z         == (replaceVertex x y z :: F)

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 (0 * 2)     == (1 * 1 :: F)

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 (3 + 4 * 5) == (4 * 1 :: F)

    putStrLn "\n============ Fold.splitVertex ============"
    test "splitVertex x []                   == removeVertex x" $ \x (y :: F) ->
         (splitVertex x []) y                == (removeVertex x y :: F)

    test "splitVertex x [x]                  == id" $ \x (y :: F) ->
         (splitVertex x [x]) y               == y

    test "splitVertex x [y]                  == replaceVertex x y" $ \x y (z :: F) ->
         (splitVertex x [y]) z               == (replaceVertex x y z :: F)

    test "splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)" $
         (splitVertex 1 [0, 1] $ 1 * (2 + 3))== ((0 + 1) * (2 + 3 :: F))

    putStrLn "\n============ Fold.transpose ============"
    test "transpose empty       == empty" $
          transpose empty       == (empty :: F)

    test "transpose (vertex x)  == vertex x" $ \(x :: Int) ->
          transpose (vertex x)  == (vertex x :: F)

    test "transpose (edge x y)  == edge y x" $ \(x :: Int) y ->
          transpose (edge x y)  == (edge y x :: F)

    test "transpose . transpose == id" $ \(x :: F) ->
         (transpose . transpose) x == x

    test "transpose . path      == path    . reverse" $ \(xs :: [Int]) ->
         (transpose . path) xs  == ((path . reverse) xs :: F)

    test "transpose . circuit   == circuit . reverse" $ \(xs :: [Int]) ->
         (transpose . circuit) xs == ((circuit . reverse) xs :: F)

    test "transpose . clique    == clique  . reverse" $ \(xs :: [Int]) ->
         (transpose . clique) xs == ((clique . reverse) xs :: F)

    test "transpose (box x y)   == box (transpose x) (transpose y)" $ mapSize (min 10) $ \(x :: F) (y :: F) ->
          transpose (box x y)   == (box (transpose x) (transpose y) :: Fold (Int, Int))

    test "edgeList . transpose  == sort . map swap . edgeList" $ \(x :: F) ->
         (edgeList . transpose) x == (sort . map swap . edgeList) x

    putStrLn "\n============ Fold.gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f :: II) ->
          gmap f empty      == (empty :: F)

    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f :: II) x ->
          gmap f (vertex x) == (vertex (f x) :: F)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f :: II) x y ->
          gmap f (edge x y) == (edge (f x) (f y) :: F)

    test "gmap id           == id" $ \(x :: F) ->
          gmap id x         == x

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f :: II) (apply -> g :: II) (x :: F) ->
         (gmap f . gmap g) x== (gmap (f . g) x :: F)

    putStrLn "\n============ Fold.bind ============"
    test "bind empty f         == empty" $ \(apply -> f :: IF) ->
          bind empty f         == empty

    test "bind (vertex x) f    == f x" $ \(apply -> f :: IF) x ->
          bind (vertex x) f    == f x

    test "bind (edge x y) f    == connect (f x) (f y)" $ \(apply -> f :: IF) x y ->
          bind (edge x y) f    == connect (f x) (f y)

    test "bind (vertices xs) f == overlays (map f xs)" $ mapSize (min 10) $ \xs (apply -> f :: IF) ->
          bind (vertices xs) f == overlays (map f xs)

    test "bind x (const empty) == empty" $ \(x :: F) ->
          bind x (const empty) == (empty :: F)

    test "bind x vertex        == x" $ \(x :: F) ->
          bind x vertex        == x

    test "bind (bind x f) g    == bind x (\\y -> bind (f y) g)" $ mapSize (min 10) $ \x (apply -> f :: IF) (apply -> g :: IF) ->
          bind (bind x f) g    == bind x (\y -> bind (f y) g)

    putStrLn "\n============ Fold.induce ============"
    test "induce (const True)  x      == x" $ \(x :: F) ->
          induce (const True)  x      == x

    test "induce (const False) x      == empty" $ \(x :: F) ->
          induce (const False) x      == (empty :: F)

    test "induce (/= x)               == removeVertex x" $ \x (y :: F) ->
          induce (/= x) y             == (removeVertex x y :: F)

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p :: IB) (apply -> q :: IB) (y :: F) ->
         (induce p . induce q) y      == (induce (\x -> p x && q x) y :: F)

    test "isSubgraphOf (induce p x) x == True" $ \(apply -> p :: IB) (x :: F) ->
          isSubgraphOf (induce p x) x == True

    putStrLn "\n============ Fold.simplify ============"
    test "simplify              == id" $ \(x :: F) ->
          simplify x            == x

    test "size (simplify x)     <= size x" $ \(x :: F) ->
          size (simplify x)     <= size x

    putStrLn "\n============ Fold.box ============"
    let unit = fmap $ \(a, ()) -> a
        comm = fmap $ \(a,  b) -> (b, a)
    test "box x y             ~~ box y x" $ mapSize (min 10) $ \(x :: F) (y :: F) ->
          comm (box x y)      == (box y x :: Fold (Int, Int))

    test "box x (overlay y z) == overlay (box x y) (box x z)" $ mapSize (min 10) $ \(x :: F) (y :: F) z ->
          box x (overlay y z) == (overlay (box x y) (box x z) :: Fold (Int, Int))

    test "box x (vertex ())   ~~ x" $ mapSize (min 10) $ \(x :: F) ->
     unit(box x (vertex ()))  == x

    test "box x empty         ~~ empty" $ mapSize (min 10) $ \(x :: F) ->
     unit(box x empty)        == empty

    let assoc = fmap $ \(a, (b, c)) -> ((a, b), c)
    test "box x (box y z)     ~~ box (box x y) z" $ mapSize (min 10) $ \(x :: F) (y :: F) (z :: F) ->
      assoc (box x (box y z)) == (box (box x y) z :: Fold ((Int, Int), Int))
