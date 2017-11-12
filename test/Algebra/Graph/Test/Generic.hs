{-# LANGUAGE GADTs, RankNTypes, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Generic
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Generic graph API testing.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Generic (
    -- * Generic tests
    Testsuite, testsuite, HTestsuite, hTestsuite, testShow, testFromAdjacencyList,
    testBasicPrimitives, testFoldg, testIsSubgraphOf, testSize, testProperties,
    testAdjacencyList, testPreSet, testPostSet, testPostIntSet, testGraphFamilies,
    testTransformations, testDfsForest, testDfsForestFrom, testDfs, testTopSort,
    testIsTopSort, testSplitVertex, testBind, testSimplify
  ) where

import Prelude ()
import Prelude.Compat
import Data.Orphans ()

import Data.Foldable (toList)
import Data.List (nub, sort)
import Data.Tree
import Data.Tuple

import Algebra.Graph.Class (Graph (..))
import Algebra.Graph.Test
import Algebra.Graph.Test.API

import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet

data Testsuite where
    Testsuite :: (Arbitrary g, Eq g, GraphAPI g, Num g, Show g, Vertex g ~ Int)
              => String -> (forall r. (g -> r) -> g -> r) -> Testsuite

testsuite :: (Arbitrary g, Eq g, GraphAPI g, Num g, Show g, Vertex g ~ Int)
          => String -> g -> Testsuite
testsuite prefix g = Testsuite prefix (\f x -> f (x `asTypeOf` g))

data HTestsuite where
    HTestsuite :: (Arbitrary g, Eq g, GraphAPI g, Num g, Show g, Vertex g ~ Int,
                   g ~ f Int, Foldable f)
               => String -> (forall r. (g -> r) -> g -> r) -> HTestsuite

hTestsuite :: (Arbitrary g, Eq g, GraphAPI g, Num g, Show g, Vertex g ~ Int,
               g ~ f Int, Foldable f) => String -> g -> HTestsuite
hTestsuite prefix g = HTestsuite prefix (\f x -> f (x `asTypeOf` g))

testBasicPrimitives :: Testsuite -> IO ()
testBasicPrimitives = mconcat [ testEmpty
                              , testVertex
                              , testEdge
                              , testOverlay
                              , testConnect
                              , testVertices
                              , testEdges
                              , testOverlays
                              , testConnects
                              , testGraph ]

testProperties :: Testsuite -> IO ()
testProperties = mconcat [ testIsEmpty
                         , testHasVertex
                         , testHasEdge
                         , testVertexCount
                         , testEdgeCount
                         , testVertexList
                         , testEdgeList
                         , testVertexSet
                         , testVertexIntSet
                         , testEdgeSet ]

testGraphFamilies :: Testsuite -> IO ()
testGraphFamilies = mconcat [ testPath
                            , testCircuit
                            , testClique
                            , testBiclique
                            , testStar
                            , testTree
                            , testForest ]

testTransformations :: Testsuite -> IO ()
testTransformations = mconcat [ testRemoveVertex
                              , testRemoveEdge
                              , testReplaceVertex
                              , testMergeVertices
                              , testTranspose
                              , testGmap
                              , testInduce ]

testShow :: Testsuite -> IO ()
testShow (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "Show ============"
    test "show (empty     :: IntAdjacencyMap) == \"empty\"" $
          show % empty                        == "empty"

    test "show (1         :: IntAdjacencyMap) == \"vertex 1\"" $
          show % 1                            == "vertex 1"

    test "show (1 + 2     :: IntAdjacencyMap) == \"vertices [1,2]\"" $
          show % (1 + 2)                      == "vertices [1,2]"

    test "show (1 * 2     :: IntAdjacencyMap) == \"edge 1 2\"" $
          show % (1 * 2)                      == "edge 1 2"

    test "show (1 * 2 * 3 :: IntAdjacencyMap) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show % (1 * 2 * 3)                  == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: IntAdjacencyMap) == \"graph [1,2,3] [(1,2)]\"" $
          show % (1 * 2 + 3)                  == "graph [1,2,3] [(1,2)]"

testEmpty :: Testsuite -> IO ()
testEmpty (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "empty ============"
    test "isEmpty     empty == True" $
          isEmpty   % empty == True

    test "hasVertex x empty == False" $ \x ->
          hasVertex x % empty == False

    test "vertexCount empty == 0" $
          vertexCount % empty == 0

    test "edgeCount   empty == 0" $
          edgeCount % empty == 0

testVertex :: Testsuite -> IO ()
testVertex (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertex ============"
    test "isEmpty     (vertex x) == False" $ \x ->
          isEmpty    % vertex x  == False

    test "hasVertex x (vertex x) == True" $ \x ->
          hasVertex x % vertex x == True

    test "hasVertex 1 (vertex 2) == False" $
          hasVertex 1 % vertex 2 == False

    test "vertexCount (vertex x) == 1" $ \x ->
          vertexCount % vertex x == 1

    test "edgeCount   (vertex x) == 0" $ \x ->
          edgeCount  % vertex x  == 0

testEdge :: Testsuite -> IO ()
testEdge (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \x y ->
          edge x y               == connect (vertex x) % vertex y

    test "hasEdge x y (edge x y) == True" $ \x y ->
          hasEdge x y % edge x y == True

    test "edgeCount   (edge x y) == 1" $ \x y ->
          edgeCount %  edge x y  == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount % edge 1 1 == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount % edge 1 2 == 2

testOverlay :: Testsuite -> IO ()
testOverlay (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty   %  overlay x y  == (isEmpty  x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z % overlay x y == (hasVertex z x || hasVertex z y)

    test "vertexCount (overlay x y) >= vertexCount x" $ \x y ->
          vertexCount % overlay x y >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount % overlay x y <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x" $ \x y ->
          edgeCount %  overlay x y  >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \x y ->
          edgeCount %  overlay x y  <= edgeCount x   + edgeCount y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount % overlay 1 2 == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount %  overlay 1 2  == 0

testConnect :: Testsuite -> IO ()
testConnect (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "connect ============"
    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty    % connect x y  == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z % connect x y == (hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x" $ \x y ->
          vertexCount % connect x y >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount % connect x y <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \x y ->
          edgeCount  % connect x y  >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \x y ->
          edgeCount  % connect x y  >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y" $ \x y ->
          edgeCount  % connect x y  >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \x y ->
          edgeCount  % connect x y  <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount % connect 1 2 == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount  % connect 1 2  == 1

testVertices :: Testsuite -> IO ()
testVertices (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertices ============"
    test "vertices []            == empty" $
          vertices []            == id % empty

    test "vertices [x]           == vertex x" $ \x ->
          vertices [x]           == id % vertex x

    test "hasVertex x . vertices == elem x" $ \x xs ->
          hasVertex x % vertices xs == elem x xs

    test "vertexCount . vertices == length . nub" $ \xs ->
          vertexCount % vertices xs == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \xs ->
          vertexSet % vertices xs == Set.fromList xs

testEdges :: Testsuite -> IO ()
testEdges (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "edges ============"
    test "edges []          == empty" $
          edges []          == id % empty

    test "edges [(x,y)]     == edge x y" $ \x y ->
          edges [(x,y)]     == id % edge x y

    test "edgeCount . edges == length . nub" $ \xs ->
          edgeCount % edges xs == (length . nubOrd) xs

testOverlays :: Testsuite -> IO ()
testOverlays (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "overlays ============"
    test "overlays []        == empty" $
          overlays []        == id % empty

    test "overlays [x]       == x" $ \x ->
          overlays [x]       == id % x

    test "overlays [x,y]     == overlay x y" $ \x y ->
          overlays [x,y]     == id % overlay x y

    test "isEmpty . overlays == all isEmpty" $ mapSize (min 10) $ \xs ->
          isEmpty % overlays xs == all isEmpty xs

testConnects :: Testsuite -> IO ()
testConnects (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "connects ============"
    test "connects []        == empty" $
          connects []        == id % empty

    test "connects [x]       == x" $ \x ->
          connects [x]       == id % x

    test "connects [x,y]     == connect x y" $ \x y ->
          connects [x,y]     == id % connect x y

    test "isEmpty . connects == all isEmpty" $ mapSize (min 10) $ \xs ->
          isEmpty % connects xs == all isEmpty xs

testGraph :: Testsuite -> IO ()
testGraph (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "graph ============"
    test "graph []  []      == empty" $
          graph []  []      == id % empty

    test "graph [x] []      == vertex x" $ \x ->
          graph [x] []      == id % vertex x

    test "graph []  [(x,y)] == edge x y" $ \x y ->
          graph []  [(x,y)] == id % edge x y

    test "graph vs  es      == overlay (vertices vs) (edges es)" $ \vs es ->
          graph vs  es      == overlay (vertices vs) % edges es

testFromAdjacencyList :: Testsuite -> IO ()
testFromAdjacencyList (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "fromAdjacencyList ============"
    test "fromAdjacencyList []                                  == empty" $
          fromAdjacencyList []                                  == id % empty

    test "fromAdjacencyList [(x, [])]                           == vertex x" $ \x ->
          fromAdjacencyList [(x, [])]                           == id % vertex x

    test "fromAdjacencyList [(x, [y])]                          == edge x y" $ \x y ->
          fromAdjacencyList [(x, [y])]                          == id % edge x y

    test "fromAdjacencyList . adjacencyList                     == id" $ \x ->
         (fromAdjacencyList . adjacencyList) % x                == x

    test "overlay (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyList xs) % fromAdjacencyList ys == fromAdjacencyList (xs ++ ys)

testFoldg :: HTestsuite -> IO ()
testFoldg (HTestsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "foldg ============"
    test "foldg empty vertex        overlay connect        == id" $ \x ->
          foldg empty vertex        overlay connect x      == id % x

    test "foldg empty vertex        overlay (flip connect) == transpose" $ \x ->
          foldg empty vertex        overlay (flip connect)x== transpose % x

    test "foldg []    return        (++)    (++)           == toList" $ \x ->
          foldg []    return        (++)    (++) x         == toList % x

    test "foldg 0     (const 1)     (+)     (+)            == length" $ \x ->
          foldg 0     (const 1)     (+)     (+) x          == length % x

    test "foldg 1     (const 1)     (+)     (+)            == size" $ \x ->
          foldg 1     (const 1)     (+)     (+) x          == size % x

    test "foldg True  (const False) (&&)    (&&)           == isEmpty" $ \x ->
          foldg True  (const False) (&&)    (&&) x         == isEmpty % x

testIsSubgraphOf :: Testsuite -> IO ()
testIsSubgraphOf (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "isSubgraphOf ============"
    test "isSubgraphOf empty         x             == True" $ \x ->
          isSubgraphOf empty       % x             == True

    test "isSubgraphOf (vertex x)    empty         == False" $ \x ->
          isSubgraphOf (vertex x)  % empty         == False

    test "isSubgraphOf x             (overlay x y) == True" $ \x y ->
          isSubgraphOf x            % overlay x y  == True

    test "isSubgraphOf (overlay x y) (connect x y) == True" $ \x y ->
          isSubgraphOf (overlay x y) % connect x y == True

    test "isSubgraphOf (path xs)     (circuit xs)  == True" $ \xs ->
          isSubgraphOf (path xs)    % circuit xs   == True

testIsEmpty :: Testsuite -> IO ()
testIsEmpty (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "isEmpty ============"
    test "isEmpty empty                       == True" $
          isEmpty % empty                     == True

    test "isEmpty (overlay empty empty)       == True" $
          isEmpty % overlay empty empty       == True

    test "isEmpty (vertex x)                  == False" $ \x ->
          isEmpty % vertex x                  == False

    test "isEmpty (removeVertex x $ vertex x) == True" $ \x ->
          isEmpty (removeVertex x % vertex x) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \x y ->
          isEmpty (removeEdge x y % edge x y) == False

testSize :: Testsuite -> IO ()
testSize (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "size ============"
    test "size empty         == 1" $
          size % empty       == 1

    test "size (vertex x)    == 1" $ \x ->
          size % vertex x    == 1

    test "size (overlay x y) == size x + size y" $ \x y ->
          size % overlay x y == size x + size y

    test "size (connect x y) == size x + size y" $ \x y ->
          size % connect x y == size x + size y

    test "size x             >= 1" $ \x ->
          size % x           >= 1

    test "size x             >= vertexCount x" $ \x ->
          size % x           >= vertexCount x

testHasVertex :: Testsuite -> IO ()
testHasVertex (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "hasVertex ============"
    test "hasVertex x empty            == False" $ \x ->
          hasVertex x % empty          == False

    test "hasVertex x (vertex x)       == True" $ \x ->
          hasVertex x % vertex x     == True

    test "hasVertex x . removeVertex x == const False" $ \x y ->
         (hasVertex x . removeVertex x) y == const False % y

testHasEdge :: Testsuite -> IO ()
testHasEdge (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "hasEdge ============"
    test "hasEdge x y empty            == False" $ \x y ->
          hasEdge x y % empty          == False

    test "hasEdge x y (vertex z)       == False" $ \x y z ->
          hasEdge x y % vertex z       == False

    test "hasEdge x y (edge x y)       == True" $ \x y ->
          hasEdge x y % edge x y       == True

    test "hasEdge x y . removeEdge x y == const False" $ \x y z ->
         (hasEdge x y . removeEdge x y) z == const False % z

    test "hasEdge x y                  == elem (x,y) . edgeList" $ \x y z -> do
        (u, v) <- elements ((x, y) : edgeList z)
        return $ hasEdge u v z == elem (u, v) (edgeList % z)

testVertexCount :: Testsuite -> IO ()
testVertexCount (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexCount ============"
    test "vertexCount empty      == 0" $
          vertexCount % empty    == 0

    test "vertexCount (vertex x) == 1" $ \x ->
          vertexCount % vertex x == 1

    test "vertexCount            == length . vertexList" $ \x ->
          vertexCount % x        == (length . vertexList) x

testEdgeCount :: Testsuite -> IO ()
testEdgeCount (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount % empty    == 0

    test "edgeCount (vertex x) == 0" $ \x ->
          edgeCount % vertex x == 0

    test "edgeCount (edge x y) == 1" $ \x y ->
          edgeCount % edge x y == 1

    test "edgeCount            == length . edgeList" $ \x ->
          edgeCount % x        == (length . edgeList) x

testVertexList :: Testsuite -> IO ()
testVertexList (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexList ============"
    test "vertexList empty      == []" $
          vertexList % empty    == []

    test "vertexList (vertex x) == [x]" $ \x ->
          vertexList % vertex x == [x]

    test "vertexList . vertices == nub . sort" $ \xs ->
          vertexList % vertices xs == (nubOrd . sort) xs

testEdgeList :: Testsuite -> IO ()
testEdgeList (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeList ============"
    test "edgeList empty          == []" $
          edgeList % empty        == []

    test "edgeList (vertex x)     == []" $ \x ->
          edgeList % vertex x     == []

    test "edgeList (edge x y)     == [(x,y)]" $ \x y ->
          edgeList % edge x y     == [(x,y)]

    test "edgeList (star 2 [3,1]) == [(2,1), (2,3)]" $
          edgeList % star 2 [3,1] == [(2,1), (2,3)]

    test "edgeList . edges        == nub . sort" $ \xs ->
          edgeList % edges xs     == (nubOrd . sort) xs

testAdjacencyList :: Testsuite -> IO ()
testAdjacencyList (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "adjacencyList ============"
    test "adjacencyList empty          == []" $
          adjacencyList % empty        == []

    test "adjacencyList (vertex x)     == [(x, [])]" $ \x ->
          adjacencyList % vertex x     == [(x, [])]

    test "adjacencyList (edge 1 2)     == [(1, [2]), (2, [])]" $
          adjacencyList % edge 1 2     == [(1, [2]), (2, [])]

    test "adjacencyList (star 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]" $
          adjacencyList % star 2 [3,1] == [(1, []), (2, [1,3]), (3, [])]

testVertexSet :: Testsuite -> IO ()
testVertexSet (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexSet ============"
    test "vertexSet empty      == Set.empty" $
          vertexSet % empty    == Set.empty

    test "vertexSet . vertex   == Set.singleton" $ \x ->
          vertexSet % vertex x == Set.singleton x

    test "vertexSet . vertices == Set.fromList" $ \xs ->
          vertexSet % vertices xs == Set.fromList xs

    test "vertexSet . clique   == Set.fromList" $ \xs ->
          vertexSet % clique xs == Set.fromList xs

testVertexIntSet :: Testsuite -> IO ()
testVertexIntSet (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexIntSet ============"
    test "vertexIntSet empty      == IntSet.empty" $
          vertexIntSet % empty    == IntSet.empty

    test "vertexIntSet . vertex   == IntSet.singleton" $ \x ->
          vertexIntSet % vertex x == IntSet.singleton x

    test "vertexIntSet . vertices == IntSet.fromList" $ \xs ->
          vertexIntSet % vertices xs == IntSet.fromList xs

    test "vertexIntSet . clique   == IntSet.fromList" $ \xs ->
          vertexIntSet % clique xs == IntSet.fromList xs

testEdgeSet :: Testsuite -> IO ()
testEdgeSet (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet % empty    == Set.empty

    test "edgeSet (vertex x) == Set.empty" $ \x ->
          edgeSet % vertex x == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \x y ->
          edgeSet % edge x y == Set.singleton (x,y)

    test "edgeSet . edges    == Set.fromList" $ \xs ->
          edgeSet % edges xs == Set.fromList xs

testPreSet :: Testsuite -> IO ()
testPreSet (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "preSet ============"
    test "preSet x empty      == Set.empty" $ \x ->
          preSet x % empty    == Set.empty

    test "preSet x (vertex x) == Set.empty" $ \x ->
          preSet x % vertex x == Set.empty

    test "preSet 1 (edge 1 2) == Set.empty" $
          preSet 1 % edge 1 2 == Set.empty

    test "preSet y (edge x y) == Set.fromList [x]" $ \x y ->
          preSet y % edge x y == Set.fromList [x]

testPostSet :: Testsuite -> IO ()
testPostSet (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "postSet ============"
    test "postSet x empty      == Set.empty" $ \x ->
          postSet x % empty    == Set.empty

    test "postSet x (vertex x) == Set.empty" $ \x ->
          postSet x % vertex x == Set.empty

    test "postSet x (edge x y) == Set.fromList [y]" $ \x y ->
          postSet x % edge x y == Set.fromList [y]

    test "postSet 2 (edge 1 2) == Set.empty" $
          postSet 2 % edge 1 2 == Set.empty

testPostIntSet :: Testsuite -> IO ()
testPostIntSet (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "postIntSet ============"
    test "postIntSet x empty      == IntSet.empty" $ \x ->
          postIntSet x % empty    == IntSet.empty

    test "postIntSet x (vertex x) == IntSet.empty" $ \x ->
          postIntSet x % vertex x == IntSet.empty

    test "postIntSet x (edge x y) == IntSet.fromList [y]" $ \x y ->
          postIntSet x % edge x y == IntSet.fromList [y]

    test "postIntSet 2 (edge 1 2) == IntSet.empty" $
          postIntSet 2 % edge 1 2 == IntSet.empty

testPath :: Testsuite -> IO ()
testPath (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "path ============"
    test "path []    == empty" $
          path []    == id % empty

    test "path [x]   == vertex x" $ \x ->
          path [x]   == id % vertex x

    test "path [x,y] == edge x y" $ \x y ->
          path [x,y] == id % edge x y

testCircuit :: Testsuite -> IO ()
testCircuit (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "circuit ============"
    test "circuit []    == empty" $
          circuit []    == id % empty

    test "circuit [x]   == edge x x" $ \x ->
          circuit [x]   == id % edge x x

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \x y ->
          circuit [x,y] == id % edges [(x,y), (y,x)]

testClique :: Testsuite -> IO ()
testClique (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "clique ============"
    test "clique []         == empty" $
          clique []         == id % empty

    test "clique [x]        == vertex x" $ \x ->
          clique [x]        == id % vertex x

    test "clique [x,y]      == edge x y" $ \x y ->
          clique [x,y]      == id % edge x y

    test "clique [x,y,z]    == edges [(x,y), (x,z), (y,z)]" $ \x y z ->
          clique [x,y,z]    == id % edges [(x,y), (x,z), (y,z)]

    test "clique (xs ++ ys) == connect (clique xs) (clique ys)" $ \xs ys ->
          clique (xs ++ ys) == connect (clique xs) % clique ys

testBiclique :: Testsuite -> IO ()
testBiclique (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "biclique ============"
    test "biclique []      []      == empty" $
          biclique []      []      == id % empty

    test "biclique [x]     []      == vertex x" $ \x ->
          biclique [x]     []      == id % vertex x

    test "biclique []      [y]     == vertex y" $ \y ->
          biclique []      [y]     == id % vertex y

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \x1 x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == id % edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]

    test "biclique xs      ys      == connect (vertices xs) (vertices ys)" $ \xs ys ->
          biclique xs      ys      == connect (vertices xs) % vertices ys

testStar :: Testsuite -> IO ()
testStar (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "star ============"
    test "star x []    == vertex x" $ \x ->
          star x []    == id % vertex x

    test "star x [y]   == edge x y" $ \x y ->
          star x [y]   == id % edge x y

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \x y z ->
          star x [y,z] == id % edges [(x,y), (x,z)]

testTree :: Testsuite -> IO ()
testTree (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "tree ============"
    test "tree (Node x [])                                         == vertex x" $ \x ->
          tree (Node x [])                                         == id % vertex x

    test "tree (Node x [Node y [Node z []]])                       == path [x,y,z]" $ \x y z ->
          tree (Node x [Node y [Node z []]])                       == id % path [x,y,z]

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \x y z ->
          tree (Node x [Node y [], Node z []])                     == id % star x [y,z]

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == id % edges [(1,2), (1,3), (3,4), (3,5)]

testForest :: Testsuite -> IO ()
testForest (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "forest ============"
    test "forest []                                                  == empty" $
          forest []                                                  == id % empty

    test "forest [x]                                                 == tree x" $ \x ->
          forest [x]                                                 == id % tree x

    test "forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]" $
          forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == id % edges [(1,2), (1,3), (4,5)]

    test "forest                                                     == overlays . map tree" $ \x ->
          forest x                                                   == id % (overlays . map tree) x

testRemoveVertex :: Testsuite -> IO ()
testRemoveVertex (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \x ->
          removeVertex x % vertex x       == empty

    test "removeVertex x . removeVertex x == removeVertex x" $ \x y ->
         (removeVertex x . removeVertex x) y == removeVertex x % y

testRemoveEdge :: Testsuite -> IO ()
testRemoveEdge (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices [x, y]" $ \x y ->
          removeEdge x y % edge x y       == vertices [x, y]

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \x y z ->
         (removeEdge x y . removeEdge x y) z == removeEdge x y % z

    test "removeEdge x y . removeVertex x == removeVertex x" $ \x y z ->
         (removeEdge x y . removeVertex x) z == removeVertex x % z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 % (1 * 1 * 2 * 2) == 1 * 2 * 2

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 % (1 * 1 * 2 * 2) == 1 * 1 + 2 * 2

testReplaceVertex :: Testsuite -> IO ()
testReplaceVertex (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "replaceVertex ============"
    test "replaceVertex x x            == id" $ \x y ->
          replaceVertex x x % y        == y

    test "replaceVertex x y (vertex x) == vertex y" $ \x y ->
          replaceVertex x y % vertex x == vertex y

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y z ->
          replaceVertex x y % z        == mergeVertices (== x) y z

testMergeVertices :: Testsuite -> IO ()
testMergeVertices (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \x y ->
          mergeVertices (const False) x % y == y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y z ->
          mergeVertices (== x) y % z       == replaceVertex x y z

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 % (0 * 2)   == 1 * 1

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 % (3 + 4 * 5) == 4 * 1

testTranspose :: Testsuite -> IO ()
testTranspose (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "transpose ============"
    test "transpose empty       == empty" $
          transpose % empty     == empty

    test "transpose (vertex x)  == vertex x" $ \x ->
          transpose % vertex x  == vertex x

    test "transpose (edge x y)  == edge y x" $ \x y ->
          transpose % edge x y  == edge y x

    test "transpose . transpose == id" $ \x ->
         (transpose . transpose) % x == x

    test "transpose . path      == path    . reverse" $ \xs ->
          transpose % path xs  == (path . reverse) xs

    test "transpose . circuit   == circuit . reverse" $ \xs ->
          transpose % circuit xs == (circuit . reverse) xs

    test "transpose . clique    == clique  . reverse" $ \xs ->
          transpose % clique xs == (clique . reverse) xs

    test "edgeList . transpose  == sort . map swap . edgeList" $ \x ->
          edgeList % transpose x == (sort . map swap . edgeList) x

testGmap :: Testsuite -> IO ()
testGmap (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f) ->
          gmap f % empty      == empty

    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f) x ->
          gmap f % vertex x == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f) x y ->
          gmap f % edge x y == edge (f x) (f y)

    test "gmap id           == id" $ \x ->
          gmap id % x       == x

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f) (apply -> g) x ->
         (gmap f . gmap g) x == gmap (f . g) % x

testInduce :: Testsuite -> IO ()
testInduce (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "induce ============"
    test "induce (const True)  x      == x" $ \x ->
          induce (const True) % x     == x

    test "induce (const False) x      == empty" $ \x ->
          induce (const False) % x    == empty

    test "induce (/= x)               == removeVertex x" $ \x y ->
          induce (/= x) % y           == removeVertex x y

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) y ->
         (induce p . induce q) % y    == induce (\x -> p x && q x) y

    test "isSubgraphOf (induce p x) x == True" $ \(apply -> p) x ->
          isSubgraphOf (induce p x) % x == True

testSplitVertex :: Testsuite -> IO ()
testSplitVertex (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "splitVertex ============"
    test "splitVertex x []                   == removeVertex x" $ \x y ->
          splitVertex x [] % y               == removeVertex x y

    test "splitVertex x [x]                  == id" $ \x y ->
          splitVertex x [x] % y              == y

    test "splitVertex x [y]                  == replaceVertex x y" $ \x y z ->
          splitVertex x [y] % z              == replaceVertex x y z

    test "splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)" $
          splitVertex 1 [0, 1] % (1 * (2 + 3)) == (0 + 1) * (2 + 3)

testBind :: Testsuite -> IO ()
testBind (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "bind ============"
    test "bind empty f         == empty" $ \(apply -> f) ->
          bind empty f         == id % empty

    test "bind (vertex x) f    == f x" $ \(apply -> f) x ->
          bind (vertex x) f    == id % f x

    test "bind (edge x y) f    == connect (f x) (f y)" $ \(apply -> f) x y ->
          bind (edge x y) f    == connect (f x) % f y

    test "bind (vertices xs) f == overlays (map f xs)" $ mapSize (min 10) $ \xs (apply -> f) ->
          bind (vertices xs) f == id % overlays (map f xs)

    test "bind x (const empty) == empty" $ \x ->
          bind x (const empty) == id % empty

    test "bind x vertex        == x" $ \x ->
          bind x vertex        == id % x

    test "bind (bind x f) g    == bind x (\\y -> bind (f y) g)" $ mapSize (min 10) $ \x (apply -> f) (apply -> g) ->
          bind (bind x f) g    == bind (id % x) (\y -> bind (f y) g)

testSimplify :: Testsuite -> IO ()
testSimplify (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "simplify ============"
    test "simplify              == id" $ \x ->
          simplify % x          == x

    test "size (simplify x)     <= size x" $ \x ->
          size % simplify x     <= size x


testDfsForest :: Testsuite -> IO ()
testDfsForest (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "dfsForest ============"
    test "forest (dfsForest $ edge 1 1)         == vertex 1" $
          forest (dfsForest % edge 1 1)         == id % vertex 1

    test "forest (dfsForest $ edge 1 2)         == edge 1 2" $
          forest (dfsForest % edge 1 2)         == id % edge 1 2

    test "forest (dfsForest $ edge 2 1)         == vertices [1, 2]" $
          forest (dfsForest % edge 2 1)         == id % vertices [1, 2]

    test "isSubgraphOf (forest $ dfsForest x) x == True" $ \x ->
          isSubgraphOf (forest $ dfsForest x) % x == True

    test "dfsForest . forest . dfsForest        == dfsForest" $ \x ->
          dfsForest % forest (dfsForest x)      == dfsForest % x

    test "dfsForest (vertices vs)               == map (\\v -> Node v []) (nub $ sort vs)" $ \vs ->
          dfsForest % vertices vs               == map (\v -> Node v []) (nub $ sort vs)

    test "dfsForest $ 3 * (1 + 4) * (1 + 5)     == <correct result>" $
          dfsForest % (3 * (1 + 4) * (1 + 5))   == [ Node { rootLabel = 1
                                                   , subForest = [ Node { rootLabel = 5
                                                                        , subForest = [] }]}
                                                   , Node { rootLabel = 3
                                                   , subForest = [ Node { rootLabel = 4
                                                                        , subForest = [] }]}]

testDfsForestFrom :: Testsuite -> IO ()
testDfsForestFrom (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "dfsForestFrom ============"
    test "forest (dfsForestFrom [1]    $ edge 1 1)     == vertex 1" $
          forest (dfsForestFrom [1]    % edge 1 1)     == id % vertex 1

    test "forest (dfsForestFrom [1]    $ edge 1 2)     == edge 1 2" $
          forest (dfsForestFrom [1]    % edge 1 2)     == id % edge 1 2

    test "forest (dfsForestFrom [2]    $ edge 1 2)     == vertex 2" $
          forest (dfsForestFrom [2]    % edge 1 2)     == id % vertex 2

    test "forest (dfsForestFrom [3]    $ edge 1 2)     == empty" $
          forest (dfsForestFrom [3]    % edge 1 2)     == id % empty

    test "forest (dfsForestFrom [2, 1] $ edge 1 2)     == vertices [1, 2]" $
          forest (dfsForestFrom [2, 1] % edge 1 2)     == id % vertices [1, 2]

    test "isSubgraphOf (forest $ dfsForestFrom vs x) x == True" $ \vs x ->
          isSubgraphOf (forest $ dfsForestFrom vs x) % x == True

    test "dfsForestFrom (vertexList x) x               == dfsForest x" $ \x ->
          dfsForestFrom (vertexList x) % x             == dfsForest % x

    test "dfsForestFrom vs             (vertices vs)   == map (\\v -> Node v []) (nub vs)" $ \vs ->
          dfsForestFrom vs           %  vertices vs    == map (\v -> Node v []) (nub vs)

    test "dfsForestFrom []             x               == []" $ \x ->
          dfsForestFrom []           % x               == []

    test "dfsForestFrom [1, 4] $ 3 * (1 + 4) * (1 + 5) == <correct result>" $
          dfsForestFrom [1, 4] % (3 * (1 + 4) * (1 + 5)) == [ Node { rootLabel = 1
                                                                   , subForest = [ Node { rootLabel = 5
                                                                                        , subForest = [] }]}
                                                            , Node { rootLabel = 4
                                                                   , subForest = [] }]

testDfs :: Testsuite -> IO ()
testDfs (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "dfs ============"
    test "dfs [1]    $ edge 1 1                == [1]" $
          dfs [1]    % edge 1 1                == [1]

    test "dfs [1]    $ edge 1 2                == [1, 2]" $
          dfs [1]    % edge 1 2                == [1, 2]

    test "dfs [2]    $ edge 1 2                == [2]" $
          dfs [2]    % edge 1 2                == [2]

    test "dfs [3]    $ edge 1 2                == []" $
          dfs [3]    % edge 1 2                == []

    test "dfs [1, 2] $ edge 1 2                == [1, 2]" $
          dfs [1, 2] % edge 1 2                == [1, 2]

    test "dfs [2, 1] $ edge 1 2                == [2, 1]" $
          dfs [2, 1] % edge 1 2                == [2, 1]

    test "dfs []     $ x                       == []" $ \x ->
          dfs []     % x                       == []

    test "dfs [1, 4] $ 3 * (1 + 4) * (1 + 5)   == [1, 5, 4]" $
          dfs [1, 4] % (3 * (1 + 4) * (1 + 5))   == [1, 5, 4]

    test "isSubgraphOf (vertices $ dfs vs x) x == True" $ \vs x ->
          isSubgraphOf (vertices $ dfs vs x) % x == True

testTopSort :: Testsuite -> IO ()
testTopSort (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "topSort ============"
    test "topSort (1 * 2 + 3 * 1)             == Just [3,1,2]" $
          topSort % (1 * 2 + 3 * 1)           == Just [3,1,2]

    test "topSort (1 * 2 + 2 * 1)             == Nothing" $
          topSort % (1 * 2 + 2 * 1)           == Nothing

    test "fmap (flip isTopSort x) (topSort x) /= Just False" $ \x ->
          fmap (flip isTopSort x) (topSort % x) /= Just False

testIsTopSort :: Testsuite -> IO ()
testIsTopSort (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "isTopSort ============"
    test "isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True" $
          isTopSort [3, 1, 2] % (1 * 2 + 3 * 1) == True

    test "isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False" $
          isTopSort [1, 2, 3] % (1 * 2 + 3 * 1) == False

    test "isTopSort []        (1 * 2 + 3 * 1) == False" $
          isTopSort []      % (1 * 2 + 3 * 1) == False

    test "isTopSort []        empty           == True" $
          isTopSort []      % empty           == True

    test "isTopSort [x]       (vertex x)      == True" $ \x ->
          isTopSort [x]      % vertex x       == True

    test "isTopSort [x]       (edge x x)      == False" $ \x ->
          isTopSort [x]      % edge x x       == False
