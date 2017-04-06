{-# LANGUAGE GADTs, RankNTypes #-}
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
    Testsuite, testsuite, testBasicPrimitives, testTranspose
  ) where

import Data.List (sort)
import Data.Tuple

import Algebra.Graph.Class (Graph (..))
import Algebra.Graph.Test
import Algebra.Graph.Test.API

import qualified Data.Set as Set

data Testsuite where
    Testsuite :: forall g. (Arbitrary g, Eq g, GraphAPI g, Num g, Show g, Vertex g ~ Int)
              => String -> (forall r. (g -> r) -> g -> r) -> Testsuite

testsuite :: forall g. (Arbitrary g, Eq g, GraphAPI g, Num g, Show g, Vertex g ~ Int)
          => String -> g -> Testsuite
testsuite prefix g = Testsuite prefix (\f x -> f (x `asTypeOf` g))

testBasicPrimitives :: Testsuite -> IO ()
testBasicPrimitives t = do
    testEmpty    t
    testVertex   t
    testEdge     t
    testOverlay  t
    testConnect  t
    testVertices t
    testEdges    t
    testOverlays t
    testConnects t
    testGraph    t

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
          isEmpty   % (vertex x) == False

    test "hasVertex x (vertex x) == True" $ \x ->
          hasVertex x % (vertex x) == True

    test "hasVertex 1 (vertex 2) == False" $
          hasVertex 1 % (vertex 2) == False

    test "vertexCount (vertex x) == 1" $ \x ->
          vertexCount % (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \x ->
          edgeCount % (vertex x) == 0

testEdge :: Testsuite -> IO ()
testEdge (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \x y ->
          edge x y               == connect (vertex x) % (vertex y)

    test "hasEdge x y (edge x y) == True" $ \x y ->
          hasEdge x y % (edge x y) == True

    test "edgeCount   (edge x y) == 1" $ \x y ->
          edgeCount % (edge x y) == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount % (edge 1 1) == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount % (edge 1 2) == 2

testOverlay :: Testsuite -> IO ()
testOverlay (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty   % (overlay x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z % (overlay x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (overlay x y) >= vertexCount x" $ \x y ->
          vertexCount % (overlay x y) >= vertexCount x

    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount % (overlay x y) <= vertexCount x + vertexCount y

    test "edgeCount   (overlay x y) >= edgeCount x" $ \x y ->
          edgeCount % (overlay x y) >= edgeCount x

    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \x y ->
          edgeCount % (overlay x y) <= edgeCount x   + edgeCount y

    test "vertexCount (overlay 1 2) == 2" $
          vertexCount % (overlay 1 2) == 2

    test "edgeCount   (overlay 1 2) == 0" $
          edgeCount % (overlay 1 2) == 0

testConnect :: Testsuite -> IO ()
testConnect (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "connect ============"
    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty   % (connect x y) == (isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z % (connect x y) == (hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x" $ \x y ->
          vertexCount % (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount % (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \x y ->
          edgeCount % (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \x y ->
          edgeCount % (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y" $ \x y ->
          edgeCount % (connect x y) >= vertexCount x * vertexCount y

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \x y ->
          edgeCount % (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount % (connect 1 2) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount % (connect 1 2) == 1

testVertices :: Testsuite -> IO ()
testVertices (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertices ============"
    test "vertices []            == empty" $
          vertices []            == id % empty

    test "vertices [x]           == vertex x" $ \x ->
          vertices [x]           == id % vertex x

    test "hasVertex x . vertices == elem x" $ \x xs ->
          hasVertex x % (vertices xs) == elem x xs

    test "vertexCount . vertices == length . nub" $ \xs ->
          vertexCount % (vertices xs) == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \xs ->
          vertexSet % (vertices xs) == Set.fromList xs

testEdges :: Testsuite -> IO ()
testEdges (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "edges ============"
    test "edges []          == empty" $
          edges []          == id % empty

    test "edges [(x,y)]     == edge x y" $ \x y ->
          edges [(x,y)]     == id % edge x y

    test "edgeCount . edges == length . nub" $ \xs ->
          edgeCount % (edges xs) == (length . nubOrd) xs

testOverlays :: Testsuite -> IO ()
testOverlays (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "overlays ============"
    test "overlays []        == empty" $
          overlays []        == id % empty

    test "overlays [x]       == x" $ \x ->
          overlays [x]       == id % x

    test "overlays [x,y]     == overlay x y" $ \x y ->
          overlays [x,y]     == id % overlay x y

    test "isEmpty . overlays == all isEmpty" $ \xs ->
          isEmpty % (overlays xs) == all isEmpty xs

testConnects :: Testsuite -> IO ()
testConnects (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "connects ============"
    test "connects []        == empty" $
          connects []        == id % empty

    test "connects [x]       == x" $ \x ->
          connects [x]       == id % x

    test "connects [x,y]     == connect x y" $ \x y ->
          connects [x,y]     == id % connect x y

    test "isEmpty . connects == all isEmpty" $ \xs ->
          isEmpty % (connects xs) == all isEmpty xs

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
          graph vs  es      == overlay (vertices vs) % (edges es)

testTranspose :: Testsuite -> IO ()
testTranspose (Testsuite prefix (%)) = do
    putStrLn $ "\n============ " ++ prefix ++ "transpose ============"
    test "transpose empty       == empty" $
          transpose % empty     == empty

    test "transpose (vertex x)  == vertex x" $ \x ->
          transpose % (vertex x) == vertex x

    test "transpose (edge x y)  == edge y x" $ \x y ->
          transpose % (edge x y) == edge y x

    test "transpose . transpose == id" $ \x ->
         (transpose . transpose) % x == x

    test "transpose . path      == path    . reverse" $ \xs ->
          transpose % (path xs) == (path . reverse) xs

    test "transpose . circuit   == circuit . reverse" $ \xs ->
          transpose % (circuit xs) == (circuit . reverse) xs

    test "transpose . clique    == clique  . reverse" $ \xs ->
          transpose % (clique xs) == (clique . reverse) xs

    test "edgeList . transpose  == sort . map swap . edgeList" $ \x ->
          edgeList % (transpose x) == (sort . map swap . edgeList) x
