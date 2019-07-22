{-# LANGUAGE RecordWildCards, GADTs, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Generic
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Generic graph API testing.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Generic where

import Control.Monad (when)
import Data.List (nub)
import Data.Maybe
import Data.Tree
import Data.Tuple

import Algebra.Graph.Test
import Algebra.Graph.Test.API

import qualified Algebra.Graph                        as G
import qualified Algebra.Graph.AdjacencyMap           as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import qualified Algebra.Graph.AdjacencyIntMap        as AIM
import qualified Data.Set                             as Set
import qualified Data.IntSet                          as IntSet

type ModulePrefix = String
type Testsuite g c = (ModulePrefix, API g c)
type TestsuiteInt g = (ModulePrefix, API g ((~) Int))

size10 :: Testable prop => prop -> Property
size10 = mapSize (min 10)

testBasicPrimitives :: TestsuiteInt g -> IO ()
testBasicPrimitives = mconcat [ testOrd
                              , testEmpty
                              , testVertex
                              , testEdge
                              , testOverlay
                              , testConnect
                              , testVertices
                              , testEdges
                              , testOverlays
                              , testConnects ]

testSymmetricBasicPrimitives :: TestsuiteInt g -> IO ()
testSymmetricBasicPrimitives = mconcat [ testSymmetricOrd
                                       , testEmpty
                                       , testVertex
                                       , testSymmetricEdge
                                       , testOverlay
                                       , testSymmetricConnect
                                       , testVertices
                                       , testSymmetricEdges
                                       , testOverlays
                                       , testSymmetricConnects ]

testToGraph :: TestsuiteInt g -> IO ()
testToGraph = mconcat [ testToGraphDefault
                      , testFoldg
                      , testIsEmpty
                      , testHasVertex
                      , testHasEdge
                      , testVertexCount
                      , testEdgeCount
                      , testVertexList
                      , testVertexSet
                      , testVertexIntSet
                      , testEdgeList
                      , testEdgeSet
                      , testAdjacencyList
                      , testPreSet
                      , testPreIntSet
                      , testPostSet
                      , testPostIntSet ]

testSymmetricToGraph :: TestsuiteInt g -> IO ()
testSymmetricToGraph = mconcat [ testSymmetricToGraphDefault
                               , testIsEmpty
                               , testHasVertex
                               , testSymmetricHasEdge
                               , testVertexCount
                               , testEdgeCount
                               , testVertexList
                               , testVertexSet
                               , testVertexIntSet
                               , testSymmetricEdgeList
                               , testSymmetricEdgeSet
                               , testSymmetricAdjacencyList
                               , testNeighbours ]

testRelational :: TestsuiteInt g -> IO ()
testRelational = mconcat [ testCompose
                         , testClosure
                         , testReflexiveClosure
                         , testSymmetricClosure
                         , testTransitiveClosure ]

testGraphFamilies :: TestsuiteInt g -> IO ()
testGraphFamilies = mconcat [ testPath
                            , testCircuit
                            , testClique
                            , testBiclique
                            , testStar
                            , testStars
                            , testTree
                            , testForest ]

testSymmetricGraphFamilies :: TestsuiteInt g -> IO ()
testSymmetricGraphFamilies = mconcat [ testSymmetricPath
                                     , testSymmetricCircuit
                                     , testSymmetricClique
                                     , testBiclique
                                     , testStar
                                     , testStars
                                     , testTree
                                     , testForest ]

testTransformations :: TestsuiteInt g -> IO ()
testTransformations = mconcat [ testRemoveVertex
                              , testRemoveEdge
                              , testReplaceVertex
                              , testMergeVertices
                              , testTranspose
                              , testGmap
                              , testInduce ]

testSymmetricTransformations :: TestsuiteInt g -> IO ()
testSymmetricTransformations = mconcat [ testRemoveVertex
                                       , testSymmetricRemoveEdge
                                       , testReplaceVertex
                                       , testMergeVertices
                                       , testGmap
                                       , testInduce ]

testConsistent :: TestsuiteInt g -> IO ()
testConsistent (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "consistent ============"
    test "Consistency of the Arbitrary instance" $ \x -> consistent x

    putStrLn ""
    test "consistent empty         == True" $
          consistent empty         == True

    test "consistent (vertex x)    == True" $ \x ->
          consistent (vertex x)    == True

    test "consistent (overlay x y) == True" $ \x y ->
          consistent (overlay x y) == True

    test "consistent (connect x y) == True" $ \x y ->
          consistent (connect x y) == True

    test "consistent (edge x y)    == True" $ \x y ->
          consistent (edge x y)    == True

    test "consistent (edges xs)    == True" $ \xs ->
          consistent (edges xs)    == True

    test "consistent (stars xs)    == True" $ \xs ->
          consistent (stars xs)    == True

testShow :: TestsuiteInt g -> IO ()
testShow (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "Show ============"
    test "show (empty    ) == \"empty\"" $
          show (empty    ) ==  "empty"

    test "show (1        ) == \"vertex 1\"" $
          show (1 `asTypeOf` empty) ==  "vertex 1"

    test "show (1 + 2    ) == \"vertices [1,2]\"" $
          show (1 + 2 `asTypeOf` empty) ==  "vertices [1,2]"

    test "show (1 * 2    ) == \"edge 1 2\"" $
          show (1 * 2 `asTypeOf` empty) ==  "edge 1 2"

    test "show (1 * 2 * 3) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 `asTypeOf` empty) == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3) == \"overlay (vertex 3) (edge 1 2)\"" $
          show (1 * 2 + 3 `asTypeOf` empty) == "overlay (vertex 3) (edge 1 2)"

    putStrLn ""
    test "show (vertex (-1)                            ) == \"vertex (-1)\"" $
          show (vertex (-1)                            ) == "vertex (-1)"

    test "show (vertex (-1) + vertex (-2)              ) == \"vertices [-2,-1]\"" $
          show (vertex (-1) + vertex (-2)              ) == "vertices [-2,-1]"

    test "show (vertex (-2) * vertex (-1)              ) == \"edge (-2) (-1)\"" $
          show (vertex (-2) * vertex (-1)              ) == "edge (-2) (-1)"

    test "show (vertex (-3) * vertex (-2) * vertex (-1)) == \"edges [(-3,-2),(-3,-1),(-2,-1)]\"" $
          show (vertex (-3) * vertex (-2) * vertex (-1)) == "edges [(-3,-2),(-3,-1),(-2,-1)]"

    test "show (vertex (-3) * vertex (-2) + vertex (-1)) == \"overlay (vertex (-1)) (edge (-3) (-2))\"" $
          show (vertex (-3) * vertex (-2) + vertex (-1)) == "overlay (vertex (-1)) (edge (-3) (-2))"

testSymmetricShow :: TestsuiteInt g -> IO ()
testSymmetricShow t@(_, API{..}) = do
    testShow t
    putStrLn ""
    test "show (2 * 1    ) == \"edge 1 2\"" $
          show (2 * 1 `asTypeOf` empty) ==  "edge 1 2"

    test "show (1 * 2 * 1) == \"edges [(1,1),(1,2)]\"" $
          show (1 * 2 * 1 `asTypeOf` empty) == "edges [(1,1),(1,2)]"

    test "show (3 * 2 * 1) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (3 * 2 * 1 `asTypeOf` empty) == "edges [(1,2),(1,3),(2,3)]"

testOrd :: TestsuiteInt g -> IO ()
testOrd (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "Ord ============"
    test "vertex 1 <  vertex 2" $
          vertex 1 <  vertex 2

    test "vertex 3 <  edge 1 2" $
          vertex 3 <  edge 1 2

    test "vertex 1 <  edge 1 1" $
          vertex 1 <  edge 1 1

    test "edge 1 1 <  edge 1 2" $
          edge 1 1 <  edge 1 2

    test "edge 1 2 <  edge 1 1 + edge 2 2" $
          edge 1 2 <  edge 1 1 + edge 2 2

    test "edge 1 2 <  edge 1 3" $
          edge 1 2 <  edge 1 3

    test "x        <= x + y" $ \x y ->
          x        <= x + (y `asTypeOf` empty)

    test "x + y    <= x * y" $ \x y ->
          x + y    <= x * (y `asTypeOf` empty)

testSymmetricOrd :: TestsuiteInt g -> IO ()
testSymmetricOrd (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "Ord ============"
    test "vertex 1 <  vertex 2" $
          vertex 1 <  vertex 2

    test "vertex 3 <  edge 1 2" $
          vertex 3 <  edge 1 2

    test "vertex 1 <  edge 1 1" $
          vertex 1 <  edge 1 1

    test "edge 1 1 <  edge 1 2" $
          edge 1 1 <  edge 1 2

    test "edge 1 2 <  edge 1 1 + edge 2 2" $
          edge 1 2 <  edge 1 1 + edge 2 2

    test "edge 2 1 <  edge 1 3" $
          edge 2 1 <  edge 1 3

    test "edge 1 2 == edge 2 1" $
          edge 1 2 == edge 2 1

    test "x        <= x + y" $ \x y ->
          x        <= x + (y `asTypeOf` empty)

    test "x + y    <= x * y" $ \x y ->
          x + y    <= x * (y `asTypeOf` empty)

testEmpty :: TestsuiteInt g -> IO ()
testEmpty (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "empty ============"
    test "isEmpty     empty == True" $
          isEmpty     empty == True

    test "hasVertex x empty == False" $ \x ->
          hasVertex x empty == False

    test "vertexCount empty == 0" $
          vertexCount empty == 0

    test "edgeCount   empty == 0" $
          edgeCount   empty == 0

testVertex :: TestsuiteInt g -> IO ()
testVertex (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertex ============"
    test "isEmpty     (vertex x) == False" $ \x ->
          isEmpty     (vertex x) == False

    test "hasVertex x (vertex x) == True" $ \x ->
          hasVertex x (vertex x) == True

    test "vertexCount (vertex x) == 1" $ \x ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \x ->
          edgeCount   (vertex x) == 0

testEdge :: TestsuiteInt g -> IO ()
testEdge (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edge ============"
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

testSymmetricEdge :: TestsuiteInt g -> IO ()
testSymmetricEdge (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edge ============"
    test "edge x y               == connect (vertex x) (vertex y)" $ \x y ->
          edge x y               == connect (vertex x) (vertex y)

    test "edge x y               == edge y x" $ \x y ->
          edge x y               == edge y x

    test "edge x y               == edges [(x,y), (y,x)]" $ \x y ->
          edge x y               == edges [(x,y), (y,x)]

    test "hasEdge x y (edge x y) == True" $ \x y ->
          hasEdge x y (edge x y) == True

    test "edgeCount   (edge x y) == 1" $ \x y ->
          edgeCount   (edge x y) == 1

    test "vertexCount (edge 1 1) == 1" $
          vertexCount (edge 1 1) == 1

    test "vertexCount (edge 1 2) == 2" $
          vertexCount (edge 1 2) == 2

testOverlay :: TestsuiteInt g -> IO ()
testOverlay (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty     (overlay x y) ==(isEmpty   x   && isEmpty   y)

    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z (overlay x y) ==(hasVertex z x || hasVertex z y)

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

testConnect :: TestsuiteInt g -> IO ()
testConnect (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "connect ============"
    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty     (connect x y) ==(isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z (connect x y) ==(hasVertex z x || hasVertex z y)

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

testSymmetricConnect :: TestsuiteInt g -> IO ()
testSymmetricConnect (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "connect ============"
    test "connect x y               == connect y x" $ \x y ->
          connect x y               == connect y x

    test "isEmpty     (connect x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty     (connect x y) ==(isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z (connect x y) ==(hasVertex z x || hasVertex z y)

    test "vertexCount (connect x y) >= vertexCount x" $ \x y ->
          vertexCount (connect x y) >= vertexCount x

    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount (connect x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect x y) >= edgeCount x" $ \x y ->
          edgeCount   (connect x y) >= edgeCount x

    test "edgeCount   (connect x y) >= edgeCount y" $ \x y ->
          edgeCount   (connect x y) >= edgeCount y

    test "edgeCount   (connect x y) >= vertexCount x * vertexCount y `div` 2" $ \x y ->
          edgeCount   (connect x y) >= vertexCount x * vertexCount y `div` 2

    test "edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \x y ->
          edgeCount   (connect x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect 1 2) == 2" $
          vertexCount (connect 1 2) == 2

    test "edgeCount   (connect 1 2) == 1" $
          edgeCount   (connect 1 2) == 1

testVertices :: TestsuiteInt g -> IO ()
testVertices (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertices ============"
    test "vertices []            == empty" $
          vertices []            == empty

    test "vertices [x]           == vertex x" $ \x ->
          vertices [x]           == vertex x

    test "hasVertex x . vertices == elem x" $ \x xs ->
         (hasVertex x . vertices) xs == elem x xs

    test "vertexCount . vertices == length . nub" $ \xs ->
         (vertexCount . vertices) xs == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \xs ->
         (vertexSet   . vertices) xs == Set.fromList xs

testEdges :: TestsuiteInt g -> IO ()
testEdges (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edges ============"
    test "edges []          == empty" $
          edges []          == empty

    test "edges [(x,y)]     == edge x y" $ \x y ->
          edges [(x,y)]     == edge x y

    test "edgeCount . edges == length . nub" $ \xs ->
         (edgeCount . edges) xs == (length . nubOrd) xs

testSymmetricEdges :: TestsuiteInt g -> IO ()
testSymmetricEdges (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edges ============"
    test "edges []             == empty" $
          edges []             == empty

    test "edges [(x,y)]        == edge x y" $ \x y ->
          edges [(x,y)]        == edge x y

    test "edges [(x,y), (y,x)] == edge x y" $ \x y ->
          edges [(x,y), (y,x)] == edge x y

testOverlays :: TestsuiteInt g -> IO ()
testOverlays (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "overlays ============"
    test "overlays []        == empty" $
          overlays []        == empty

    test "overlays [x]       == x" $ \x ->
          overlays [x]       == x

    test "overlays [x,y]     == overlay x y" $ \x y ->
          overlays [x,y]     == overlay x y

    test "overlays           == foldr overlay empty" $ size10 $ \xs ->
          overlays xs        == foldr overlay empty xs

    test "isEmpty . overlays == all isEmpty" $ size10 $ \xs ->
         (isEmpty . overlays) xs == all isEmpty xs

testConnects :: TestsuiteInt g -> IO ()
testConnects (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "connects ============"
    test "connects []        == empty" $
          connects []        == empty

    test "connects [x]       == x" $ \x ->
          connects [x]       == x

    test "connects [x,y]     == connect x y" $ \x y ->
          connects [x,y]     == connect x y

    test "connects           == foldr connect empty" $ size10 $ \xs ->
          connects xs        == foldr connect empty xs

    test "isEmpty . connects == all isEmpty" $ size10 $ \xs ->
         (isEmpty . connects) xs == all isEmpty xs

testSymmetricConnects :: TestsuiteInt g -> IO ()
testSymmetricConnects t@(_, API{..}) = do
    testConnects t
    test "connects           == connects . reverse" $ size10 $ \xs ->
          connects xs        == connects (reverse xs)

testStars :: TestsuiteInt g -> IO ()
testStars (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "stars ============"
    test "stars []                      == empty" $
          stars []                      == empty

    test "stars [(x, [])]               == vertex x" $ \x ->
          stars [(x, [])]               == vertex x

    test "stars [(x, [y])]              == edge x y" $ \x y ->
          stars [(x, [y])]              == edge x y

    test "stars [(x, ys)]               == star x ys" $ \x ys ->
          stars [(x, ys)]               == star x ys

    test "stars                         == overlays . map (uncurry star)" $ \xs ->
          stars xs                      == overlays (map (uncurry star) xs)

    test "stars . adjacencyList         == id" $ \x ->
         (stars . adjacencyList) x      == id x

    test "overlay (stars xs) (stars ys) == stars (xs ++ ys)" $ \xs ys ->
          overlay (stars xs) (stars ys) == stars (xs ++ ys)

testFromAdjacencySets :: TestsuiteInt g -> IO ()
testFromAdjacencySets (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "fromAdjacencySets ============"
    test "fromAdjacencySets []                                  == empty" $
          fromAdjacencySets []                                  == empty

    test "fromAdjacencySets [(x, Set.empty)]                    == vertex x" $ \x ->
          fromAdjacencySets [(x, Set.empty)]                    == vertex x

    test "fromAdjacencySets [(x, Set.singleton y)]              == edge x y" $ \x y ->
          fromAdjacencySets [(x, Set.singleton y)]              == edge x y

    test "fromAdjacencySets . map (fmap Set.fromList)           == stars" $ \x ->
         (fromAdjacencySets . map (fmap Set.fromList)) x        == stars x

    test "overlay (fromAdjacencySets xs) (fromAdjacencySets ys) == fromAdjacencySets (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencySets xs) (fromAdjacencySets ys) == fromAdjacencySets (xs ++ ys)

testFromAdjacencyIntSets :: TestsuiteInt g -> IO ()
testFromAdjacencyIntSets (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "fromAdjacencyIntSets ============"
    test "fromAdjacencyIntSets []                                     == empty" $
          fromAdjacencyIntSets []                                     == empty

    test "fromAdjacencyIntSets [(x, IntSet.empty)]                    == vertex x" $ \x ->
          fromAdjacencyIntSets [(x, IntSet.empty)]                    == vertex x

    test "fromAdjacencyIntSets [(x, IntSet.singleton y)]              == edge x y" $ \x y ->
          fromAdjacencyIntSets [(x, IntSet.singleton y)]              == edge x y

    test "fromAdjacencyIntSets . map (fmap IntSet.fromList)           == stars" $ \x ->
         (fromAdjacencyIntSets . map (fmap IntSet.fromList)) x        == stars x

    test "overlay (fromAdjacencyIntSets xs) (fromAdjacencyIntSets ys) == fromAdjacencyIntSets (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyIntSets xs) (fromAdjacencyIntSets ys) == fromAdjacencyIntSets (xs ++ ys)

testIsSubgraphOf :: TestsuiteInt g -> IO ()
testIsSubgraphOf (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "isSubgraphOf ============"
    test "isSubgraphOf empty         x             ==  True" $ \x ->
          isSubgraphOf empty         x             ==  True

    test "isSubgraphOf (vertex x)    empty         ==  False" $ \x ->
          isSubgraphOf (vertex x)    empty         ==  False

    test "isSubgraphOf x             (overlay x y) ==  True" $ \x y ->
          isSubgraphOf x             (overlay x y) ==  True

    test "isSubgraphOf (overlay x y) (connect x y) ==  True" $ \x y ->
          isSubgraphOf (overlay x y) (connect x y) ==  True

    test "isSubgraphOf (path xs)     (circuit xs)  ==  True" $ \xs ->
          isSubgraphOf (path xs)     (circuit xs)  ==  True

    test "isSubgraphOf x y                         ==> x <= y" $ \x z ->
        let y = x + z -- Make sure we hit the precondition
        in isSubgraphOf x y                        ==> x <= y

testSymmetricIsSubgraphOf :: TestsuiteInt g -> IO ()
testSymmetricIsSubgraphOf t@(_, API{..}) = do
    testIsSubgraphOf t
    test "isSubgraphOf (edge x y) (edge y x)       ==  True" $ \x y ->
          isSubgraphOf (edge x y) (edge y x)       ==  True

testToGraphDefault :: TestsuiteInt g -> IO ()
testToGraphDefault (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "toGraph et al. ============"
    test "toGraph                    == foldg Empty Vertex Overlay Connect" $ \x ->
          toGraph x                  == foldg G.Empty G.Vertex G.Overlay G.Connect x

    test "foldg                      == Algebra.Graph.foldg . toGraph" $ \e (apply -> v) (applyFun2 -> o) (applyFun2 -> c) x ->
          foldg e v o c x            == (G.foldg (e :: Int) v o c . toGraph) x

    test "isEmpty                    == foldg True (const False) (&&) (&&)" $ \x ->
          isEmpty x                  == foldg True (const False) (&&) (&&) x

    test "size                       == foldg 1 (const 1) (+) (+)" $ \x ->
          size x                     == foldg 1 (const 1) (+) (+) x

    test "hasVertex x                == foldg False (==x) (||) (||)" $ \x y ->
          hasVertex x y              == foldg False (==x) (||) (||) y

    test "hasEdge x y                == Algebra.Graph.hasEdge x y . toGraph" $ \x y z ->
          hasEdge x y z              == (G.hasEdge x y . toGraph) z

    test "vertexCount                == Set.size . vertexSet" $ \x ->
          vertexCount x              == (Set.size . vertexSet) x

    test "edgeCount                  == Set.size . edgeSet" $ \x ->
          edgeCount x                == (Set.size . edgeSet) x

    test "vertexList                 == Set.toAscList . vertexSet" $ \x ->
          vertexList x               == (Set.toAscList . vertexSet) x

    test "edgeList                   == Set.toAscList . edgeSet" $ \x ->
          edgeList x                 == (Set.toAscList . edgeSet) x

    test "vertexSet                  == foldg Set.empty Set.singleton Set.union Set.union" $ \x ->
          vertexSet x                == foldg Set.empty Set.singleton Set.union Set.union x

    test "vertexIntSet               == foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union" $ \x ->
          vertexIntSet x             == foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union x

    test "edgeSet                    == Algebra.Graph.AdjacencyMap.edgeSet . foldg empty vertex overlay connect" $ \x ->
          edgeSet x                  == (AM.edgeSet . foldg AM.empty AM.vertex AM.overlay AM.connect) x

    test "preSet x                   == Algebra.Graph.AdjacencyMap.preSet x . toAdjacencyMap" $ \x y ->
          preSet x y                 == (AM.preSet x . toAdjacencyMap) y

    test "preIntSet x                == Algebra.Graph.AdjacencyIntMap.preIntSet x . toAdjacencyIntMap" $ \x y ->
          preIntSet x y              == (AIM.preIntSet x . toAdjacencyIntMap) y

    test "postSet x                  == Algebra.Graph.AdjacencyMap.postSet x . toAdjacencyMap" $ \x y ->
          postSet x y                == (AM.postSet x . toAdjacencyMap) y

    test "postIntSet x               == Algebra.Graph.AdjacencyIntMap.postIntSet x . toAdjacencyIntMap" $ \x y ->
          postIntSet x y             == (AIM.postIntSet x . toAdjacencyIntMap) y

    test "adjacencyList              == Algebra.Graph.AdjacencyMap.adjacencyList . toAdjacencyMap" $ \x ->
          adjacencyList x            == (AM.adjacencyList . toAdjacencyMap) x

    test "adjacencyMap               == Algebra.Graph.AdjacencyMap.adjacencyMap . toAdjacencyMap" $ \x ->
          adjacencyMap x             == (AM.adjacencyMap . toAdjacencyMap) x

    test "adjacencyIntMap            == Algebra.Graph.AdjacencyIntMap.adjacencyIntMap . toAdjacencyIntMap" $ \x ->
          adjacencyIntMap x          == (AIM.adjacencyIntMap . toAdjacencyIntMap) x

    test "adjacencyMapTranspose      == Algebra.Graph.AdjacencyMap.adjacencyMap . toAdjacencyMapTranspose" $ \x ->
          adjacencyMapTranspose x    == (AM.adjacencyMap . toAdjacencyMapTranspose) x

    test "adjacencyIntMapTranspose   == Algebra.Graph.AdjacencyIntMap.adjacencyIntMap . toAdjacencyIntMapTranspose" $ \x ->
          adjacencyIntMapTranspose x == (AIM.adjacencyIntMap . toAdjacencyIntMapTranspose) x

    test "dfsForest                  == Algebra.Graph.AdjacencyMap.dfsForest . toAdjacencyMap" $ \x ->
          dfsForest x                == (AM.dfsForest . toAdjacencyMap) x

    test "dfsForestFrom vs           == Algebra.Graph.AdjacencyMap.dfsForestFrom vs . toAdjacencyMap" $ \vs x ->
          dfsForestFrom vs x         == (AM.dfsForestFrom vs . toAdjacencyMap) x

    test "dfs vs                     == Algebra.Graph.AdjacencyMap.dfs vs . toAdjacencyMap" $ \vs x ->
          dfs vs x                   == (AM.dfs vs . toAdjacencyMap) x

    test "reachable x                == Algebra.Graph.AdjacencyMap.reachable x . toAdjacencyMap" $ \x y ->
          reachable x y              == (AM.reachable x . toAdjacencyMap) y

    test "topSort                    == Algebra.Graph.AdjacencyMap.topSort . toAdjacencyMap" $ \x ->
          topSort x                  == (AM.topSort . toAdjacencyMap) x

    test "isAcyclic                  == Algebra.Graph.AdjacencyMap.isAcyclic . toAdjacencyMap" $ \x ->
          isAcyclic x                == (AM.isAcyclic . toAdjacencyMap) x

    test "isTopSortOf vs             == Algebra.Graph.AdjacencyMap.isTopSortOf vs . toAdjacencyMap" $ \vs x ->
          isTopSortOf vs x           == (AM.isTopSortOf vs . toAdjacencyMap) x

    test "toAdjacencyMap             == foldg empty vertex overlay connect" $ \x ->
          toAdjacencyMap x           == foldg AM.empty AM.vertex AM.overlay AM.connect x

    test "toAdjacencyMapTranspose    == foldg empty vertex overlay (flip connect)" $ \x ->
          toAdjacencyMapTranspose x  == foldg AM.empty AM.vertex AM.overlay (flip AM.connect) x

    test "toAdjacencyIntMap          == foldg empty vertex overlay connect" $ \x ->
          toAdjacencyIntMap x        == foldg AIM.empty AIM.vertex AIM.overlay AIM.connect x

    test "toAdjacencyIntMapTranspose == foldg empty vertex overlay (flip connect)" $ \x ->
          toAdjacencyIntMapTranspose x == foldg AIM.empty AIM.vertex AIM.overlay (flip AIM.connect) x

    test "isDfsForestOf f            == Algebra.Graph.AdjacencyMap.isDfsForestOf f . toAdjacencyMap" $ \f x ->
          isDfsForestOf f x          == (AM.isDfsForestOf f . toAdjacencyMap) x

    test "isTopSortOf vs             == Algebra.Graph.AdjacencyMap.isTopSortOf vs . toAdjacencyMap" $ \vs x ->
          isTopSortOf vs x           == (AM.isTopSortOf vs . toAdjacencyMap) x

-- TODO: We currently do not test 'edgeSet'.
testSymmetricToGraphDefault :: TestsuiteInt g -> IO ()
testSymmetricToGraphDefault (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "toGraph et al. ============"
    test "toGraph                    == foldg Empty Vertex Overlay Connect" $ \x ->
          toGraph x                  == foldg G.Empty G.Vertex G.Overlay G.Connect x

    test "foldg                      == Algebra.Graph.foldg . toGraph" $ \e (apply -> v) (applyFun2 -> o) (applyFun2 -> c) x ->
          foldg e v o c x            == (G.foldg (e :: Int) v o c . toGraph) x

    test "isEmpty                    == foldg True (const False) (&&) (&&)" $ \x ->
          isEmpty x                  == foldg True (const False) (&&) (&&) x

    test "size                       == foldg 1 (const 1) (+) (+)" $ \x ->
          size x                     == foldg 1 (const 1) (+) (+) x

    test "hasVertex x                == foldg False (==x) (||) (||)" $ \x y ->
          hasVertex x y              == foldg False (==x) (||) (||) y

    test "hasEdge x y                == Algebra.Graph.hasEdge x y . toGraph" $ \x y z ->
          hasEdge x y z              == (G.hasEdge x y . toGraph) z

    test "vertexCount                == Set.size . vertexSet" $ \x ->
          vertexCount x              == (Set.size . vertexSet) x

    test "edgeCount                  == Set.size . edgeSet" $ \x ->
          edgeCount x                == (Set.size . edgeSet) x

    test "vertexList                 == Set.toAscList . vertexSet" $ \x ->
          vertexList x               == (Set.toAscList . vertexSet) x

    test "edgeList                   == Set.toAscList . edgeSet" $ \x ->
          edgeList x                 == (Set.toAscList . edgeSet) x

    test "vertexSet                  == foldg Set.empty Set.singleton Set.union Set.union" $ \x ->
          vertexSet x                == foldg Set.empty Set.singleton Set.union Set.union x

    test "vertexIntSet               == foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union" $ \x ->
          vertexIntSet x             == foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union x

    test "adjacencyList              == Algebra.Graph.AdjacencyMap.adjacencyList . toAdjacencyMap" $ \x ->
          adjacencyList x            == (AM.adjacencyList . toAdjacencyMap) x

    test "adjacencyMap               == Algebra.Graph.AdjacencyMap.adjacencyMap . toAdjacencyMap" $ \x ->
          adjacencyMap x             == (AM.adjacencyMap . toAdjacencyMap) x

    test "adjacencyIntMap            == Algebra.Graph.AdjacencyIntMap.adjacencyIntMap . toAdjacencyIntMap" $ \x ->
          adjacencyIntMap x          == (AIM.adjacencyIntMap . toAdjacencyIntMap) x

    test "adjacencyMapTranspose      == Algebra.Graph.AdjacencyMap.adjacencyMap . toAdjacencyMapTranspose" $ \x ->
          adjacencyMapTranspose x    == (AM.adjacencyMap . toAdjacencyMapTranspose) x

    test "adjacencyIntMapTranspose   == Algebra.Graph.AdjacencyIntMap.adjacencyIntMap . toAdjacencyIntMapTranspose" $ \x ->
          adjacencyIntMapTranspose x == (AIM.adjacencyIntMap . toAdjacencyIntMapTranspose) x

    test "dfsForest                  == Algebra.Graph.AdjacencyMap.dfsForest . toAdjacencyMap" $ \x ->
          dfsForest x                == (AM.dfsForest . toAdjacencyMap) x

    test "dfsForestFrom vs           == Algebra.Graph.AdjacencyMap.dfsForestFrom vs . toAdjacencyMap" $ \vs x ->
          dfsForestFrom vs x         == (AM.dfsForestFrom vs . toAdjacencyMap) x

    test "dfs vs                     == Algebra.Graph.AdjacencyMap.dfs vs . toAdjacencyMap" $ \vs x ->
          dfs vs x                   == (AM.dfs vs . toAdjacencyMap) x

    test "reachable x                == Algebra.Graph.AdjacencyMap.reachable x . toAdjacencyMap" $ \x y ->
          reachable x y              == (AM.reachable x . toAdjacencyMap) y

    test "topSort                    == Algebra.Graph.AdjacencyMap.topSort . toAdjacencyMap" $ \x ->
          topSort x                  == (AM.topSort . toAdjacencyMap) x

    test "isAcyclic                  == Algebra.Graph.AdjacencyMap.isAcyclic . toAdjacencyMap" $ \x ->
          isAcyclic x                == (AM.isAcyclic . toAdjacencyMap) x

    test "isTopSortOf vs             == Algebra.Graph.AdjacencyMap.isTopSortOf vs . toAdjacencyMap" $ \vs x ->
          isTopSortOf vs x           == (AM.isTopSortOf vs . toAdjacencyMap) x

    test "toAdjacencyMap             == foldg empty vertex overlay connect" $ \x ->
          toAdjacencyMap x           == foldg AM.empty AM.vertex AM.overlay AM.connect x

    test "toAdjacencyMapTranspose    == foldg empty vertex overlay (flip connect)" $ \x ->
          toAdjacencyMapTranspose x  == foldg AM.empty AM.vertex AM.overlay (flip AM.connect) x

    test "toAdjacencyIntMap          == foldg empty vertex overlay connect" $ \x ->
          toAdjacencyIntMap x        == foldg AIM.empty AIM.vertex AIM.overlay AIM.connect x

    test "toAdjacencyIntMapTranspose == foldg empty vertex overlay (flip connect)" $ \x ->
          toAdjacencyIntMapTranspose x == foldg AIM.empty AIM.vertex AIM.overlay (flip AIM.connect) x

    test "isDfsForestOf f            == Algebra.Graph.AdjacencyMap.isDfsForestOf f . toAdjacencyMap" $ \f x ->
          isDfsForestOf f x          == (AM.isDfsForestOf f . toAdjacencyMap) x

    test "isTopSortOf vs             == Algebra.Graph.AdjacencyMap.isTopSortOf vs . toAdjacencyMap" $ \vs x ->
          isTopSortOf vs x           == (AM.isTopSortOf vs . toAdjacencyMap) x

testFoldg :: TestsuiteInt g -> IO ()
testFoldg (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "foldg ============"
    test "foldg empty vertex        overlay connect        == id" $ \x ->
          foldg empty vertex        overlay connect x      == id x

    test "foldg empty vertex        overlay (flip connect) == transpose" $ \x ->
          foldg empty vertex        overlay (flip connect) x == transpose x

    test "foldg 1     (const 1)     (+)     (+)            == size" $ \x ->
          foldg 1     (const 1)     (+)     (+) x          == size x

    test "foldg True  (const False) (&&)    (&&)           == isEmpty" $ \x ->
          foldg True  (const False) (&&)    (&&) x         == isEmpty x

testIsEmpty :: TestsuiteInt g -> IO ()
testIsEmpty (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "isEmpty ============"
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

testSize :: TestsuiteInt g -> IO ()
testSize (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "size ============"
    test "size empty         == 1" $
          size empty         == 1

    test "size (vertex x)    == 1" $ \x ->
          size (vertex x)    == 1

    test "size (overlay x y) == size x + size y" $ \x y ->
          size (overlay x y) == size x + size y

    test "size (connect x y) == size x + size y" $ \x y ->
          size (connect x y) == size x + size y

    test "size x             >= 1" $ \x ->
          size x             >= 1

    test "size x             >= vertexCount x" $ \x ->
          size x             >= vertexCount x

testHasVertex :: TestsuiteInt g -> IO ()
testHasVertex (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "hasVertex ============"
    test "hasVertex x empty            == False" $ \x ->
          hasVertex x empty            == False

    test "hasVertex x (vertex x)       == True" $ \x ->
          hasVertex x (vertex x)       == True

    test "hasVertex 1 (vertex 2)       == False" $
          hasVertex 1 (vertex 2)       == False

    test "hasVertex x . removeVertex x == const False" $ \x y ->
         (hasVertex x . removeVertex x) y == const False y

testHasEdge :: TestsuiteInt g -> IO ()
testHasEdge (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "hasEdge ============"
    test "hasEdge x y empty            == False" $ \x y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \x y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \x y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \x y z ->
         (hasEdge x y . removeEdge x y) z == const False z

    test "hasEdge x y                  == elem (x,y) . edgeList" $ \x y z -> do
        (u, v) <- elements ((x, y) : edgeList z)
        return $ hasEdge u v z == elem (u, v) (edgeList z)

testSymmetricHasEdge :: TestsuiteInt g -> IO ()
testSymmetricHasEdge (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "hasEdge ============"
    test "hasEdge x y empty            == False" $ \x y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \x y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True" $ \x y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y (edge y x)       == True" $ \x y ->
          hasEdge x y (edge y x)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \x y z ->
         (hasEdge x y . removeEdge x y) z == const False z

    test "hasEdge x y                  == elem (min x y, max x y) . edgeList" $ \x y z -> do
        (u, v) <- elements ((x, y) : edgeList z)
        return $ hasEdge u v z == elem (min u v, max u v) (edgeList z)

testVertexCount :: TestsuiteInt g -> IO ()
testVertexCount (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexCount ============"
    test "vertexCount empty             ==  0" $
          vertexCount empty             ==  0

    test "vertexCount (vertex x)        ==  1" $ \x ->
          vertexCount (vertex x)        ==  1

    test "vertexCount                   ==  length . vertexList" $ \x ->
          vertexCount x                 == (length . vertexList) x

    test "vertexCount x < vertexCount y ==> x < y" $ \x y ->
        if vertexCount x < vertexCount y
        then property (x < y)
        else (vertexCount x > vertexCount y ==> x > y)

testEdgeCount :: TestsuiteInt g -> IO ()
testEdgeCount (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount empty      == 0

    test "edgeCount (vertex x) == 0" $ \x ->
          edgeCount (vertex x) == 0

    test "edgeCount (edge x y) == 1" $ \x y ->
          edgeCount (edge x y) == 1

    test "edgeCount            == length . edgeList" $ \x ->
          edgeCount x          == (length . edgeList) x

testVertexList :: TestsuiteInt g -> IO ()
testVertexList (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexList ============"
    test "vertexList empty      == []" $
          vertexList empty      == []

    test "vertexList (vertex x) == [x]" $ \x ->
          vertexList (vertex x) == [x]

    test "vertexList . vertices == nub . sort" $ \xs ->
         (vertexList . vertices) xs == (nubOrd . sort) xs

testEdgeList :: TestsuiteInt g -> IO ()
testEdgeList (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeList ============"
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

testSymmetricEdgeList :: TestsuiteInt g -> IO ()
testSymmetricEdgeList (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeList ============"
    test "edgeList empty          == []" $
          edgeList empty          == []

    test "edgeList (vertex x)     == []" $ \x ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(min x y, max y x)]" $ \x y ->
          edgeList (edge x y)     == [(min x y, max y x)]

    test "edgeList (star 2 [3,1]) == [(1,2), (2,3)]" $
          edgeList (star 2 [3,1]) == [(1,2), (2,3)]

testAdjacencyList :: TestsuiteInt g -> IO ()
testAdjacencyList (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "adjacencyList ============"
    test "adjacencyList empty          == []" $
          adjacencyList empty          == []

    test "adjacencyList (vertex x)     == [(x, [])]" $ \x ->
          adjacencyList (vertex x)     == [(x, [])]

    test "adjacencyList (edge 1 2)     == [(1, [2]), (2, [])]" $
          adjacencyList (edge 1 2)     == [(1, [2]), (2, [])]

    test "adjacencyList (star 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]" $
          adjacencyList (star 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]

testSymmetricAdjacencyList :: TestsuiteInt g -> IO ()
testSymmetricAdjacencyList (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "adjacencyList ============"
    test "adjacencyList empty          == []" $
          adjacencyList empty          == []

    test "adjacencyList (vertex x)     == [(x, [])]" $ \x ->
          adjacencyList (vertex x)     == [(x, [])]

    test "adjacencyList (edge 1 2)     == [(1, [2]), (2, [1])]" $
          adjacencyList (edge 1 2)     == [(1, [2]), (2, [1])]

    test "adjacencyList (star 2 [3,1]) == [(1, [2]), (2, [1,3]), (3, [2])]" $
          adjacencyList (star 2 [3,1]) == [(1, [2]), (2, [1,3]), (3, [2])]

testVertexSet :: TestsuiteInt g -> IO ()
testVertexSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexSet ============"
    test "vertexSet empty      == Set.empty" $
          vertexSet empty      == Set.empty

    test "vertexSet . vertex   == Set.singleton" $ \x ->
         (vertexSet . vertex) x == Set.singleton x

    test "vertexSet . vertices == Set.fromList" $ \xs ->
         (vertexSet . vertices) xs == Set.fromList xs

testVertexIntSet :: TestsuiteInt g -> IO ()
testVertexIntSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "vertexIntSet ============"
    test "vertexIntSet empty      == IntSet.empty" $
          vertexIntSet empty      == IntSet.empty

    test "vertexIntSet . vertex   == IntSet.singleton" $ \x ->
         (vertexIntSet . vertex) x == IntSet.singleton x

    test "vertexIntSet . vertices == IntSet.fromList" $ \xs ->
         (vertexIntSet . vertices) xs == IntSet.fromList xs

    test "vertexIntSet . clique   == IntSet.fromList" $ \xs ->
         (vertexIntSet . clique) xs == IntSet.fromList xs

testEdgeSet :: TestsuiteInt g -> IO ()
testEdgeSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet empty      == Set.empty

    test "edgeSet (vertex x) == Set.empty" $ \x ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \x y ->
          edgeSet (edge x y) == Set.singleton (x,y)

    test "edgeSet . edges    == Set.fromList" $ \xs ->
         (edgeSet . edges) xs == Set.fromList xs

testSymmetricEdgeSet :: TestsuiteInt g -> IO ()
testSymmetricEdgeSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet empty      == Set.empty

    test "edgeSet (vertex x) == Set.empty" $ \x ->
          edgeSet (vertex x) == Set.empty

    test "edgeSet (edge x y) == Set.singleton (min x y, max x y)" $ \x y ->
          edgeSet (edge x y) == Set.singleton (min x y, max x y)

testPreSet :: TestsuiteInt g -> IO ()
testPreSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "preSet ============"
    test "preSet x empty      == Set.empty" $ \x ->
          preSet x empty      == Set.empty

    test "preSet x (vertex x) == Set.empty" $ \x ->
          preSet x (vertex x) == Set.empty

    test "preSet 1 (edge 1 2) == Set.empty" $
          preSet 1 (edge 1 2) == Set.empty

    test "preSet y (edge x y) == Set.fromList [x]" $ \x y ->
          preSet y (edge x y) == Set.fromList [x]

testPostSet :: TestsuiteInt g -> IO ()
testPostSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "postSet ============"
    test "postSet x empty      == Set.empty" $ \x ->
          postSet x empty      == Set.empty

    test "postSet x (vertex x) == Set.empty" $ \x ->
          postSet x (vertex x) == Set.empty

    test "postSet x (edge x y) == Set.fromList [y]" $ \x y ->
          postSet x (edge x y) == Set.fromList [y]

    test "postSet 2 (edge 1 2) == Set.empty" $
          postSet 2 (edge 1 2) == Set.empty

testPreIntSet :: TestsuiteInt g -> IO ()
testPreIntSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "preIntSet ============"
    test "preIntSet x empty      == IntSet.empty" $ \x ->
          preIntSet x empty      == IntSet.empty

    test "preIntSet x (vertex x) == IntSet.empty" $ \x ->
          preIntSet x (vertex x) == IntSet.empty

    test "preIntSet 1 (edge 1 2) == IntSet.empty" $
          preIntSet 1 (edge 1 2) == IntSet.empty

    test "preIntSet y (edge x y) == IntSet.fromList [x]" $ \x y ->
          preIntSet y (edge x y) == IntSet.fromList [x]

testPostIntSet :: TestsuiteInt g -> IO ()
testPostIntSet (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "postIntSet ============"
    test "postIntSet x empty      == IntSet.empty" $ \x ->
          postIntSet x empty      == IntSet.empty

    test "postIntSet x (vertex x) == IntSet.empty" $ \x ->
          postIntSet x (vertex x) == IntSet.empty

    test "postIntSet 2 (edge 1 2) == IntSet.empty" $
          postIntSet 2 (edge 1 2) == IntSet.empty

    test "postIntSet x (edge x y) == IntSet.fromList [y]" $ \x y ->
          postIntSet x (edge x y) == IntSet.fromList [y]

testNeighbours :: TestsuiteInt g -> IO ()
testNeighbours (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "neighbours ============"
    test "neighbours x empty      == Set.empty" $ \x ->
          neighbours x empty      == Set.empty

    test "neighbours x (vertex x) == Set.empty" $ \x ->
          neighbours x (vertex x) == Set.empty

    test "neighbours x (edge x y) == Set.fromList [y]" $ \x y ->
          neighbours x (edge x y) == Set.fromList [y]

    test "neighbours y (edge x y) == Set.fromList [x]" $ \x y ->
          neighbours y (edge x y) == Set.fromList [x]

testPath :: TestsuiteInt g -> IO ()
testPath (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "path ============"
    test "path []    == empty" $
          path []    == empty

    test "path [x]   == vertex x" $ \x ->
          path [x]   == vertex x

    test "path [x,y] == edge x y" $ \x y ->
          path [x,y] == edge x y

testSymmetricPath :: TestsuiteInt g -> IO ()
testSymmetricPath t@(_, API{..}) = do
    testPath t
    test "path       == path . reverse" $ \xs ->
          path xs    ==(path . reverse) xs

testCircuit :: TestsuiteInt g -> IO ()
testCircuit (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "circuit ============"
    test "circuit []    == empty" $
          circuit []    == empty

    test "circuit [x]   == edge x x" $ \x ->
          circuit [x]   == edge x x

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \x y ->
          circuit [x,y] == edges [(x,y), (y,x)]

testSymmetricCircuit :: TestsuiteInt g -> IO ()
testSymmetricCircuit t@(_, API{..}) = do
    testCircuit t
    test "circuit       == circuit . reverse" $ \xs ->
          circuit xs    ==(circuit . reverse) xs

testClique :: TestsuiteInt g -> IO ()
testClique (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "clique ============"
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

testSymmetricClique :: TestsuiteInt g -> IO ()
testSymmetricClique t@(_, API{..}) = do
    testClique t
    test "clique            == clique . reverse" $ \xs->
          clique xs         ==(clique . reverse) xs

testBiclique :: TestsuiteInt g -> IO ()
testBiclique (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "biclique ============"
    test "biclique []      []      == empty" $
          biclique []      []      == empty

    test "biclique [x]     []      == vertex x" $ \x ->
          biclique [x]     []      == vertex x

    test "biclique []      [y]     == vertex y" $ \y ->
          biclique []      [y]     == vertex y

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \x1 x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]

    test "biclique xs      ys      == connect (vertices xs) (vertices ys)" $ \xs ys ->
          biclique xs      ys      == connect (vertices xs) (vertices ys)

testStar :: TestsuiteInt g -> IO ()
testStar (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "star ============"
    test "star x []    == vertex x" $ \x ->
          star x []    == vertex x

    test "star x [y]   == edge x y" $ \x y ->
          star x [y]   == edge x y

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \x y z ->
          star x [y,z] == edges [(x,y), (x,z)]

    test "star x ys    == connect (vertex x) (vertices ys)" $ \x ys ->
          star x ys    == connect (vertex x) (vertices ys)

testTree :: TestsuiteInt g -> IO ()
testTree (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "tree ============"
    test "tree (Node x [])                                         == vertex x" $ \x ->
          tree (Node x [])                                         == vertex x

    test "tree (Node x [Node y [Node z []]])                       == path [x,y,z]" $ \x y z ->
          tree (Node x [Node y [Node z []]])                       == path [x,y,z]

    test "tree (Node x [Node y [], Node z []])                     == star x [y,z]" $ \x y z ->
          tree (Node x [Node y [], Node z []])                     == star x [y,z]

    test "tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]" $
          tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == edges [(1,2), (1,3), (3,4), (3,5)]

testForest :: TestsuiteInt g -> IO ()
testForest (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "forest ============"
    test "forest []                                                  == empty" $
          forest []                                                  == empty

    test "forest [x]                                                 == tree x" $ \x ->
          forest [x]                                                 == tree x

    test "forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]" $
          forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == edges [(1,2), (1,3), (4,5)]

    test "forest                                                     == overlays . map tree" $ \x ->
          forest x                                                   ==(overlays . map tree) x

testMesh :: Testsuite g Ord -> IO ()
testMesh (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "mesh ============"
    test "mesh xs     []    == empty" $ \(xs :: [Int]) ->
          mesh xs ([] :: [Int]) == empty

    test "mesh []     ys    == empty" $ \(ys :: [Int]) ->
          mesh ([] :: [Int]) ys == empty

    test "mesh [x]    [y]   == vertex (x, y)" $ \(x :: Int) (y :: Int) ->
          mesh [x]    [y]   == vertex (x, y)

    test "mesh xs     ys    == box (path xs) (path ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          mesh xs     ys    == box (path xs) (path ys)

    test "mesh [1..3] \"ab\"  == <correct result>" $
          mesh [1..3]  "ab"   == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(2,'b')), ((2,'a'),(2,'b'))
                                       , ((2,'a'),(3,'a')), ((2,'b'),(3,'b')), ((3,'a'),(3 :: Int,'b')) ]

    test "size (mesh xs ys) == max 1 (3 * length xs * length ys - length xs - length ys -1)" $ \(xs :: [Int]) (ys :: [Int]) ->
          size (mesh xs ys) == max 1 (3 * length xs * length ys - length xs - length ys -1)

testTorus :: Testsuite g Ord -> IO ()
testTorus (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "torus ============"
    test "torus xs     []    == empty" $ \(xs :: [Int]) ->
          torus xs ([] :: [Int]) == empty

    test "torus []     ys    == empty" $ \(ys :: [Int]) ->
          torus ([] :: [Int]) ys == empty

    test "torus [x]    [y]   == edge (x,y) (x,y)" $ \(x :: Int) (y :: Int) ->
          torus [x]    [y]   == edge (x,y) (x,y)

    test "torus xs     ys    == box (circuit xs) (circuit ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          torus xs     ys    == box (circuit xs) (circuit ys)

    test "torus [1,2]  \"ab\"  == <correct result>" $
          torus [1,2]   "ab"   == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(1,'a')), ((1,'b'),(2,'b'))
                                        , ((2,'a'),(1,'a')), ((2,'a'),(2,'b')), ((2,'b'),(1,'b')), ((2,'b'),(2 :: Int,'a')) ]

    test "size (torus xs ys) == max 1 (3 * length xs * length ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          size (torus xs ys) == max 1 (3 * length xs * length ys)

testDeBruijn :: Testsuite g Ord -> IO ()
testDeBruijn (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "deBruijn ============"
    test "          deBruijn 0 xs               == edge [] []" $ \(xs :: [Int]) ->
                    deBruijn 0 xs               == edge [] []

    test "n > 0 ==> deBruijn n []               == empty" $ \n ->
          n > 0 ==> deBruijn n ([] :: [Int])    == empty

    test "          deBruijn 1 [0,1]            == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]" $
                    deBruijn 1 [0,1::Int]       == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]

    test "          deBruijn 2 \"0\"              == edge \"00\" \"00\"" $
                    deBruijn 2  "0"               == edge "00" "00"

    test "          deBruijn 2 \"01\"             == <correct result>" $
                    deBruijn 2  "01"              == edges [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
                                                           , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]

    test "          transpose   (deBruijn n xs) == gmap reverse $ deBruijn n xs" $ mapSize (min 5) $ \(NonNegative n) (xs :: [Int]) ->
                    transpose   (deBruijn n xs) == gmap reverse (deBruijn n xs)

    test "          vertexCount (deBruijn n xs) == (length $ nub xs)^n" $ mapSize (min 5) $ \(NonNegative n) (xs :: [Int]) ->
                    vertexCount (deBruijn n xs) == (length $ nubOrd xs)^n

    test "n > 0 ==> edgeCount   (deBruijn n xs) == (length $ nub xs)^(n + 1)" $ mapSize (min 5) $ \(NonNegative n) (xs :: [Int]) ->
          n > 0 ==> edgeCount   (deBruijn n xs) == (length $ nubOrd xs)^(n + 1)

testBox :: Testsuite g Ord -> IO ()
testBox (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "box ============"
    let unit = gmap $ \(a :: Int, ()      ) -> a
        comm = gmap $ \(a :: Int, b :: Int) -> (b, a)
    test "box x y               ~~ box y x" $ mapSize (min 10) $ \x y ->
          comm (box x y)        == box y x

    test "box x (overlay y z)   == overlay (box x y) (box x z)" $ mapSize (min 10) $ \x y z ->
        let _ = x + y + z + vertex (0 :: Int) in
          box x (overlay y z)   == overlay (box x y) (box x z)

    test "box x (vertex ())     ~~ x" $ mapSize (min 10) $ \x ->
     unit(box x (vertex ()))    == (x `asTypeOf` empty)

    test "box x empty           ~~ empty" $ mapSize (min 10) $ \x ->
     unit(box x empty)          == empty

    let assoc = gmap $ \(a :: Int, (b :: Int, c :: Int)) -> ((a, b), c)
    test "box x (box y z)       ~~ box (box x y) z" $ mapSize (min 10) $ \x y z ->
      assoc (box x (box y z))   == box (box x y) z

    test "transpose   (box x y) == box (transpose x) (transpose y)" $ mapSize (min 10) $ \x y ->
        let _ = x + y + vertex (0 :: Int) in
          transpose   (box x y) == box (transpose x) (transpose y)

    test "vertexCount (box x y) == vertexCount x * vertexCount y" $ mapSize (min 10) $ \x y ->
        let _ = x + y + vertex (0 :: Int) in
          vertexCount (box x y) == vertexCount x * vertexCount y

    test "edgeCount   (box x y) <= vertexCount x * edgeCount y + edgeCount x * vertexCount y" $ mapSize (min 10) $ \x y ->
        let _ = x + y + vertex (0 :: Int) in
          edgeCount   (box x y) <= vertexCount x * edgeCount y + edgeCount x * vertexCount y

testRemoveVertex :: TestsuiteInt g -> IO ()
testRemoveVertex (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \x ->
          removeVertex x (vertex x)       == empty

    test "removeVertex 1 (vertex 2)       == vertex 2" $
          removeVertex 1 (vertex 2)       == vertex 2

    test "removeVertex x (edge x x)       == empty" $ \x ->
          removeVertex x (edge x x)       == empty

    test "removeVertex 1 (edge 1 2)       == vertex 2" $
          removeVertex 1 (edge 1 2)       == vertex 2

    test "removeVertex x . removeVertex x == removeVertex x" $ \x y ->
         (removeVertex x . removeVertex x) y == removeVertex x y

testRemoveEdge :: TestsuiteInt g -> IO ()
testRemoveEdge (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "removeEdge ============"
    test "removeEdge x y (edge x y)       == vertices [x,y]" $ \x y ->
          removeEdge x y (edge x y)       == vertices [x,y]

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \x y z ->
         (removeEdge x y . removeEdge x y) z == removeEdge x y z

    test "removeEdge x y . removeVertex x == removeVertex x" $ \x y z ->
         (removeEdge x y . removeVertex x) z == removeVertex x z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2

    -- TODO: Ouch. Generic tests are becoming awkward. We need a better way.
    when (prefix == "Fold." || prefix == "Graph.") $ do
        test "size (removeEdge x y z)         <= 3 * size z" $ \x y z ->
              size (removeEdge x y z)         <= 3 * size z

testSymmetricRemoveEdge :: TestsuiteInt g -> IO ()
testSymmetricRemoveEdge t@(_, API{..}) = do
    testRemoveEdge t
    test "removeEdge x y                  == removeEdge y x" $ \x y z ->
          removeEdge x y z                == removeEdge y x z

testReplaceVertex :: TestsuiteInt g -> IO ()
testReplaceVertex (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "replaceVertex ============"
    test "replaceVertex x x            == id" $ \x y ->
          replaceVertex x x y          == id y

    test "replaceVertex x y (vertex x) == vertex y" $ \x y ->
          replaceVertex x y (vertex x) == vertex y

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y z ->
          replaceVertex x y z          == mergeVertices (== x) y z

testMergeVertices :: TestsuiteInt g -> IO ()
testMergeVertices (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "mergeVertices ============"
    test "mergeVertices (const False) x    == id" $ \x y ->
          mergeVertices (const False) x y  == id y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y z ->
          mergeVertices (== x) y z         == replaceVertex x y z

    test "mergeVertices even 1 (0 * 2)     == 1 * 1" $
          mergeVertices even 1 (0 * 2)     == 1 * 1

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1" $
          mergeVertices odd  1 (3 + 4 * 5) == 4 * 1

testTranspose :: TestsuiteInt g -> IO ()
testTranspose (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "transpose ============"
    test "transpose empty       == empty" $
          transpose empty       == empty

    test "transpose (vertex x)  == vertex x" $ \x ->
          transpose (vertex x)  == vertex x

    test "transpose (edge x y)  == edge y x" $ \x y ->
          transpose (edge x y)  == edge y x

    test "transpose . transpose == id" $ size10 $ \x ->
         (transpose . transpose) x == id x

    test "edgeList . transpose  == sort . map swap . edgeList" $ \x ->
         (edgeList . transpose) x == (sort . map swap . edgeList) x

testGmap :: TestsuiteInt g -> IO ()
testGmap (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f) ->
          gmap f empty      == empty

    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f) x ->
          gmap f (vertex x) == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f) x y ->
          gmap f (edge x y) == edge (f x) (f y)

    test "gmap id           == id" $ \x ->
          gmap id x         == id x

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f :: Int -> Int) (apply -> g :: Int -> Int) x ->
         (gmap f . gmap g) x == gmap (f . g) x

testInduce :: TestsuiteInt g -> IO ()
testInduce (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "induce ============"
    test "induce (const True ) x      == x" $ \x ->
          induce (const True ) x      == x

    test "induce (const False) x      == empty" $ \x ->
          induce (const False) x      == empty

    test "induce (/= x)               == removeVertex x" $ \x y ->
          induce (/= x) y             == removeVertex x y

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) y ->
         (induce p . induce q) y      == induce (\x -> p x && q x) y

    test "isSubgraphOf (induce p x) x == True" $ \(apply -> p) x ->
          isSubgraphOf (induce p x) x == True

testInduceJust :: Testsuite g Ord -> IO ()
testInduceJust (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "induceJust ============"
    test "induceJust (vertex Nothing)                               == empty" $
          induceJust (vertex (Nothing :: Maybe Int))                == empty

    test "induceJust (edge (Just x) Nothing)                        == vertex x" $ \x ->
          induceJust (edge (Just x) (Nothing :: Maybe Int))         == vertex x

    test "induceJust . gmap Just                                    == id" $ \(x :: g Int) ->
         (induceJust . gmap Just) x                                 == id x

    test "induceJust . gmap (\\x -> if p x then Just x else Nothing) == induce p" $ \(x :: g Int) (apply -> p) ->
         (induceJust . gmap (\x -> if p x then Just x else Nothing)) x == induce p x

testCompose :: TestsuiteInt g -> IO ()
testCompose (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "compose ============"
    test "compose empty            x                == empty" $ \x ->
          compose empty            x                == empty

    test "compose x                empty            == empty" $ \x ->
          compose x                empty            == empty

    test "compose (vertex x)       y                == empty" $ \x y ->
          compose (vertex x)       y                == empty

    test "compose x                (vertex y)       == empty" $ \x y ->
          compose x                (vertex y)       == empty

    test "compose x                (compose y z)    == compose (compose x y) z" $ size10 $ \x y z ->
          compose x                (compose y z)    == compose (compose x y) z

    test "compose x                (overlay y z)    == overlay (compose x y) (compose x z)" $ size10 $ \x y z ->
          compose x                (overlay y z)    == overlay (compose x y) (compose x z)

    test "compose (overlay x y) z                   == overlay (compose x z) (compose y z)" $ size10 $ \x y z ->
          compose (overlay x y) z                   == overlay (compose x z) (compose y z)

    test "compose (edge x y)       (edge y z)       == edge x z" $ \x y z ->
          compose (edge x y)       (edge y z)       == edge x z

    test "compose (path    [1..5]) (path    [1..5]) == edges [(1,3),(2,4),(3,5)]" $
          compose (path    [1..5]) (path    [1..5]) == edges [(1,3),(2,4),(3,5)]

    test "compose (circuit [1..5]) (circuit [1..5]) == circuit [1,3,5,2,4]" $
          compose (circuit [1..5]) (circuit [1..5]) == circuit [1,3,5,2,4]

testClosure :: TestsuiteInt g -> IO ()
testClosure (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "closure ============"
    test "closure empty           == empty" $
          closure empty           == empty

    test "closure (vertex x)      == edge x x" $ \x ->
          closure (vertex x)      == edge x x

    test "closure (edge x x)      == edge x x" $ \x ->
          closure (edge x x)      == edge x x

    test "closure (edge x y)      == edges [(x,x), (x,y), (y,y)]" $ \x y ->
          closure (edge x y)      == edges [(x,x), (x,y), (y,y)]

    test "closure (path $ nub xs) == reflexiveClosure (clique $ nub xs)" $ \xs ->
          closure (path $ nubOrd xs) == reflexiveClosure (clique $ nubOrd xs)

    test "closure                 == reflexiveClosure . transitiveClosure" $ size10 $ \x ->
          closure x               == (reflexiveClosure . transitiveClosure) x

    test "closure                 == transitiveClosure . reflexiveClosure" $ size10 $ \x ->
          closure x               == (transitiveClosure . reflexiveClosure) x

    test "closure . closure       == closure" $ size10 $ \x ->
         (closure . closure) x    == closure x

    test "postSet x (closure y)   == Set.fromList (reachable x y)" $ size10 $ \x y ->
          postSet x (closure y)   == Set.fromList (reachable x y)

testReflexiveClosure :: TestsuiteInt g -> IO ()
testReflexiveClosure (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "reflexiveClosure ============"
    test "reflexiveClosure empty              == empty" $
          reflexiveClosure empty              == empty

    test "reflexiveClosure (vertex x)         == edge x x" $ \x ->
          reflexiveClosure (vertex x)         == edge x x

    test "reflexiveClosure (edge x x)         == edge x x" $ \x ->
          reflexiveClosure (edge x x)         == edge x x

    test "reflexiveClosure (edge x y)         == edges [(x,x), (x,y), (y,y)]" $ \x y ->
          reflexiveClosure (edge x y)         == edges [(x,x), (x,y), (y,y)]

    test "reflexiveClosure . reflexiveClosure == reflexiveClosure" $ \x ->
         (reflexiveClosure . reflexiveClosure) x == reflexiveClosure x

testSymmetricClosure :: TestsuiteInt g -> IO ()
testSymmetricClosure (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "symmetricClosure ============"
    test "symmetricClosure empty              == empty" $
          symmetricClosure empty              == empty

    test "symmetricClosure (vertex x)         == vertex x" $ \x ->
          symmetricClosure (vertex x)         == vertex x

    test "symmetricClosure (edge x y)         == edges [(x,y), (y,x)]" $ \x y ->
          symmetricClosure (edge x y)         == edges [(x,y), (y,x)]

    test "symmetricClosure x                  == overlay x (transpose x)" $ \x ->
          symmetricClosure x                  == overlay x (transpose x)

    test "symmetricClosure . symmetricClosure == symmetricClosure" $ \x ->
         (symmetricClosure . symmetricClosure) x == symmetricClosure x

testTransitiveClosure :: TestsuiteInt g -> IO ()
testTransitiveClosure (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "transitiveClosure ============"
    test "transitiveClosure empty               == empty" $
          transitiveClosure empty               == empty

    test "transitiveClosure (vertex x)          == vertex x" $ \x ->
          transitiveClosure (vertex x)          == vertex x

    test "transitiveClosure (edge x y)          == edge x y" $ \x y ->
          transitiveClosure (edge x y)          == edge x y

    test "transitiveClosure (path $ nub xs)     == clique (nub $ xs)" $ \xs ->
          transitiveClosure (path $ nubOrd xs)  == clique (nubOrd xs)

    test "transitiveClosure . transitiveClosure == transitiveClosure" $ size10 $ \x ->
         (transitiveClosure . transitiveClosure) x == transitiveClosure x

testSplitVertex :: TestsuiteInt g -> IO ()
testSplitVertex (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "splitVertex ============"
    test "splitVertex x []                   == removeVertex x" $ \x y ->
          splitVertex x [] y                 == removeVertex x y

    test "splitVertex x [x]                  == id" $ \x y ->
          splitVertex x [x] y                == id y

    test "splitVertex x [y]                  == replaceVertex x y" $ \x y z ->
          splitVertex x [y] z                == replaceVertex x y z

    test "splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)" $
          splitVertex 1 [0, 1] (1 * (2 + 3)) == (0 + 1) * (2 + 3)

testBind :: TestsuiteInt g -> IO ()
testBind (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "bind ============"
    test "bind empty f         == empty" $ \(apply -> f) ->
          bind empty f         == empty

    test "bind (vertex x) f    == f x" $ \(apply -> f) x ->
          bind (vertex x) f    == f x

    test "bind (edge x y) f    == connect (f x) (f y)" $ \(apply -> f) x y ->
          bind (edge x y) f    == connect (f x) (f y)

    test "bind (vertices xs) f == overlays (map f xs)" $ size10 $ \xs (apply -> f) ->
          bind (vertices xs) f == overlays (map f xs)

    test "bind x (const empty) == empty" $ \x ->
          bind x (const empty) == empty

    test "bind x vertex        == x" $ \x ->
          bind x vertex        == x

    test "bind (bind x f) g    == bind x (\\y -> bind (f y) g)" $ size10 $ \x (apply -> f) (apply -> g) ->
          bind (bind x f) g    == bind x (\y  -> bind (f y) g)

testSimplify :: TestsuiteInt g -> IO ()
testSimplify (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "simplify ============"
    test "simplify              == id" $ \x ->
          simplify x            == id x

    test "size (simplify x)     <= size x" $ \x ->
          size (simplify x)     <= size x

testBfsForest :: TestsuiteInt g -> IO ()
testBfsForest (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "bfsForest ============"
    test "bfsForest empty                       == []" $
          bfsForest empty                       == []

    test "forest (bfsForest $ edge 1 1)         == vertex 1" $
          forest (bfsForest $ edge 1 1)         == vertex 1

    test "forest (bfsForest $ edge 1 2)         == edge 1 2" $
          forest (bfsForest $ edge 1 2)         == edge 1 2

    test "forest (bfsForest $ edge 2 1)         == vertices [1,2]" $
          forest (bfsForest $ edge 2 1)         == vertices [1,2]

    test "isSubgraphOf (forest $ bfsForest x) x == True" $ \x ->
          isSubgraphOf (forest $ bfsForest x) x == True


    test "bfsForest . forest . bfsForest        == bfsForest" $ \x ->
         (bfsForest . forest . bfsForest) x     == bfsForest x

    test "bfsForest (vertices vs)               == map (\\v -> Node v []) (nub $ sort vs)" $ \vs ->
          bfsForest (vertices vs)               == map (\v -> Node v []) (nub $ sort vs)

    test "bfsForest $ 3 * (1 + 4) * (1 + 5)     == <correct result>" $
          bfsForest  (3 * (1 + 4) * (1 + 5))    == [ Node { rootLabel = 1
                                                   , subForest = [ Node { rootLabel = 5
                                                                        , subForest = [] }]}
                                                   , Node { rootLabel = 3
                                                   , subForest = [ Node { rootLabel = 4
                                                                        , subForest = [] }]}]
          
    test "bfsForest (circuit [1..5] + (circuit [5,4..1])) == <correct result>" $
          bfsForest (circuit [1..5] + (circuit [5,4..1])) ==
          [ Node { rootLabel = 1
                 , subForest = [ Node { rootLabel = 2
                                      , subForest = [ Node { rootLabel = 3
                                                           , subForest = []}]}
                               , Node { rootLabel = 5
                                      , subForest = [ Node { rootLabel = 4
                                                           , subForest = []}]}]}]

testBfsForestFrom :: TestsuiteInt g -> IO ()
testBfsForestFrom (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "bfsForestFrom ============"
    test "bfsForestFrom vs empty                           == []" $ \vs ->
          bfsForestFrom vs empty                           == []

    test "forest (bfsForestFrom [1]   $ edge 1 1)          == vertex 1" $
          forest (bfsForestFrom [1]   $ edge 1 1)          == vertex 1

    test "forest (bfsForestFrom [1]   $ edge 1 2)          == edge 1 2" $
          forest (bfsForestFrom [1]   $ edge 1 2)          == edge 1 2

    test "forest (bfsForestFrom [2]   $ edge 1 2)          == vertex 2" $
          forest (bfsForestFrom [2]   $ edge 1 2)          == vertex 2

    test "forest (bfsForestFrom [3]   $ edge 1 2)          == empty" $
          forest (bfsForestFrom [3]   $ edge 1 2)          == empty

    test "forest (bfsForestFrom [2,1] $ edge 1 2)          == vertices [1,2]" $
          forest (bfsForestFrom [2,1] $ edge 1 2)          == vertices [1,2]

    test "isSubgraphOf (forest $ bfsForestFrom vs x) x     == True" $ \vs x ->
          isSubgraphOf (forest $ bfsForestFrom vs x) x     == True

    test "bfsForestFrom (vertexList x) x                   == bfsForest x" $ \x ->
          bfsForestFrom (vertexList x) x                   == bfsForest x

    test "bfsForestFrom vs             (vertices vs)       == map (\\v -> Node v []) (nub vs)" $ \vs ->
          bfsForestFrom vs             (vertices vs)       == map (\v -> Node v []) (nub vs)

    test "bfsForestFrom []             x                   == []" $ \x ->
          bfsForestFrom []             x                   == []

    test "bfsForestFrom [1,4] $ 3 * (1 + 4) * (1 + 5)      == <correct result>" $
          bfsForestFrom [1,4]  (3 * (1 + 4) * (1 + 5))     == [ Node { rootLabel = 1
                                                                     , subForest = [ Node { rootLabel = 5
                                                                                          , subForest = [] }]}
                                                              , Node { rootLabel = 4
                                                                     , subForest = [] }]
          
    test "bfsForestFrom [3] (circuit [1..5] + (circuit [5,4..1])) == <correct result>" $
          bfsForestFrom [3] (circuit [1..5] + (circuit [5,4..1])) ==
          [ Node { rootLabel = 3
                 , subForest = [ Node { rootLabel = 2
                                      , subForest = [ Node { rootLabel = 1
                                                           , subForest = []}]}
                               , Node { rootLabel = 4
                                      , subForest = [ Node { rootLabel = 5
                                                           , subForest = []}]}]}]
          
testBfs :: TestsuiteInt g -> IO ()
testBfs (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "bfs ============"
    
    test "bfs vs    $ empty                    == []" $ \vs ->
          bfs vs      empty                    == []

    test "bfs []    $ g                        == []" $ \g ->
          bfs []      g                        == []

    test "bfs [1]   $ edge 1 1                 == [[1]]" $
          bfs [1]    (edge 1 1)                == [[1]]

    test "bfs [1]   $ edge 1 2                 == [[1],[2]]" $
          bfs [1]    (edge 1 2)                == [[1],[2]]

    test "bfs [2]   $ edge 1 2                 == [[2]]" $
          bfs [2]    (edge 1 2)                == [[2]]

    test "bfs [3]   $ edge 1 2                 == []" $
          bfs [3]    (edge 1 2)                == []

    test "bfs [1,2] $ edge 1 2                 == [[1],[2]]" $
          bfs [1,2]  (edge 1 2)                == [[1],[2]]

    test "bfs [2,1] $ edge 1 2                 == [[2],[1]]" $
          bfs [2,1]  (edge 1 2)                == [[2],[1]]

    test "bfs [1,2] ((1*2) + (3*4) + (5*6))    == [[1],[2]]" $
          bfs [1,2] ((1*2) + (3*4) + (5*6))    == [[1],[2]]

    test "bfs [1,3] ((1*2) + (3*4) + (5*6))    == [[1],[2],[3],[4]]" $
          bfs [1,3] ((1*2) + (3*4) + (5*6))    == [[1],[2],[3],[4]]

    test "bfs [3] $ 3 * (1 + 4) * (1 + 5)    == [[3],[1,4,5]]" $
          bfs [3]  (3 * (1 + 4) * (1 + 5))   == [[3],[1,4,5]]

    test "bfs [2] (circuit [1..5] + (circuit [5,4..1])) == [[2],[1,3],[5,4]]" $
          bfs [2] (circuit [1..5] + (circuit [5,4..1])) == [[2],[1,3],[5,4]]
          
    test "concat (bfs [3] $ circuit [1..5] + circuit [5,4..1]) == [3,2,4,1,5]" $
          concat (bfs [3] $ circuit [1..5] + circuit [5,4..1]) == [3,2,4,1,5]
          
    test "isSubgraphOf (vertices $ concat $ bfs vs x) x == True" $ \vs x ->
          isSubgraphOf (vertices $ concat $ bfs vs x) x == True

testDfsForest :: TestsuiteInt g -> IO ()
testDfsForest (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "dfsForest ============"
    test "dfsForest empty                       == []" $
          dfsForest empty                       == []

    test "forest (dfsForest $ edge 1 1)         == vertex 1" $
          forest (dfsForest $ edge 1 1)         == vertex 1

    test "forest (dfsForest $ edge 1 2)         == edge 1 2" $
          forest (dfsForest $ edge 1 2)         == edge 1 2

    test "forest (dfsForest $ edge 2 1)         == vertices [1,2]" $
          forest (dfsForest $ edge 2 1)         == vertices [1,2]

    test "isSubgraphOf (forest $ dfsForest x) x == True" $ \x ->
          isSubgraphOf (forest $ dfsForest x) x == True

    test "isDfsForestOf (dfsForest x) x         == True" $ \x ->
          isDfsForestOf (dfsForest x) x         == True

    test "dfsForest . forest . dfsForest        == dfsForest" $ \x ->
         (dfsForest . forest . dfsForest) x     == dfsForest x

    test "dfsForest (vertices vs)               == map (\\v -> Node v []) (nub $ sort vs)" $ \vs ->
          dfsForest (vertices vs)               == map (\v -> Node v []) (nub $ sort vs)

    test "dfsForest $ 3 * (1 + 4) * (1 + 5)     == <correct result>" $
          dfsForest  (3 * (1 + 4) * (1 + 5))    == [ Node { rootLabel = 1
                                                   , subForest = [ Node { rootLabel = 5
                                                                        , subForest = [] }]}
                                                   , Node { rootLabel = 3
                                                   , subForest = [ Node { rootLabel = 4
                                                                        , subForest = [] }]}]
    test "forest (dfsForest $ circuit [1..5] + circuit [5,4..1]) == path [1,2,3,4,5]" $
          forest (dfsForest $ circuit [1..5] + circuit [5,4..1]) == path [1,2,3,4,5]

testDfsForestFrom :: TestsuiteInt g -> IO ()
testDfsForestFrom (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "dfsForestFrom ============"
    test "dfsForestFrom vs empty                           == []" $ \vs ->
          dfsForestFrom vs empty                           == []

    test "forest (dfsForestFrom [1]   $ edge 1 1)          == vertex 1" $
          forest (dfsForestFrom [1]   $ edge 1 1)          == vertex 1

    test "forest (dfsForestFrom [1]   $ edge 1 2)          == edge 1 2" $
          forest (dfsForestFrom [1]   $ edge 1 2)          == edge 1 2

    test "forest (dfsForestFrom [2]   $ edge 1 2)          == vertex 2" $
          forest (dfsForestFrom [2]   $ edge 1 2)          == vertex 2

    test "forest (dfsForestFrom [3]   $ edge 1 2)          == empty" $
          forest (dfsForestFrom [3]   $ edge 1 2)          == empty

    test "forest (dfsForestFrom [2,1] $ edge 1 2)          == vertices [1,2]" $
          forest (dfsForestFrom [2,1] $ edge 1 2)          == vertices [1,2]

    test "isSubgraphOf (forest $ dfsForestFrom vs x) x     == True" $ \vs x ->
          isSubgraphOf (forest $ dfsForestFrom vs x) x     == True

    test "isDfsForestOf (dfsForestFrom (vertexList x) x) x == True" $ \x ->
          isDfsForestOf (dfsForestFrom (vertexList x) x) x == True

    test "dfsForestFrom (vertexList x) x                   == dfsForest x" $ \x ->
          dfsForestFrom (vertexList x) x                   == dfsForest x

    test "dfsForestFrom vs             (vertices vs)       == map (\\v -> Node v []) (nub vs)" $ \vs ->
          dfsForestFrom vs             (vertices vs)       == map (\v -> Node v []) (nub vs)

    test "dfsForestFrom []             x                   == []" $ \x ->
          dfsForestFrom []             x                   == []

    test "dfsForestFrom [1,4] $ 3 * (1 + 4) * (1 + 5)      == <correct result>" $
          dfsForestFrom [1,4]  (3 * (1 + 4) * (1 + 5))     == [ Node { rootLabel = 1
                                                                     , subForest = [ Node { rootLabel = 5
                                                                                          , subForest = [] }]}
                                                              , Node { rootLabel = 4
                                                                     , subForest = [] }]
    test "forest (dfsForestFrom [3] $ circuit [1..5] + circuit [5,4..1]) == path [3,2,1,5,4]" $
          forest (dfsForestFrom [3] $ circuit [1..5] + circuit [5,4..1]) == path [3,2,1,5,4]
   

testDfs :: TestsuiteInt g -> IO ()
testDfs (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "dfs ============"
    test "dfs vs    $ empty                    == []" $ \vs ->
          dfs vs      empty                    == []

    test "dfs [1]   $ edge 1 1                 == [1]" $
          dfs [1]    (edge 1 1)                == [1]

    test "dfs [1]   $ edge 1 2                 == [1,2]" $
          dfs [1]    (edge 1 2)                == [1,2]

    test "dfs [2]   $ edge 1 2                 == [2]" $
          dfs [2]    (edge 1 2)                 == [2]

    test "dfs [3]   $ edge 1 2                 == []" $
          dfs [3]    (edge 1 2)                == []

    test "dfs [1,2] $ edge 1 2                 == [1,2]" $
          dfs [1,2]  (edge 1 2)                == [1,2]

    test "dfs [2,1] $ edge 1 2                 == [2,1]" $
          dfs [2,1]  (edge 1 2)                 == [2,1]

    test "dfs []    $ x                        == []" $ \x ->
          dfs []      x                        == []

    test "dfs [1,4] $ 3 * (1 + 4) * (1 + 5)    == [1,5,4]" $
          dfs [1,4]  (3 * (1 + 4) * (1 + 5))   == [1,5,4]

    test "isSubgraphOf (vertices $ dfs vs x) x == True" $ \vs x ->
          isSubgraphOf (vertices $ dfs vs x) x == True

    test "dfs [3] (circuit [1..5] + circuit [5,4..1]) == [3,2,1,5,4]" $
          dfs [3] (circuit [1..5] + circuit [5,4..1]) == [3,2,1,5,4]

testReachable :: TestsuiteInt g -> IO ()
testReachable (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "dfs ============"
    test "reachable x $ empty                       == []" $ \x ->
          reachable x   empty                       == []

    test "reachable 1 $ vertex 1                    == [1]" $
          reachable 1  (vertex 1)                   == [1]

    test "reachable 1 $ vertex 2                    == []" $
          reachable 1  (vertex 2)                   == []

    test "reachable 1 $ edge 1 1                    == [1]" $
          reachable 1  (edge 1 1)                   == [1]

    test "reachable 1 $ edge 1 2                    == [1,2]" $
          reachable 1  (edge 1 2)                   == [1,2]

    test "reachable 4 $ path    [1..8]              == [4..8]" $
          reachable 4  (path    [1..8])             == [4..8]

    test "reachable 4 $ circuit [1..8]              == [4..8] ++ [1..3]" $
          reachable 4  (circuit [1..8])             == [4..8] ++ [1..3]

    test "reachable 8 $ clique  [8,7..1]            == [8] ++ [1..7]" $
          reachable 8  (clique  [8,7..1])           == [8] ++ [1..7]

    test "isSubgraphOf (vertices $ reachable x y) y == True" $ \x y ->
          isSubgraphOf (vertices $ reachable x y) y == True

testTopSort :: TestsuiteInt g -> IO ()
testTopSort (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "topSort ============"
    test "topSort (1 * 2 + 3 * 1)               == Just [3,1,2]" $
          topSort (1 * 2 + 3 * 1)               == Just [3,1,2]

    test "topSort (1 * 2 + 2 * 1)               == Nothing" $
          topSort (1 * 2 + 2 * 1)               == Nothing

    test "fmap (flip isTopSortOf x) (topSort x) /= Just False" $ \x ->
          fmap (flip isTopSortOf x) (topSort x) /= Just False

testIsAcyclic :: TestsuiteInt g -> IO ()
testIsAcyclic (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "testIsAcyclic ============"
    test "isAcyclic (1 * 2 + 3 * 1) == True" $
          isAcyclic (1 * 2 + 3 * 1) == True

    test "isAcyclic (1 * 2 + 2 * 1) == False" $
          isAcyclic (1 * 2 + 2 * 1) == False

    test "isAcyclic . circuit       == null" $ \xs ->
         (isAcyclic . circuit) xs  == null xs

    test "isAcyclic                 == isJust . topSort" $ \x ->
          isAcyclic x               == isJust (topSort x)

testIsDfsForestOf :: TestsuiteInt g -> IO ()
testIsDfsForestOf (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "isDfsForestOf ============"
    test "isDfsForestOf []                              empty            == True" $
          isDfsForestOf []                              empty            == True

    test "isDfsForestOf []                              (vertex 1)       == False" $
          isDfsForestOf []                              (vertex 1)       == False

    test "isDfsForestOf [Node 1 []]                     (vertex 1)       == True" $
          isDfsForestOf [Node 1 []]                     (vertex 1)       == True

    test "isDfsForestOf [Node 1 []]                     (vertex 2)       == False" $
          isDfsForestOf [Node 1 []]                     (vertex 2)       == False

    test "isDfsForestOf [Node 1 [], Node 1 []]          (vertex 1)       == False" $
          isDfsForestOf [Node 1 [], Node 1 []]          (vertex 1)       == False

    test "isDfsForestOf [Node 1 []]                     (edge 1 1)       == True" $
          isDfsForestOf [Node 1 []]                     (edge 1 1)       == True

    test "isDfsForestOf [Node 1 []]                     (edge 1 2)       == False" $
          isDfsForestOf [Node 1 []]                     (edge 1 2)       == False

    test "isDfsForestOf [Node 1 [], Node 2 []]          (edge 1 2)       == False" $
          isDfsForestOf [Node 1 [], Node 2 []]          (edge 1 2)       == False

    test "isDfsForestOf [Node 2 [], Node 1 []]          (edge 1 2)       == True" $
          isDfsForestOf [Node 2 [], Node 1 []]          (edge 1 2)       == True

    test "isDfsForestOf [Node 1 [Node 2 []]]            (edge 1 2)       == True" $
          isDfsForestOf [Node 1 [Node 2 []]]            (edge 1 2)       == True

    test "isDfsForestOf [Node 1 [], Node 2 []]          (vertices [1,2]) == True" $
          isDfsForestOf [Node 1 [], Node 2 []]          (vertices [1,2]) == True

    test "isDfsForestOf [Node 2 [], Node 1 []]          (vertices [1,2]) == True" $
          isDfsForestOf [Node 2 [], Node 1 []]          (vertices [1,2]) == True

    test "isDfsForestOf [Node 1 [Node 2 []]]            (vertices [1,2]) == False" $
          isDfsForestOf [Node 1 [Node 2 []]]            (vertices [1,2]) == False

    test "isDfsForestOf [Node 1 [Node 2 [Node 3 []]]]   (path [1,2,3])   == True" $
          isDfsForestOf [Node 1 [Node 2 [Node 3 []]]]   (path [1,2,3])   == True

    test "isDfsForestOf [Node 1 [Node 3 [Node 2 []]]]   (path [1,2,3])   == False" $
          isDfsForestOf [Node 1 [Node 3 [Node 2 []]]]   (path [1,2,3])   == False

    test "isDfsForestOf [Node 3 [], Node 1 [Node 2 []]] (path [1,2,3])   == True" $
          isDfsForestOf [Node 3 [], Node 1 [Node 2 []]] (path [1,2,3])   == True

    test "isDfsForestOf [Node 2 [Node 3 []], Node 1 []] (path [1,2,3])   == True" $
          isDfsForestOf [Node 2 [Node 3 []], Node 1 []] (path [1,2,3])   == True

    test "isDfsForestOf [Node 1 [], Node 2 [Node 3 []]] (path [1,2,3])   == False" $
          isDfsForestOf [Node 1 [], Node 2 [Node 3 []]] (path [1,2,3])   == False

testIsTopSortOf :: TestsuiteInt g -> IO ()
testIsTopSortOf (prefix, API{..}) = do
    putStrLn $ "\n============ " ++ prefix ++ "isTopSortOf ============"
    test "isTopSortOf [3,1,2] (1 * 2 + 3 * 1) == True" $
          isTopSortOf [3,1,2] (1 * 2 + 3 * 1) == True

    test "isTopSortOf [1,2,3] (1 * 2 + 3 * 1) == False" $
          isTopSortOf [1,2,3] (1 * 2 + 3 * 1) == False

    test "isTopSortOf []      (1 * 2 + 3 * 1) == False" $
          isTopSortOf []      (1 * 2 + 3 * 1) == False

    test "isTopSortOf []      empty           == True" $
          isTopSortOf []      empty           == True

    test "isTopSortOf [x]     (vertex x)      == True" $ \x ->
          isTopSortOf [x]     (vertex x)      == True

    test "isTopSortOf [x]     (edge x x)      == False" $ \x ->
          isTopSortOf [x]     (edge x x)      == False
