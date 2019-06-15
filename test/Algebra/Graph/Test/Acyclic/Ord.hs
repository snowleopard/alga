{-# LANGUAGE ViewPatterns #-}
-------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Acyclic.AdjacencyMap
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Acyclic.AdjacencyMap".
-------------------------------------------------------------

module Algebra.Graph.Test.Acyclic.Ord (
  testAcyclicOrd 
  ) where

import Algebra.Graph.Acyclic.AdjacencyMap
import Algebra.Graph.Acyclic.Ord
import Algebra.Graph.Test

import qualified Data.List

type AAI = AdjacencyMap Int

-- TODO: Switch to using generic tests.
testAcyclicOrd :: IO ()
testAcyclicOrd = do

  putStrLn "\n=====AcyclicOrd consistency====="

  test "edge" $ \x y                       -> consistent (edge x y :: AAI)
  test "overlay" $ \x y                    -> consistent (overlay x y :: AAI)
  test "connect" $ \x y                    -> consistent (connect x y :: AAI)
  test "edges" $ \x                        -> consistent (edges x :: AAI)
  test "overlays" $ \x                     -> consistent (overlays x :: AAI)
  test "connects" $ \x                     -> consistent (connects x :: AAI)
  test "replaceVertex" $ \x y z            -> consistent (replaceVertex x y z :: AAI)
  test "mergeVertices" $ \(apply -> p) v x -> consistent (mergeVertices p v x :: AAI) 
  test "gmap" $ \(apply -> f) x            -> consistent (gmap f (x :: AAI) :: AAI) 

  putStrLn "\n=====AcyclicOrd edge====="

  test "edge x y                      == connect (vertex x) (vertex y)" $ \x y ->
        (edge x y :: AAI)             == connect (vertex x) (vertex y)
  test "hasEdge 1 2 (edge 1 2)        == True" $
        hasEdge 1 2 (edge 1 2 :: AAI) == True
  test "hasEdge 2 1 (edge 2 1)        == False" $
        hasEdge 2 1 (edge 2 1 :: AAI) == False
  test "edgeCount   (edge 1 2)        == 1" $
        edgeCount   (edge 1 2 :: AAI) == 1
  test "edgeCount   (edge 2 1)        == 0" $
        edgeCount   (edge 2 1 :: AAI) == 0
  test "vertexCount (edge 1 1)        == 1" $
        vertexCount (edge 1 1 :: AAI) == 1
  test "vertexCount (edge 1 2)        == 2" $
        vertexCount (edge 1 2 :: AAI) == 2
  test "vertexCount (edge 2 1)        == 2" $
        vertexCount (edge 2 1 :: AAI) == 2

  putStrLn "\n=====AcyclicOrd overlay====="

  test "isEmpty     (overlay x y)        == isEmpty x && isEmpty y" $ \x y ->
        isEmpty     (overlay x y :: AAI) == (isEmpty x && isEmpty y)
  test "hasVertex z (overlay x y)        == hasVertex z x || hasVertex z y" $ \x y z ->
        hasVertex z (overlay x y :: AAI) == (hasVertex z x || hasVertex z y)
  test "vertexCount (overlay x y)        >= vertexCount x" $ \x y ->
        vertexCount (overlay x y :: AAI) >= vertexCount x
  test "vertexCount (overlay x y)        <= vertexCount x + vertexCount y" $ \x y ->
        vertexCount (overlay x y :: AAI) <= vertexCount x + vertexCount y
  test "edgeCount   (overlay x y)        >= edgeCount x" $ \x y ->
        edgeCount   (overlay x y :: AAI) >= edgeCount x
  test "edgeCount   (overlay x y)        <= edgeCount x + edgeCount y" $ \x y ->
        edgeCount   (overlay x y :: AAI) <= edgeCount x + edgeCount y
  test "vertexCount (overlay 1 2)        == 2" $
        vertexCount (overlay 1 2 :: AAI) == 2
  test "edgeCount   (overlay 1 2)        == 0" $
        edgeCount   (overlay 1 2 :: AAI) == 0

  putStrLn "\n=====AcyclicOrd connect====="

  test "isEmpty     (connect x y)        == isEmpty x && isEmpty y" $ \x y ->
        isEmpty     (connect x y :: AAI) == (isEmpty x && isEmpty y)
  test "hasVertex z (connect x y)        == hasVertex z x || hasVertex z y" $ \x y z ->
        hasVertex z (connect x y :: AAI) == (hasVertex z x || hasVertex z y)
  test "vertexCount (connect x y)        >= vertexCount x" $ \x y ->
        vertexCount (connect x y :: AAI) >= vertexCount x
  test "vertexCount (connect x y)        <= vertexCount x + vertexCount y" $ \x y ->
        vertexCount (connect x y :: AAI) <= vertexCount x + vertexCount y
  test "edgeCount   (connect x y)        >= edgeCount x" $ \x y ->
        edgeCount   (connect x y :: AAI) >= edgeCount x
  test "edgeCount   (connect x y)        >= edgeCount y" $ \x y ->
        edgeCount   (connect x y :: AAI) >= edgeCount y
  test "edgeCount   (connect x y)        <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ \x y ->
        edgeCount   (connect x y :: AAI) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y
  test "vertexCount (connect 1 2)        == 2" $
        vertexCount (connect 1 2 :: AAI) == 2
  test "edgeCount   (connect 1 2)        == 1" $
        edgeCount   (connect 1 2 :: AAI) == 1
  test "edgeCount   (connect 2 1) == 0" $
        edgeCount   (connect 2 1 :: AAI) == 0

  putStrLn "\n=====AcyclicOrd edges====="

  test "edges []                   == empty" $
        (edges [] :: AAI)          == empty
  test "edges [(x,y)]              == edge x y" $ \x y ->
        (edges [(x,y)] :: AAI)     == edge x y
  test "edgeCount . edges          == length . filter (uncurry (<)) . Data.List.nub" $ \x ->
        edgeCount (edges x :: AAI) == (length . filter (uncurry (<)) . Data.List.nub $ x)
  test "edgeList . edges           == filter (uncurry (<)) . Data.List.nub . Data.List.sort" $ \x ->
        edgeList (edges x :: AAI)  == (filter (uncurry (<)) . Data.List.nub . Data.List.sort $ x)

  putStrLn "\n=====AcyclicOrd overlays====="

  test "overlays []               == empty" $
        (overlays [] :: AAI)      == empty
  test "overlays [x]              == x" $ \x ->
        (overlays [x] :: AAI)     == x
  test "overlays [x,y]            == overlay x y" $ \x y ->
        (overlays [x,y] :: AAI)   == overlay x y
  test "overlays                  == foldr overlay empty" $ \x ->
        (overlays x :: AAI)       == foldr overlay empty x
  test "isEmpty . overlays        == all isEmpty" $ \x ->
        isEmpty (overlays x :: AAI) == all isEmpty x

  putStrLn "\n=====AcyclicAdjacencyMap Show====="

  test "connects []                 == empty" $
        (connects [] :: AAI)        == empty
  test "connects [x]                == x" $ \x ->
        (connects [x] :: AAI)       == x
  test "connects [x,y]              == connect x y" $ \x y ->
        (connects [x,y] :: AAI)     == connect x y
  test "connects                    == foldr connect empty" $ \x ->
        (connects x :: AAI)         == foldr connect empty x
  test "isEmpty . connects          == all isEmpty" $ \x ->
        isEmpty (connects x :: AAI) == all isEmpty x

  putStrLn "\n=====AcyclicOrd replaceVertex====="

  test "replaceVertex x x                   == id" $ \x y ->
        (replaceVertex x x y :: AAI)        == id y
  test "replaceVertex x y (vertex x)        == vertex y" $ \x y ->
        replaceVertex x y (vertex x :: AAI) == vertex y
  test "replaceVertex x y                   == mergeVertices ( == x) y" $ \x y z ->
        (replaceVertex x y z :: AAI)        == mergeVertices ( == x) y z
  test "replaceVertex 1 2 (1 * 3)           == 2 * 3" $
        replaceVertex 1 2 (1 * 3 :: AAI)    == 2 * 3
  test "replaceVertex 1 4 (1 * 3)           == 4 + 3" $
        replaceVertex 1 4 (1 * 3 :: AAI)    == 4 + 3

  putStrLn "\n=====AcyclicOrd mergeVertices====="

  test "mergeVertices (const False) x               == id" $ \x y ->
        mergeVertices (const False) x (y :: AAI)    == id y
  test "mergeVertices (                             == x) y             == replaceVertex x y" $ \x y z ->
        mergeVertices (                             == x) y (z :: AAI)  == replaceVertex x y z
  test "mergeVertices even 1 (0 * 2)                == 1" $
        mergeVertices even 1 (0 * 2 :: AAI)         == 1
  test "mergeVertices odd  1 (3 + 4 * 5)            == 4 + 1" $
        mergeVertices odd  1 (3 + 4 * 5 :: AAI)     == 4 + 1
  test "mergeVertices even 1 (2 * 3 + 4 * 5)        == 1 * 3 + 1 * 5" $
        mergeVertices even 1 (2 * 3 + 4 * 5 :: AAI) == 1 * 3 + 1 * 5

  putStrLn "\n=====AcyclicOrd gmap====="

  test "gmap f empty                                 == empty" $ \(apply -> f) ->
        gmap f (empty :: AAI)                        == (empty :: AAI)
  test "gmap f (vertex x)                            == vertex (f x)" $ \(apply -> f) x ->
        gmap f (vertex x :: AAI)                     == (vertex (f x) :: AAI)
  test "edgeCount (gmap f (edge x y))                <= edgeCount (edge (f x) (f y))" $ \(apply -> f) x y ->
        edgeCount (gmap f (edge x y :: AAI))         <= edgeCount (edge (f x) (f y) :: AAI)
  test "vertexList (gmap f (edge x y))               == vertexList (edge (f x) (f y))" $ \(apply -> f) x y ->
        vertexList (gmap f (edge x y :: AAI))        == vertexList (edge (f x) (f y) :: AAI)
  test "gmap id                                      == id" $ \x ->
        gmap id (x :: AAI)                           == id x
  test "vertexList . gmap f . gmap g                 == vertexList . gmap (f . g)" $ \(apply -> f) (apply -> g) x ->
        vertexList (gmap f (gmap g x :: AAI) :: AAI) == vertexList (gmap (f . g) (x :: AAI))
  test "edgeCount (gmap f (gmap g x))                <= edgeCount (gmap (f . g) x)" $ \(apply -> f) (apply -> g) x ->
        edgeCount (gmap f (gmap g x :: AAI) :: AAI)  <= edgeCount (gmap (f . g) (x :: AAI))
