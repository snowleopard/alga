{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Acyclic.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Acyclic.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Acyclic.AdjacencyMap (testAcyclicAdjacencyMap) where

import Algebra.Graph.Acyclic.AdjacencyMap
import Algebra.Graph.Acyclic.Ord
import Algebra.Graph.Test
import Algebra.Graph.ToGraph (ToGraph (toGraph))
import Data.List.NonEmpty hiding (transpose, filter, length)

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

type AAI = AdjacencyMap Int
type AAE = AdjacencyMap (Either Int Int)
type AAT = AdjacencyMap (Int, Int)
type AI = AM.AdjacencyMap Int

-- TODO: Switch to using generic tests.
testAcyclicAdjacencyMap :: IO ()
testAcyclicAdjacencyMap = do
  testAcyclicOrd

  putStrLn "\n=====AcyclicAdjacencyMap Show====="

  test "show empty              == \"fromMaybe empty . toAcyclic $ empty\"" $
        show (empty :: AAI)     == "fromMaybe empty . toAcyclic $ empty"
  test "show 1                  == \"fromMaybe empty . toAcyclic $ vertex 1\"" $
        show (1 :: AAI)         == "fromMaybe empty . toAcyclic $ vertex 1"
  test "show (1 + 2)            == \"fromMaybe empty . toAcyclic $ vertices [1,2]\"" $
        show (1 + 2 :: AAI)     == "fromMaybe empty . toAcyclic $ vertices [1,2]"
  test "show (1 * 2)            == \"fromMaybe empty . toAcyclic $ edge 1 2\"" $
        show (1 * 2 :: AAI)     == "fromMaybe empty . toAcyclic $ edge 1 2"
  test "show (1 * 2 * 3)        == \"fromMaybe empty . toAcyclic $ edges [(1,2),(1,3),(2,3)]\"" $
        show (1 * 2 * 3 :: AAI) == "fromMaybe empty . toAcyclic $ edges [(1,2),(1,3),(2,3)]"
  test "show (1 * 2 + 3)        == \"fromMaybe empty . toAcyclic $ overlay (vertex 3) (edge 1 2)\"" $
        show (1 * 2 + 3 :: AAI) == "fromMaybe empty . toAcyclic $ overlay (vertex 3) (edge 1 2)"

  putStrLn "\n=====AcyclicAdjacencyMap toAcyclic====="

  test "toAcyclic (AdjacencyMap.'AM.path' [1, 2, 1]) == Nothing" $
        toAcyclic (AM.path [1, 2, 1] :: AI)          == Nothing
  test "toAcyclic (AdjacencyMap.'AM.path' [1, 2, 3]) == Just (1 * 2 + 2 * 3)" $
        toAcyclic (AM.path [1, 2, 3] :: AI)          == Just (1 * 2 + 2 * 3)

  putStrLn "\n=====AcyclicAdjacencyMap toAcyclicOrd====="

  test "toAcyclicOrd (2 * 1)               == 1 + 2" $
        toAcyclicOrd (2 * 1 :: AI)         == 1 + 2
  test "toAcyclicOrd (1 * 2)               == 1 * 2" $
        toAcyclicOrd (1 * 2 :: AI)         == 1 * 2
  test "toAcyclicOrd (1 * 2 + 2 * 1)       == 1 * 2" $
        toAcyclicOrd (1 * 2 + 2 * 1 :: AI) == 1 * 2
  test "toAcyclicOrd                       == fromGraph (<) . toGraph" $ \x ->
        toAcyclicOrd (x :: AI)             == (fromGraph (<) . toGraph $ x)

  putStrLn "\n=====AcyclicAdjacencyMap fromAcyclic====="

  test "fromAcyclic (1 * 2 + 3 * 4)                 == AM.edges [(1,2), (3,4)]" $
        fromAcyclic (1 * 2 + 3 * 4 :: AAI)          == AM.edges [(1,2), (3,4)]
  test "AM.vertexCount  . fromAcyclic               == vertexCount" $ \x ->
        (AM.vertexCount . fromAcyclic $ (x :: AAI)) == vertexCount x
  test "AM.edgeCount    . fromAcyclic               == edgeCount" $ \x ->
        (AM.edgeCount   . fromAcyclic $ (x :: AAI)) == edgeCount x

  putStrLn "\n=====AcyclicAdjacencyMap consistency====="

  test "arbitraryAcyclicAdjacencyMap" $ \x -> consistent (x :: AAI)
  test "empty" $                              consistent (empty :: AAI)
  test "vertex" $ \x                       -> consistent (vertex x :: AAI)
  test "disjointOverlay" $ \x y            -> consistent (disjointOverlay x y :: AAE)
  test "disjointConnect" $ \x y            -> consistent (disjointConnect x y :: AAE)
  test "vertices" $ \x                     -> consistent (vertices x :: AAI)
  test "box" $ \x y                        -> consistent (box x y :: AAT)
  test "transitiveClosure" $ \x            -> consistent (transitiveClosure x :: AAI)
  test "transpose" $ \x                    -> consistent (transpose x :: AAI)
  test "fromGraph (<)" $ \x                -> consistent (fromGraph (<) x :: AAI)
  test "fromGraph (>)" $ \x                -> consistent (fromGraph (>) x :: AAI)
  test "toAcyclicOrd" $ \x                 -> consistent (toAcyclicOrd x :: AAI)

  test "consistent (1 + 2)                == True" $
        consistent (1 + 2 :: AAI)         == True
  test "consistent (1 * 2 + 2 * 3)        == True" $
        consistent (1 * 2 + 2 * 3 :: AAI) == True

  putStrLn "\n=====AcyclicAdjacencyMap Num instance====="
  test "vertexList  1                 == [1]" $
        vertexList (1 :: AAI)         == [1]
  test "edgeList    1                 == []" $
        edgeList   (1 :: AAI)         == []
  test "edgeList   (1 + 2)            == []" $
        edgeList   (1 + 2 :: AAI)     == []
  test "vertexList (1 + 2)            == [1,2]" $
        vertexList (1 + 2 :: AAI)     == [1,2]
  test "edgeList   (1 * 2)            == [(1,2)]" $
        edgeList   (1 * 2 :: AAI)     == [(1,2)]
  test "vertexList (1 * 2)            == [1,2]" $
        vertexList (1 * 2 :: AAI)     == [1,2]
  test "edgeList   (1 + 2 * 3)        == [(2,3)]" $
        edgeList   (1 + 2 * 3 :: AAI) == [(2,3)]
  test "vertexList (1 + 2 * 3)        == [1,2,3]" $
        vertexList (1 + 2 * 3 :: AAI) == [1,2,3]
  test "edgeList   (1 * 2 + 3)        == [(1,2)]" $
        edgeList   (1 * 2 + 3 :: AAI) == [(1,2)]
  test "vertexList (1 * 2 + 3)        == [1,2,3]" $
        vertexList (1 * 2 + 3 :: AAI) == [1,2,3]

  putStrLn "\n=====AcyclicAdjacencyMap construction primitives====="

  test "isEmpty 'empty'                                  == True" $
        isEmpty (empty :: AAI)                           == True
  test "isEmpty ('vertex' x)                             == False" $ \x ->
        isEmpty (vertex x :: AAI)                        == False
  test "isEmpty ('disjointOverlay' 'empty' 'empty')             == True" $
        isEmpty (disjointOverlay (empty :: AAI) (empty :: AAI)) == True
  test "isEmpty ('removeVertex' x $ 'vertex' x)          == True" $ \x ->
        isEmpty (removeVertex x $ vertex x :: AAI)       == True
  test "isEmpty ('removeEdge' 1 2 $ 1 * 2)               == False" $
        isEmpty (removeEdge 1 2 $ 1 * 2 :: AAI)          == False

  test "'isEmpty'     (vertex x)        == False" $ \x ->
         isEmpty      (vertex x :: AAI) == False
  test "'hasVertex' x (vertex x)        == True" $ \x ->
         hasVertex x  (vertex x :: AAI) == True
  test "'vertexCount' (vertex x)        == 1" $ \x ->
         vertexCount  (vertex x :: AAI) == 1
  test "'edgeCount'   (vertex x)        == 0" $ \x ->
         edgeCount    (vertex x :: AAI) == 0

  test "vertices []                       == 'empty'" $
        vertices []                       == (empty :: AAI)
  test "vertices [x]                      == 'vertex' x" $ \x ->
        vertices [x]                      == (vertex x :: AAI)
  test "'hasVertex' x . vertices          == 'elem' x" $ \x y ->
        (hasVertex x (vertices y :: AAI)) == elem x y
  test "'vertexCount' . vertices          == 'length' . 'Data.List.nub'" $ \x ->
        (vertexCount (vertices x :: AAI)) == (List.length . List.nub $ x)
  test "'vertexSet'   . vertices          == Set.'Set.fromList'" $ \x ->
        (vertexSet (vertices x :: AAI))   == Set.fromList x

  test "'isEmpty' (disjointOverlay x y)                  == 'isEmpty' x && 'isEmpty' y" $ \x y ->
        isEmpty (disjointOverlay x y :: AAE)             == (isEmpty x && isEmpty y)
  test "'hasVertex' (Left z) (disjointOverlay x y)       == 'hasVertex' z x" $ \x y z ->
        hasVertex (Left z) (disjointOverlay x y :: AAE)  == hasVertex z x
  test "'hasVertex' (Right z) (disjointOverlay x y)      == 'hasVertex' z y" $ \x y z ->
        hasVertex (Right z) (disjointOverlay x y :: AAE) == hasVertex z y
  test "'vertexCount' (disjointOverlay x y)              >= 'vertexCount' x" $ \x y ->
        vertexCount (disjointOverlay x y :: AAE)         >= vertexCount x
  test "'vertexCount' (disjointOverlay x y)              == 'vertexCount' x + 'vertexCount' y" $ \x y ->
        vertexCount (disjointOverlay x y :: AAE)         == vertexCount x + vertexCount y
  test "'edgeCount' (disjointOverlay x y)                >= 'edgeCount' x" $ \x y ->
        edgeCount (disjointOverlay x y :: AAE)           >= edgeCount x
  test "'edgeCount' (disjointOverlay x y)                == 'edgeCount' x   + 'edgeCount' y" $ \x y ->
        edgeCount (disjointOverlay x y :: AAE)           == edgeCount x   + edgeCount y
  test "'vertexCount' (disjointOverlay 1 2)              == 2" $
        vertexCount (disjointOverlay 1 2 :: AAE)         == 2
  test "'edgeCount' (disjointOverlay 1 2)                == 0" $
        edgeCount (disjointOverlay 1 2 :: AAE)           == 0

  test "'isEmpty' (disjointConnect x y)                  == 'isEmpty' x && 'isEmpty' y" $ \x y ->
        isEmpty (disjointConnect x y :: AAE)             == (isEmpty x && isEmpty y)
  test "'hasVertex' (Left z) (disjointConnect x y)       == 'hasVertex' z x" $ \x y z ->
        hasVertex (Left z) (disjointConnect x y :: AAE)  == hasVertex z x
  test "'hasVertex' (Right z) (disjointConnect x y)      == 'hasVertex' z y" $ \x y z ->
        hasVertex (Right z) (disjointConnect x y :: AAE) == hasVertex z y
  test "'vertexCount' (disjointConnect x y)              >= 'vertexCount' x" $ \x y ->
        vertexCount (disjointConnect x y :: AAE)         >= vertexCount x
  test "'vertexCount' (disjointConnect x y)              == 'vertexCount' x + 'vertexCount' y" $ \x y ->
        vertexCount (disjointConnect x y :: AAE)         == vertexCount x + vertexCount y
  test "'edgeCount' (disjointConnect x y)                >= 'edgeCount' x" $ \x y ->
        edgeCount (disjointConnect x y :: AAE)           >= edgeCount x
  test "'edgeCount' (disjointConnect x y)                >= 'edgeCount' y" $ \x y ->
        edgeCount (disjointConnect x y :: AAE)           >= edgeCount y
  test "'edgeCount' (disjointConnect x y)                >= 'vertexCount' x * 'vertexCount' y" $ \x y ->
        edgeCount (disjointConnect x y :: AAE)           >= vertexCount x * vertexCount y
  test "'edgeCount' (disjointConnect x y)                == 'vertexCount' x * 'vertexCount' y + 'edgeCount' x + 'edgeCount' y" $ \x y ->
        edgeCount (disjointConnect x y :: AAE)           == vertexCount x * vertexCount y + edgeCount x + edgeCount y
  test "'vertexCount' (disjointConnect 1 2)              == 2" $
        vertexCount (disjointConnect 1 2 :: AAE)         == 2
  test "'edgeCount' (disjointConnect 1 2)                == 1" $
        edgeCount (disjointConnect 1 2 :: AAE)           == 1

  putStrLn "\n=====AcyclicAdjacencyMap transitiveClosure====="

  test "transitiveClosure empty                           == empty" $
        transitiveClosure (empty :: AAI)                  == empty
  test "transitiveClosure (vertex x)                      == vertex x" $ \x ->
        transitiveClosure (vertex x :: AAI)               == vertex x
  test "transitiveClosure (1 * 2 + 2 * 3)                 == 1 * 2 + 2 * 3 + 1 * 3" $
        transitiveClosure (1 * 2 + 2 * 3 :: AAI)          == 1 * 2 + 2 * 3 + 1 * 3
  test "transitiveClosure . transitiveClosure             == transitiveClosure" $ \x ->
       (transitiveClosure . transitiveClosure $ x :: AAI) == transitiveClosure x

  putStrLn "\n=====AcyclicAdjacencyMap box====="

  test "edgeList (box (1 * 2) (3 * 4))                 == [((1,3),(1,4)),((1,3),(2,3)),((1,4),(2,4)),((2,3),(2,4))]" $
        edgeList (box (1 * 2 :: AAI) (3 * 4 :: AAI))   == [((1,3),(1,4)),((1,3),(2,3)),((1,4),(2,4)),((2,3),(2,4))]
  test "edgeList (box (1 + 2) (3 + 4))                 == []" $
        edgeList (box (1 + 2 :: AAI) (3 + 4 :: AAI))   == []
  test "vertexList (box (1 + 2) (3 + 4))               == [(1,3),(1,4),(2,3),(2,4)]" $
        vertexList (box (1 + 2 :: AAI) (3 + 4 :: AAI)) == [(1,3),(1,4),(2,3),(2,4)]

  putStrLn "\n=====AcyclicAdjacencyMap topsort====="

  test "topSort (1)                == [1]" $
        topSort (1 :: AAI)         == [1]
  test "topSort (1 * 2 * 3)        == [1,2,3]" $
        topSort (1 * 2 * 3 :: AAI) == [1,2,3]

  putStrLn "\n=====AcyclicAdjacencyMap fromGraph primitive====="

  test "fromGraph (<) (2 * 1)         == 1 + 2" $
        fromGraph (<) (2 * 1)         == (1 + 2 :: AAI)
  test "fromGraph (<) (1 * 2)         == 1 * 2" $
        fromGraph (<) (1 * 2)         == (1 * 2 :: AAI)
  test "fromGraph (<) (1 * 2 + 2 * 1) == 1 * 2" $
        fromGraph (<) (1 * 2 + 2 * 1) == (1 * 2 :: AAI)

  putStrLn "\n=====AcyclicAdjacencyMap graph transformation====="

  test "removeVertex x ('vertex' x)                  == 'empty'" $ \x ->
        removeVertex x (vertex x :: AAI)             == empty
  test "removeVertex 1 ('vertex' 2)                  == 'vertex' 2" $
        removeVertex 1 (vertex 2 :: AAI)             == vertex 2
  test "removeVertex 1 (1 * 2)                       == 'vertex' 2" $
        removeVertex 1 (1 * 2 :: AAI)                == vertex 2
  test "removeVertex x . removeVertex x              == removeVertex x" $ \x y ->
        (removeVertex x . removeVertex x $ y :: AAI) == removeVertex x y

  test "removeEdge 1 2 (1 * 2)                       == (1 + 2)" $
        removeEdge 1 2 (1 * 2 :: AAI)                == (1 + 2)
  test "removeEdge x y . removeEdge x y              == removeEdge x y" $ \x y z ->
        (removeEdge x y . removeEdge x y $ z :: AAI) == removeEdge x y z
  test "removeEdge x y . 'removeVertex' x            == 'removeVertex' x" $ \x y z ->
        (removeEdge x y . removeVertex x $ z :: AAI) == removeVertex x z
  test "removeEdge 1 2 (1 * 2 + 3 * 4)               == 1 + 2 + 3 * 4" $
        removeEdge 1 2 (1 * 2 + 3 * 4 :: AAI)        == 1 + 2 + 3 * 4

  test "induce ('const' True) x          == x" $ \x ->
        induce (const True) (x :: AAI)   == x
  test "induce ('const' False) x         == 'empty'" $ \x ->
        induce (const False) (x :: AAI)  == empty
  test "induce (/= x)                    == 'removeVertex' x" $ \x y ->
        induce (/= x) y                  == (removeVertex x y :: AAI)
  test "induce p . induce q              == induce (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) y ->
        (induce p . induce q $ y :: AAI) == induce (\x -> p x && q x) y

  test "transpose 'empty'                   == 'empty'" $
        transpose empty                     == (empty :: AAI)
  test "transpose ('vertex' x)              == 'vertex' x" $ \x ->
        transpose (vertex x :: AAI)         == vertex x
  test "transpose . transpose               == id" $ \x ->
        (transpose . transpose $ x :: AAI)  == id x
  test "'edgeList' . transpose              == 'Data.List.sort' . 'map' 'Data.Tuple.swap' . 'edgeList'" $ \x ->
        (edgeList . transpose $ (x :: AAI)) == (List.sort . List.map Tuple.swap . edgeList $ x)

  putStrLn "\n=====AcyclicAdjacencyMap properties====="

  test "isEmpty empty                              == True" $
        isEmpty (empty :: AAI)                     == True
  test "isEmpty (vertex x)                         == False" $ \x ->
        isEmpty (vertex x :: AAI)                  == False
  test "isEmpty (removeVertex x $ vertex x)        == True" $ \x ->
        isEmpty (removeVertex x $ vertex x :: AAI) == True
  test "isEmpty (removeEdge 1 2 $ 1 * 2)           == False" $
        isEmpty (removeEdge 1 2 $ 1 * 2 :: AAI)    == False

  test "edgeSet empty             == Set.empty" $
        edgeSet (empty :: AAI)    == Set.empty
  test "edgeSet (vertex x)        == Set.empty" $ \x ->
        edgeSet (vertex x :: AAI) == Set.empty
  test "edgeSet (1 * 2)           == Set.singleton (1,2)" $
        edgeSet (1 * 2 :: AAI)    == Set.singleton (1,2)

  test "vertexSet empty               == Set.empty" $
        vertexSet (empty :: AAI)      == Set.empty
  test "vertexSet . vertex            == Set.singleton" $ \x ->
        vertexSet (vertex x :: AAI)   == Set.singleton x
  test "vertexSet . vertices          == Set.fromList" $ \x ->
        vertexSet (vertices x :: AAI) == Set.fromList x

  test "vertexCount empty                                     == 0" $
        vertexCount (empty :: AAI)                            == 0
  test "vertexCount (vertex x)                                == 1" $ \x ->
        vertexCount (vertex x :: AAI)                         == 1
  test "vertexCount                                           == length . vertexList" $ \x ->
        vertexCount (x :: AAI)                                == (List.length . vertexList $ x)
  test "vertexCount x < vertexCount y                         == > x < y" $ \x y ->
        not (vertexCount (x :: AAI) < vertexCount y) || x < y

  test "edgeCount empty             == 0" $
        edgeCount (empty :: AAI)    == 0
  test "edgeCount (vertex x)        == 0" $ \x ->
        edgeCount (vertex x :: AAI) == 0
  test "edgeCount (1 * 2)           == 1" $
        edgeCount (1 * 2 :: AAI)    == 1
  test "edgeCount                   == length . edgeList" $ \x ->
        edgeCount (x :: AAI)        == (List.length . edgeList $ x)

  test "adjacencyList empty             == []" $
        adjacencyList (empty :: AAI)    == []
  test "adjacencyList (vertex x)        == [(x, [])]" $ \x ->
        adjacencyList (vertex x :: AAI) == [(x, [])]
  test "adjacencyList (1 * 2)           == [(1, [2]), (2, [])]" $
        adjacencyList (1 * 2 :: AAI)    == [(1, [2]), (2, [])]

  test "hasEdge x y empty                           == False" $ \x y ->
        hasEdge x y (empty :: AAI)                  == False
  test "hasEdge x y (vertex z)                      == False" $ \x y z ->
        hasEdge x y (vertex z :: AAI)               == False
  test "hasEdge 1 2 (1 * 2)                         == True" $
        hasEdge 1 2 (1 * 2 :: AAI)                  == True
  test "hasEdge x y . removeEdge x y                == const False" $ \x y z ->
        (hasEdge x y . removeEdge x y $ (z :: AAI)) == const False z
  test "hasEdge x y                                 == elem (x,y) . edgeList" $ \x y z ->
        (hasEdge x y $ (z :: AAI))                  == (elem (x,y) . edgeList $ z)

  test "hasVertex x empty                           == False" $ \x ->
        hasVertex x (empty :: AAI)                  == False
  test "hasVertex x (vertex x)                      == True" $ \x ->
        hasVertex x (vertex x :: AAI)               == True
  test "hasVertex 1 (vertex 2)                      == False" $
        hasVertex 1 (vertex 2 :: AAI)               == False
  test "hasVertex x . removeVertex x                == const False" $ \x z ->
        (hasVertex x . removeVertex x $ (z :: AAI)) == const False z

  test "edgeList empty             == []" $
        edgeList (empty :: AAI)    == []
  test "edgeList (vertex 5)        == []" $
        edgeList (vertex 5 :: AAI) == []
  test "edgeList (1 * 2)           == [(1,2)]" $
        edgeList (1 * 2 :: AAI)    == [(1,2)]
  test "edgeList (2 * 1)           == []" $
        edgeList (2 * 1 :: AAI)    == []

  test "vertexList empty                         == []" $
        vertexList (empty :: AAI)                == []
  test "vertexList (vertex 1)                    == [1]" $
        vertexList (vertex 1 :: AAI)             == [1]
  test "vertexList (vertices ([1, 3, 2]))        == List.sort [1, 3, 2] == [1,2,3]" $
        vertexList (vertices ([1, 3, 2]) :: AAI) == List.sort [1, 3, 2]


  putStrLn "\n============ AcyclicAdjacencyMap scc ============"

  test "scc AM.empty         == empty" $
        scc (AM.empty :: AI) == empty

  test "scc (AM.vertex x) == vertex (NonEmpty.vertex x)" $ \(x :: Int) ->
        scc (AM.vertex x) == vertex (NonEmpty.vertex x)

  test "scc (edge 1 1)          == vertex (NonEmpty.edge 1 1)" $
        scc (AM.edge 1 1 :: AI) == vertex (NonEmpty.edge 1 1)

  test "vertexList (scc (edge 1 2))          == [NonEmpty.vertex 1,NonEmpty.vertex 2]" $
        vertexList (scc (AM.edge 1 2 :: AI)) == [NonEmpty.vertex 1,NonEmpty.vertex 2]
  test "edgeList (scc (edge 1 2))            == [(NonEmpty.vertex 1,NonEmpty.vertex 2)]" $
        edgeList (scc (AM.edge 1 2 :: AI))   == [(NonEmpty.vertex 1,NonEmpty.vertex 2)]

  test "scc (AM.circuit (1:xs)) == vertex (NonEmpty.circuit1 (1 :| xs))" $ \(xs :: [Int]) ->
        scc (AM.circuit (1:xs)) == vertex (NonEmpty.circuit1 (1 :| xs))

  test "vertexList (scc (3 * 1 * 4 * 1 * 5))       == <correct result>" $
        vertexList (scc (3 * 1 * 4 * 1 * 5 :: AI)) == [NonEmpty.vertex 3,NonEmpty.vertex 5,NonEmpty.clique1 (1 :| [4,1])]
  test "edgeList (scc (3 * 1 * 4 * 1 * 5))         == <correct result>" $
        edgeList (scc (3 * 1 * 4 * 1 * 5 :: AI))   == [(NonEmpty.vertex 3,NonEmpty.vertex 5),(NonEmpty.vertex 3,NonEmpty.clique1 (1 :| [4,1])),(NonEmpty.clique1 (1 :| [4,1]),NonEmpty.vertex 5)]

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
        edgeCount (edges x :: AAI) == (length . filter (uncurry (<)) . List.nub $ x)
  test "edgeList . edges           == filter (uncurry (<)) . Data.List.nub . Data.List.sort" $ \x ->
        edgeList (edges x :: AAI)  == (filter (uncurry (<)) . List.nub . List.sort $ x)

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
