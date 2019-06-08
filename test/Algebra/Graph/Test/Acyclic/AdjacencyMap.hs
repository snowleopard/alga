{-# LANGUAGE ViewPatterns #-}
-------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Acyclic.AdjacencyMap
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Acyclic.AdjacencyMap".
-------------------------------------------------------------

module Algebra.Graph.Test.Acyclic.AdjacencyMap (
  testAcyclicAdjacencyMap 
  ) where

import Algebra.Graph.Acyclic.AdjacencyMap
import Algebra.Graph.Test
import Data.List.NonEmpty hiding (transpose)

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

  test "consistent (1 + 2)                == True" $
        consistent (1 + 2 :: AAI)         == True
  test "consistent (1 * 2 + 2 * 3)        == True" $
        consistent (1 * 2 + 2 * 3 :: AAI) == True

  putStrLn "\n=====AcyclicAdjacencyMap Num instance====="
  test "edgeList    0                 == []" $
        edgeList   (0 :: AAI)         == []
  test "vertexList  0                 == [0]" $
        vertexList (0 :: AAI)         == [0]
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
  test "isEmpty ('disjointOverlay' 'empty' 'empty')             == True" $
        isEmpty (disjointOverlay (empty :: AAI) (empty :: AAI)) == True
  test "isEmpty ('vertex' x)                             == False" $ \x ->
        isEmpty (vertex x :: AAI)                        == False
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


