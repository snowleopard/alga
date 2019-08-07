{-# LANGUAGE ViewPatterns #-}
-------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Acyclic.Labelled.AdjacencyMap
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Acyclic.Labelled.AdjacencyMap".
-------------------------------------------------------------

module Algebra.Graph.Test.Acyclic.Labelled.AdjacencyMap (
  testAcyclicLabelledAdjacencyMap 
  ) where
    
import Data.Monoid

import Algebra.Graph.Acyclic.Labelled.AdjacencyMap
import Algebra.Graph.Acyclic.Labelled.AdjacencyMap.Algorithm
import Algebra.Graph.Test
import Algebra.Graph.Label

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM

type S = Sum Int
type D = Distance Int

type LAS = AdjacencyMap S Int
type LAD = AdjacencyMap D Int
type LCD = AdjacencyMap D Char

-- TODO: Switch to using generic tests.
-- TODO: Clean the code, use proper and standerdized formatting.
testAcyclicLabelledAdjacencyMap :: IO ()
testAcyclicLabelledAdjacencyMap = do

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.consistent ======="
  test "consistent empty                            == True" $
        consistent (empty :: LAS)                   == True
  test "consistent (vertex x)                       == True" $ \x ->
        consistent (vertex x :: LAS)                == True
  test "consistent (vertices x)                     == True" $ \x ->
        consistent (vertices x :: LAS)              == True
  test "consistent (transitiveClosure x)            == True" $ \x ->
        consistent (transitiveClosure x :: LAD)     == True
  test "consistent (transpose x)                    == True" $ \x ->
        consistent (transpose x :: LAS)             == True
  test "consistent (removeVertex x g)               == True" $ \x g ->
        consistent (removeVertex x (g :: LAS))      == True
  test "consistent (removeEdge x y g)               == True" $ \x y g ->
        consistent (removeEdge x y (g :: LAS))      == True
  test "consistent (emap h g)                       == True" $ \(apply -> h) g ->
        consistent (emap (fmap h::S->S) (g :: LAS)) == True
  test "consistent (induce f x)                     == True" $ \(apply -> f) g ->
        consistent (induce f (g :: LAS))            == True
  test "consistent (induceJust x)                   == True" $ \x ->
        consistent (induceJust x :: LAS)            == True
  test "consistent (1 + 2)                          == True" $
        consistent (1 + 2 :: LAD)                   == True
  test "consistent (1 * 2 + 2 * 3)                  == True" $
        consistent (1 * 2 + 2 * 3 :: LAD)           == True

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.empty ======="
  test "isEmpty     empty          == True" $
        isEmpty     (empty :: LAS) == True
  test "hasVertex x empty          == False" $ \x ->
        hasVertex x (empty :: LAS) == False
  test "vertexCount empty          == 0" $
        vertexCount (empty :: LAS) == 0
  test "edgeCount   empty          == 0" $
        edgeCount   (empty :: LAS) == 0

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.vertex ======="
  test "isEmpty     (vertex x)        == False" $ \x ->
        isEmpty     (vertex x :: LAS) == False
  test "hasVertex x (vertex x)        == True" $ \x ->
        hasVertex x (vertex x :: LAS) == True
  test "vertexCount (vertex x)        == 1" $ \x ->
        vertexCount (vertex x :: LAS) == 1
  test "edgeCount   (vertex x)        == 0" $ \x ->
        edgeCount   (vertex x :: LAS) == 0

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.vertices ======="
  test "vertices []                     == empty" $
        vertices []                     == (empty :: LAS)
  test "vertices [x]                    == vertex x" $ \x ->
        (vertices [x] :: LAS)           == vertex x
  test "hasVertex x . vertices          == elem x" $ \x y ->
        hasVertex x (vertices y :: LAS) == elem x y
  test "vertexCount . vertices          == length . Data.List.nub" $ \x ->
        vertexCount (vertices x :: LAS) == (length . List.nub $ x)
  test "vertexSet   . vertices          == Set.fromList" $ \x ->
        vertexSet (vertices x :: LAS)   == Set.fromList x

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.isSubgraphOf ======="
  test "isSubgraphOf empty      x                     == True" $ \x ->
        isSubgraphOf (empty :: LAD)      x            == True
  test "isSubgraphOf (vertex x) empty                 == False" $ \x ->
        isSubgraphOf (vertex x) (empty :: LAD)        == False
  test "isSubgraphOf x y                              == > x <= y" $ \(x :: LAD) (y :: LAD) ->
        (not $ isSubgraphOf x y) || (x <= y)

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.isEmpty ======="
  test "isEmpty empty                              == True" $
        isEmpty (empty :: LAS)                     == True
  test "isEmpty (vertex x)                         == False" $ \x ->
        isEmpty (vertex x :: LAS)                  == False
  test "isEmpty (removeVertex x $ vertex x)        == True" $ \x ->
        isEmpty (removeVertex x $ vertex x :: LAS) == True

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.hasVertex ======="
  test "hasVertex x empty                           == False" $ \x ->
        hasVertex x (empty :: LAS)                  == False
  test "hasVertex x (vertex x)                      == True" $ \x ->
        hasVertex x (vertex x :: LAS)               == True
  test "hasVertex 1 (vertex 2)                      == False" $
        hasVertex 1 (vertex 2 :: LAS)               == False
  test "hasVertex x . removeVertex x                == const False" $ \x y ->
        (hasVertex x . removeVertex x $ (y :: LAS)) == const False y

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.hasEdge ======="
  test "hasEdge x y empty                           == False" $ \x y ->
        hasEdge x y (empty :: LAS)                  == False
  test "hasEdge x y (vertex z)                      == False" $ \x y z ->
        hasEdge x y (vertex z :: LAS)               == False
  test "hasEdge x y . removeEdge x y                == const False" $ \x y z ->
        (hasEdge x y . removeEdge x y $ (z :: LAS)) == const False z
  test "hasEdge x y                                 == not . null . filter (\\(_,ex,ey) -> ex == x && ey == y) . edgeList" $ \x y z ->
        hasEdge x y (z :: LAS)                      == (not . null . filter (\(_,ex,ey) -> ex == x && ey == y) . edgeList $ z)

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.edgeLabel ======="
  test "edgeLabel x y empty             == zero" $ \x y ->
        edgeLabel x y (empty :: LAS)    == zero
  test "edgeLabel x y (vertex z)        == zero" $ \x y z ->
        edgeLabel x y (vertex z :: LAS) == zero

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.vertexCount ======="
  test "vertexCount empty             ==  0" $
        vertexCount (empty :: LAS)    ==  0
  test "vertexCount (vertex x)        ==  1" $ \x ->
        vertexCount (vertex x :: LAS) ==  1
  test "vertexCount                   ==  length . vertexList" $ \x ->
        vertexCount (x :: LAS)        ==  (length . vertexList $ x)
  test "vertexCount x < vertexCount y ==> x < y" $ \x y ->
        (not $ vertexCount x < vertexCount y) || ((x :: LAS) < (y :: LAS))

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.edgeCount ======="
  test "edgeCount empty             == 0" $
        edgeCount (empty :: LAS)    == 0
  test "edgeCount (vertex x)        == 0" $ \x ->
        edgeCount (vertex x :: LAS) == 0
  test "edgeCount                   == length . edgeList" $ \x ->
        edgeCount (x :: LAS)        == (length . edgeList $ x)

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.vertexList ======="
  test "vertexList empty               == []" $
        vertexList (empty :: LAS)      == []
  test "vertexList (vertex x)          == [x]" $ \x ->
        vertexList (vertex x :: LAS)   == [x]
  test "vertexList . vertices          == Data.List.nub . Data.List.sort" $ \x ->
        vertexList (vertices x :: LAS) == (List.nub . List.sort $ x)

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.edgeList ======="
  test "edgeList empty             == []" $
        edgeList (empty :: LAS)    == []
  test "edgeList (vertex x)        == []" $ \x ->
        edgeList (vertex x :: LAS) == []

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.vertexSet ======="
  test "vertexSet empty               == Set.empty" $
        vertexSet (empty :: LAS)      == Set.empty
  test "vertexSet . vertex            == Set.singleton" $ \x ->
        vertexSet (vertex x :: LAS)   == Set.singleton x
  test "vertexSet . vertices          == Set.fromList" $ \x ->
        vertexSet (vertices x :: LAS) == Set.fromList x

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.edgeSet ======="
  test "edgeSet empty             == Set.empty" $
        edgeSet (empty :: LAS)    == Set.empty
  test "edgeSet (vertex x)        == Set.empty" $ \x ->
        edgeSet (vertex x :: LAS) == Set.empty

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.removeVertex ======="
  test "removeVertex x (vertex x)             == empty" $ \x ->
        removeVertex x (vertex x :: LAS)      == empty
  test "removeVertex 1 (vertex 2)             == vertex 2" $
        removeVertex 1 (vertex 2 :: LAS)      == vertex 2
  test "removeVertex x . removeVertex x       == removeVertex x" $ \x y ->
        (removeVertex x . removeVertex x $ y) == removeVertex x (y :: LAS)

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.removeEdge ======="
  test "removeEdge x y . removeEdge x y       == removeEdge x y" $ \x y z ->
        (removeEdge x y . removeEdge x y $ z) == removeEdge x y (z :: LAS)
  test "removeEdge x y . removeVertex x       == removeVertex x" $ \x y z ->
        (removeEdge x y . removeVertex x $ z) == removeVertex x (z :: LAS)

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.transpose ======="
  test "transpose empty             == empty" $
        transpose (empty :: LAS)    == empty
  test "transpose (vertex x)        == vertex x" $ \x ->
        transpose (vertex x :: LAS) == vertex x
  test "transpose . transpose       == id" $ \x ->
        (transpose . transpose $ x) == id (x :: LAS)

  putStrLn "\n======= Acyclic.Labelled.AdjacencyMap.emap ======="
  test "emap h empty           == empty" $ \(k :: S) ->
        let h = (k*)
        in emap h empty        == (empty :: LAS)
  test "emap h (vertex x)      == vertex x" $ \(k :: S) x ->
        let h = (k*)
        in emap h (vertex x)   == (vertex x :: LAS)
  test "emap id                == id" $ \x ->
        emap id x              == (id x :: LAS)
  test "emap g . emap h        == emap (g . h)" $ \(k :: S) (l :: S) x ->
        let h = (k*)
            g = (l*)
        in (emap g . emap h) x == (emap (g . h) x :: LAS)

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.induce ======="
  test "induce (const True ) x               == x" $ \x ->
        induce (const True ) x               == (x :: LAS)
  test "induce (const False) x               == empty" $ \x ->
        induce (const False) (x :: LAS)      == empty
  test "induce (/= x)                        == removeVertex x" $ \x y ->
        induce (/= x) (y :: LAS)             == removeVertex x y
  test "induce p . induce q                  == induce (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) y ->
        (induce p . induce q) (y :: LAS)     == induce (\x -> p x && q x) y
  test "isSubgraphOf (induce p x) x          == True" $ \(apply -> p) x ->
        isSubgraphOf (induce p x) (x :: LAD) == True

  putStrLn $ "\n======= Acyclic.Labelled.AdjacencyMap.induceJust ======="
  test "induceJust (vertex Nothing)                == empty" $
        induceJust (vertex (Nothing :: Maybe Int)) == (empty :: LAS)

  putStrLn "\n======= Acyclic.Labelled.AdjacencyMap.transitiveClosure ======="
  test "transitiveClosure empty                   == empty" $
        transitiveClosure empty                   == (empty :: LAD)
  test "transitiveClosure (vertex x)              == vertex x" $ \x ->
        transitiveClosure (vertex x)              == (vertex x :: LAD)
  test "transitiveClosure . transitiveClosure     == transitiveClosure" $ \x ->
        (transitiveClosure . transitiveClosure) x == transitiveClosure (x :: LAD)

  putStrLn "\n======= Acyclic.Labelled.Algorithm.fold ======="

  let applyFun4 (Fun _ f) a b c d = f (a, b, c, d)

  test "fold f s empty                   == s" $ \(applyFun4 -> f) s ->
        fold f (s :: Int) (empty :: LAS) == s

  test "fold f s (vertex x)                 == s" $ \(applyFun4 -> f) s x ->
        fold f (s :: Int) (vertex x :: LAS) == s

  test "fold f s (vertices xs)                 == s" $ \(applyFun4 -> f) s xs ->
        fold f (s :: Int) (vertices xs :: LAS) == s

  test "fold (\\e v1 v2 -> flip (++) [(e, v1, v2)]) [] (LAM.edge 5 1 2)                      == [(5, 1, 2)] " $
        fold (\e v1 v2 -> flip (++) [(e, v1, v2)]) [] (toAcyclicOrd $ LAM.edge 5 1 2 :: LAD) == [(5::D, 1::Int, 2::Int)] 

  test "fold (\\e v1 v2 -> flip (++) [(e, v1, v2)]) [] (toAcyclicOrd $ LAM.edges [(5, 2, 3), (0, 1, 2), (6, 1, 3)])       == [(0, 1, 2), (5, 2, 3), (6, 1, 3)]" $
        fold (\e v1 v2 -> flip (++) [(e, v1, v2)]) [] (toAcyclicOrd $ LAM.edges [(5, 2, 3), (0, 1, 2), (6, 1, 3)] :: LAD) == [(0::D, 1::Int, 2::Int), (5, 2, 3), (6, 1, 3)]

  putStrLn "\n======= Acyclic.Labelled.Algorithm.optimumPath ======="

  test "optimumPath (vertex x) x        == Map.fromList [(x, 0)]" $ \x ->
        optimumPath (vertex x :: LAD) x == Map.fromList [(x, 0)]

  test "optimumPath (vertex 'a') 'z'        == Map.fromList [('a', distance infinite)]" $
        optimumPath (vertex 'a' :: LCD) 'z' == Map.fromList [('a', distance infinite)]

  test "optimumPath (toAcyclicOrd $ LAM.edge 2 'a' 'b') 'a'        == Map.fromList [('a', 0), ('b', 2)]" $
        optimumPath (toAcyclicOrd $ LAM.edge (2 :: D) 'a' 'b') 'a' == Map.fromList [('a', 0), ('b', 2)]

  test "optimumPath (toAcyclicOrd $ LAM.edge 2 'a' 'b') 'z'        == Map.fromList [('a', distance infinite), ('b', distance infinite)]" $
        optimumPath (toAcyclicOrd $ LAM.edge (2 :: D) 'a' 'b') 'z' == Map.fromList [('a', distance infinite), ('b', distance infinite)]

  test "optimumPath (vertices ['a', 'b']) 'a'        == Map.fromList [('a', 0), ('b', distance infinite)]" $
        optimumPath (vertices ['a', 'b'] :: LCD) 'a' == Map.fromList [('a', 0), ('b', distance infinite)]

  test "optimumPath (vertices ['a', 'b']) 'z'        == Map.fromList [('a', distance infinite), ('b', distance infinite)]" $
        optimumPath (vertices ['a', 'b'] :: LCD) 'z' == Map.fromList [('a', distance infinite), ('b', distance infinite)]

  test "optimumPath (toAcyclicOrd $ edges [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'z'          == Map.fromList [('a', distance infinite), ('b', distance infinite), ('c', distance infinite)]" $
        optimumPath (toAcyclicOrd $ LAM.edges [(2 :: D, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'z' == Map.fromList [('a', distance infinite), ('b', distance infinite), ('c', distance infinite)]

  test "optimumPath (toAcyclicOrd $ edges [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'a'          == Map.fromList [('a', 0), ('b', 1), ('c', 3)]" $
        optimumPath (toAcyclicOrd $ LAM.edges [(2 :: D, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'a' == Map.fromList [('a', 0 :: D), ('b', 1), ('c', 3)]
