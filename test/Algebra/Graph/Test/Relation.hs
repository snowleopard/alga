{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Relation
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'Relation'.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Relation (
    -- * Testsuite
    testRelation
  ) where

import Algebra.Graph.Relation
import Algebra.Graph.Relation.Internal
import Algebra.Graph.Test

import qualified Algebra.Graph.Class as C
import qualified Data.Set            as Set

type RI = Relation Int
type II = Int -> Int

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testRelation :: IO ()
testRelation = do
    putStrLn "\n============ Relation ============"
    test "Axioms of graphs" $ sizeLimit
        (axioms :: GraphTestsuite (Relation Int))

    test "Consistency of arbitraryRelation" $ \(m :: RI) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \xs ->
        consistent (fromAdjacencyList xs :: RI)

    putStrLn "\n============ Show ============"
    test "show (empty     :: Relation Int) == \"empty\"" $
          show (empty     :: Relation Int) == "empty"

    test "show (1         :: Relation Int) == \"vertex 1\"" $
          show (1         :: Relation Int) == "vertex 1"

    test "show (1 + 2     :: Relation Int) == \"vertices [1,2]\"" $
          show (1 + 2     :: Relation Int) == "vertices [1,2]"

    test "show (1 * 2     :: Relation Int) == \"edge 1 2\"" $
          show (1 * 2     :: Relation Int) == "edge 1 2"

    test "show (1 * 2 * 3 :: Relation Int) == \"edges [(1,2),(1,3),(2,3)]\"" $
          show (1 * 2 * 3 :: Relation Int) == "edges [(1,2),(1,3),(2,3)]"

    test "show (1 * 2 + 3 :: Relation Int) == \"graph [1,2,3] [(1,2)]\"" $
          show (1 * 2 + 3 :: Relation Int) == "graph [1,2,3] [(1,2)]"

    putStrLn "\n============ isEmpty ============"
    test "isEmpty empty      == True" $
          isEmpty (empty::RI)== True

    test "isEmpty (vertex x) == False" $ \(x :: Int) ->
          isEmpty (vertex x) == False

    putStrLn "\n============ hasVertex ============"
    test "hasVertex x empty      == False" $ \(x :: Int) ->
          hasVertex x empty      == False

    test "hasVertex x (vertex x) == True" $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    putStrLn "\n============ hasEdge ============"
    test "hasEdge x y empty      == False" $ \(x :: Int) y ->
          hasEdge x y empty      == False

    test "hasEdge x y (vertex z) == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z) == False

    test "hasEdge x y (edge x y) == True" $ \(x :: Int) y ->
          hasEdge x y (edge x y) == True

    putStrLn "\n============ vertexSet ============"
    test "vertexSet empty         == Set.empty" $
          vertexSet (empty :: RI) == Set.empty

    test "vertexSet (vertex x)    == Set.singleton x" $ \(x :: Int) ->
          vertexSet (vertex x)    == Set.singleton x

    test "vertexSet (vertices xs) == Set.fromList xs" $ \(xs :: [Int]) ->
          vertexSet (vertices xs) == Set.fromList xs

    test "vertexSet (clique xs)   == Set.fromList xs" $ \(xs :: [Int]) ->
          vertexSet (clique xs)   == Set.fromList xs

    putStrLn "\n============ preset ============"
    test "preset x empty      == Set.empty" $ \(x :: Int) ->
          preset x empty      == Set.empty

    test "preset x (vertex x) == Set.empty" $ \(x :: Int) ->
          preset x (vertex x) == Set.empty

    test "preset 1 (edge 1 2) == Set.empty" $
          preset 1 (edge 1 2) ==(Set.empty :: Set.Set Int)

    test "preset y (edge x y) == Set.fromList [x]" $ \(x :: Int) y ->
          preset y (edge x y) == Set.fromList [x]

    putStrLn "\n============ postset ============"
    test "postset x empty      == Set.empty" $ \(x :: Int) ->
          postset x empty      == Set.empty

    test "postset x (vertex x) == Set.empty" $ \(x :: Int) ->
          postset x (vertex x) == Set.empty

    test "postset x (edge x y) == Set.fromList [y]" $ \(x :: Int) y ->
          postset x (edge x y) == Set.fromList [y]

    test "postset 2 (edge 1 2) == Set.empty" $
          postset 2 (edge 1 2) ==(Set.empty :: Set.Set Int)

    putStrLn "\n============ reflexiveClosure ============"
    test "reflexiveClosure empty      == empty" $
          reflexiveClosure empty      ==(empty :: RI)

    test "reflexiveClosure (vertex x) == edge x x" $ \(x :: Int) ->
          reflexiveClosure (vertex x) == edge x x

    putStrLn "\n============ symmetricClosure ============"

    test "symmetricClosure empty      == empty" $
          symmetricClosure empty      ==(empty :: RI)

    test "symmetricClosure (vertex x) == vertex x" $ \(x :: Int) ->
          symmetricClosure (vertex x) == vertex x

    test "symmetricClosure (edge x y) == edges [(x, y), (y, x)]" $ \(x :: Int) y ->
          symmetricClosure (edge x y) == edges [(x, y), (y, x)]

    putStrLn "\n============ transitiveClosure ============"
    test "transitiveClosure empty           == empty" $
          transitiveClosure empty           ==(empty :: RI)

    test "transitiveClosure (vertex x)      == vertex x" $ \(x :: Int) ->
          transitiveClosure (vertex x)      == vertex x

    test "transitiveClosure (path $ nub xs) == clique (nub $ xs)" $ \(xs :: [Int]) ->
          transitiveClosure (path $ nubOrd xs) == clique (nubOrd $ xs)

    putStrLn "\n============ preorderClosure ============"
    test "preorderClosure empty           == empty" $
          preorderClosure empty           ==(empty :: RI)

    test "preorderClosure (vertex x)      == edge x x" $ \(x :: Int) ->
          preorderClosure (vertex x)      == edge x x

    test "preorderClosure (path $ nub xs) == reflexiveClosure (clique $ nub xs)" $ \(xs :: [Int]) ->
          preorderClosure (path $ nubOrd xs) == reflexiveClosure (clique $ nubOrd xs)

    putStrLn "\n============ gmap ============"
    test "gmap f empty      == empty" $ \(apply -> f :: II) ->
          gmap f empty      == empty

    test "gmap f (vertex x) == vertex (f x)" $ \(apply -> f :: II) x ->
          gmap f (vertex x) == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f :: II) x y ->
          gmap f (edge x y) == edge (f x) (f y)

    test "gmap id           == id" $ \x ->
          gmap id x         == (x :: RI)

    test "gmap f . gmap g   == gmap (f . g)" $ \(apply -> f :: II) (apply -> g :: II) x ->
         (gmap f . gmap g) x== gmap (f . g) x

    putStrLn "\n============ edgeList ============"
    test "edgeList empty          == []" $
          edgeList (empty :: RI)  == []

    test "edgeList (vertex x)     == []" $ \(x :: Int) ->
          edgeList (vertex x)     == []

    test "edgeList (edge x y)     == [(x,y)]" $ \(x :: Int) y ->
          edgeList (edge x y)     == [(x,y)]

    test "edgeList (star 2 [1,3]) == [(2,1), (2,3)]" $
          edgeList (star 2 [1,3]) == [(2,1), (2,3 :: Int)]

    test "edgeList . edges        == nub . sort" $ \(xs :: [(Int, Int)]) ->
         (edgeList . edges) xs    ==(nubOrd . sort) xs

    putStrLn "\n============ edges ============"
    test "edges []         == empty" $
          edges []         ==(empty :: RI)

    test "edges [(x, y)]   == edge x y" $ \(x :: Int) y ->
          edges [(x, y)]   == edge x y

    putStrLn "\n============ fromAdjacencyList ============"
    test "fromAdjacencyList []                                  == empty" $
          fromAdjacencyList []                                  == (empty :: RI)

    test "fromAdjacencyList [(x, [])]                           == vertex x" $ \(x :: Int) ->
          fromAdjacencyList [(x, [])]                           == vertex x

    test "fromAdjacencyList [(x, [y])]                          == edge x y" $ \(x :: Int) y ->
          fromAdjacencyList [(x, [y])]                          == edge x y

    test "overlay (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyList xs) (fromAdjacencyList ys) ==(fromAdjacencyList (xs ++ ys) :: RI)

    putStrLn "\n============ ReflexiveRelation ============"
    test "Axioms of reflexive graphs" $ sizeLimit
        (reflexiveAxioms :: GraphTestsuite (ReflexiveRelation Int))

    putStrLn "\n============ SymmetricRelation ============"
    test "Axioms of undirected graphs" $ sizeLimit
        (undirectedAxioms :: GraphTestsuite (SymmetricRelation Int))

    putStrLn "\n============ TransitiveRelation ============"
    test "Axioms of transitive graphs" $ sizeLimit
        (transitiveAxioms :: GraphTestsuite (TransitiveRelation Int))

    test "path xs == (clique xs :: TransitiveRelation Int)" $ sizeLimit $ \xs ->
          C.path xs == (C.clique xs :: TransitiveRelation Int)

    putStrLn "\n============ PreorderRelation ============"
    test "Axioms of preorder graphs" $ sizeLimit
        (preorderAxioms :: GraphTestsuite (PreorderRelation Int))

    test "path xs == (clique xs :: PreorderRelation Int)" $ sizeLimit $ \xs ->
          C.path xs == (C.clique xs :: PreorderRelation Int)
