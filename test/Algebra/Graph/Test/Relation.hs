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

import Algebra.Graph hiding (edges)
import Algebra.Graph.Relation.Internal
import Algebra.Graph.Test

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testRelation :: IO ()
testRelation = do
    putStrLn "\n============ Relation ============"
    quickCheck (axioms :: GraphTestsuite (Relation Int))

    test "Consistency of arbitraryRelation" $ \(m :: Relation Int) ->
        consistent m

    test "Consistency of fromAdjacencyList" $ \(xs :: [(Int, [Int])]) ->
        consistent (fromAdjacencyList xs)

    test "Overlay of fromAdjacencyList" $ \(xs :: [(Int, [Int])]) ys ->
        fromAdjacencyList xs `overlay` fromAdjacencyList ys == fromAdjacencyList (xs ++ ys)

    test "Weak inverse of edges" $ \(xs :: [(Int, Int)]) ->
        edgeList (edges xs) == nubOrd (sort xs)

    putStrLn "============ ReflexiveRelation ============"
    quickCheck (reflexiveAxioms :: GraphTestsuite (ReflexiveRelation Int))

    putStrLn "============ SymmetricRelation ============"
    quickCheck $ sizeLimit
        (undirectedAxioms :: GraphTestsuite (SymmetricRelation Int))

    putStrLn "============ TransitiveRelation ============"
    quickCheck $ sizeLimit
        (transitiveAxioms :: GraphTestsuite (TransitiveRelation Int))

    test "Path equals clique" $ sizeLimit $ \xs ->
        path xs == (clique xs :: TransitiveRelation Int)

    putStrLn "============ PreorderRelation ============"
    quickCheck $ sizeLimit
        (preorderAxioms :: GraphTestsuite (PreorderRelation Int))

    test "Path equals clique" $ sizeLimit $ \xs ->
        path xs == (clique xs :: PreorderRelation Int)
