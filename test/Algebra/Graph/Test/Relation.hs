-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Relation
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Relation".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Relation (
    -- * Testsuite
    testRelation
  ) where

import Algebra.Graph.Relation
import Algebra.Graph.Relation.Internal
import Algebra.Graph.Relation.Preorder
import Algebra.Graph.Relation.Reflexive
import Algebra.Graph.Relation.Symmetric
import Algebra.Graph.Relation.Transitive
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

import qualified Algebra.Graph.Class as C
import qualified Data.Set            as Set

t :: Testsuite
t = testsuite "Relation." empty

type RI = Relation Int

sizeLimit :: Testable prop => prop -> Property
sizeLimit = mapSize (min 10)

testRelation :: IO ()
testRelation = do
    putStrLn "\n============ Relation ============"
    test "Axioms of graphs" $ sizeLimit (axioms :: GraphTestsuite RI)

    test "Consistency of arbitraryRelation" $ \(m :: RI) ->
        consistent m

    testShow            t
    testBasicPrimitives t
    testIsSubgraphOf    t
    testToGraph         t
    testGraphFamilies   t
    testTransformations t

    putStrLn "\n============ Relation.compose ============"
    test "compose empty            x                == empty" $ \(x :: RI) ->
          compose empty            x                == empty

    test "compose x                empty            == empty" $ \(x :: RI) ->
          compose x                empty            == empty

    test "compose x                (compose y z)    == compose (compose x y) z" $ sizeLimit $ \(x :: RI) y z ->
          compose x                (compose y z)    == compose (compose x y) z

    test "compose (edge y z)       (edge x y)       == edge x z" $ \(x :: Int) y z ->
          compose (edge y z)       (edge x y)       == edge x z

    test "compose (path    [1..5]) (path    [1..5]) == edges [(1,3),(2,4),(3,5)]" $
          compose (path    [1..5]) (path    [1..5]) == edges [(1,3),(2,4),(3,5::Int)]

    test "compose (circuit [1..5]) (circuit [1..5]) == circuit [1,3,5,2,4]" $
          compose (circuit [1..5]) (circuit [1..5]) == circuit [1,3,5,2,4::Int]

    putStrLn "\n============ Relation.reflexiveClosure ============"
    test "reflexiveClosure empty      == empty" $
          reflexiveClosure empty      ==(empty :: RI)

    test "reflexiveClosure (vertex x) == edge x x" $ \(x :: Int) ->
          reflexiveClosure (vertex x) == edge x x

    putStrLn "\n============ Relation.symmetricClosure ============"

    test "symmetricClosure empty      == empty" $
          symmetricClosure empty      ==(empty :: RI)

    test "symmetricClosure (vertex x) == vertex x" $ \(x :: Int) ->
          symmetricClosure (vertex x) == vertex x

    test "symmetricClosure (edge x y) == edges [(x, y), (y, x)]" $ \(x :: Int) y ->
          symmetricClosure (edge x y) == edges [(x, y), (y, x)]

    putStrLn "\n============ Relation.transitiveClosure ============"
    test "transitiveClosure empty           == empty" $
          transitiveClosure empty           ==(empty :: RI)

    test "transitiveClosure (vertex x)      == vertex x" $ \(x :: Int) ->
          transitiveClosure (vertex x)      == vertex x

    test "transitiveClosure (path $ nub xs) == clique (nub $ xs)" $ \(xs :: [Int]) ->
          transitiveClosure (path $ nubOrd xs) == clique (nubOrd xs)

    putStrLn "\n============ Relation.preorderClosure ============"
    test "preorderClosure empty           == empty" $
          preorderClosure empty           ==(empty :: RI)

    test "preorderClosure (vertex x)      == edge x x" $ \(x :: Int) ->
          preorderClosure (vertex x)      == edge x x

    test "preorderClosure (path $ nub xs) == reflexiveClosure (clique $ nub xs)" $ \(xs :: [Int]) ->
          preorderClosure (path $ nubOrd xs) == reflexiveClosure (clique $ nubOrd xs)

    putStrLn "\n============ ReflexiveRelation ============"
    test "Axioms of reflexive graphs" $ sizeLimit
        (reflexiveAxioms :: GraphTestsuite (ReflexiveRelation Int))

    putStrLn "\n============ SymmetricRelation ============"
    test "Axioms of undirected graphs" $ sizeLimit
        (undirectedAxioms :: GraphTestsuite (SymmetricRelation Int))

    putStrLn "\n============ SymmetricRelation.neighbours ============"
    test "neighbours x empty      == Set.empty" $ \(x :: Int) ->
          neighbours x C.empty    == Set.empty

    test "neighbours x (vertex x) == Set.empty" $ \(x :: Int) ->
          neighbours x (C.vertex x) == Set.empty

    test "neighbours x (edge x y) == Set.fromList [y]" $ \(x :: Int) y ->
          neighbours x (C.edge x y) == Set.fromList [y]

    test "neighbours y (edge x y) == Set.fromList [x]" $ \(x :: Int) y ->
          neighbours y (C.edge x y) == Set.fromList [x]

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
