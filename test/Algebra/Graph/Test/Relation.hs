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
t = testsuite "Relation." C.empty

type RI = Relation Int

testRelation :: IO ()
testRelation = do
    putStrLn "\n============ Relation ============"
    test "Axioms of graphs" $ size10 (axioms :: GraphTestsuite RI)

    test "Consistency of arbitraryRelation" $ \(m :: RI) ->
        consistent m

    testShow            t
    testBasicPrimitives t
    testIsSubgraphOf    t
    testToGraph         t
    testGraphFamilies   t
    testTransformations t
    testRelational      t

    putStrLn "\n============ ReflexiveRelation ============"
    test "Axioms of reflexive graphs" $ size10
        (reflexiveAxioms :: GraphTestsuite (ReflexiveRelation Int))

    putStrLn "\n============ SymmetricRelation ============"
    test "Axioms of undirected graphs" $ size10
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
    test "Axioms of transitive graphs" $ size10
        (transitiveAxioms :: GraphTestsuite (TransitiveRelation Int))

    test "path xs == (clique xs :: TransitiveRelation Int)" $ size10 $ \xs ->
          C.path xs == (C.clique xs :: TransitiveRelation Int)

    putStrLn "\n============ PreorderRelation ============"
    test "Axioms of preorder graphs" $ size10
        (preorderAxioms :: GraphTestsuite (PreorderRelation Int))

    test "path xs == (clique xs :: PreorderRelation Int)" $ size10 $ \xs ->
          C.path xs == (C.clique xs :: PreorderRelation Int)
