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
module Algebra.Graph.Test.Relation.SymmetricRelation (
    -- * Testsuite
    testSymmetricRelation
  ) where

import Algebra.Graph.Relation.Symmetric
import Algebra.Graph.Relation.InternalDerived
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

import qualified Algebra.Graph.Class as C
import qualified Data.Set            as Set

t :: Testsuite
t = testsuite "Relation.Symmetric." empty

type SRI = SymmetricRelation Int

testSymmetricRelation :: IO ()
testSymmetricRelation = do
    putStrLn "\n============ SymmetricRelation ============"
    test "Axioms of graphs" $ size10 (axioms :: GraphTestsuite SRI)

    test "Consistency of arbitraryRelation" $ \(m :: SRI) ->
         consistent m

    testSymmetricShow            t
    testSymmetricBasicPrimitives t
    testIsSubgraphOf             t
    testSymmetricToGraph         t
    testGraphFamilies            t
    testTransformations          t

    test "Axioms of undirected graphs" $ size10
        (undirectedAxioms :: GraphTestsuite (SymmetricRelation Int))

    test "neighbours x empty      == Set.empty" $ \(x :: Int) ->
          neighbours x C.empty    == Set.empty

    test "neighbours x (vertex x) == Set.empty" $ \(x :: Int) ->
          neighbours x (C.vertex x) == Set.empty

    test "neighbours x (edge x y) == Set.fromList [y]" $ \(x :: Int) y ->
          neighbours x (C.edge x y) == Set.fromList [y]

    test "neighbours y (edge x y) == Set.fromList [x]" $ \(x :: Int) y ->
          neighbours y (C.edge x y) == Set.fromList [x]
