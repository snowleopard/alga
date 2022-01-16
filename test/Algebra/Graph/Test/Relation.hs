-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Relation
-- Copyright  : (c) Andrey Mokhov 2016-2022
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
import Algebra.Graph.Relation.Preorder
import Algebra.Graph.Relation.Reflexive
import Algebra.Graph.Relation.Transitive
import Algebra.Graph.Test
import Algebra.Graph.Test.API (toIntAPI, relationAPI)
import Algebra.Graph.Test.Generic

import qualified Algebra.Graph.Class as C

tPoly :: Testsuite Relation Ord
tPoly = ("Relation.", relationAPI)

t :: TestsuiteInt Relation
t = fmap toIntAPI tPoly

type RI = Relation Int

testRelation :: IO ()
testRelation = do
    putStrLn "\n============ Relation ============"
    test "Axioms of graphs" $ size10 $ axioms @RI

    testConsistent      t
    testShow            t
    testBasicPrimitives t
    testIsSubgraphOf    t
    testToGraph         t
    testGraphFamilies   t
    testTransformations t
    testRelational      t
    testInduceJust      tPoly

    putStrLn "\n============ ReflexiveRelation ============"
    test "Axioms of reflexive graphs" $ size10 $
        reflexiveAxioms @(ReflexiveRelation Int)

    putStrLn "\n============ TransitiveRelation ============"
    test "Axioms of transitive graphs" $ size10 $
        transitiveAxioms @(TransitiveRelation Int)

    test "path xs == (clique xs :: TransitiveRelation Int)" $ size10 $ \xs ->
          C.path xs == (C.clique xs :: TransitiveRelation Int)

    putStrLn "\n============ PreorderRelation ============"
    test "Axioms of preorder graphs" $ size10 $
        preorderAxioms @(PreorderRelation Int)

    test "path xs == (clique xs :: PreorderRelation Int)" $ size10 $ \xs ->
          C.path xs == (C.clique xs :: PreorderRelation Int)
