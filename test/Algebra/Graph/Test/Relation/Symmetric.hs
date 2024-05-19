-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Relation.Symmetric
-- Copyright  : (c) Andrey Mokhov 2016-2024
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Relation.Symmetric".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Relation.Symmetric (
    -- * Testsuite
    testSymmetricRelation
    ) where

import Algebra.Graph.Relation.Symmetric
import Algebra.Graph.Test
import Algebra.Graph.Test.API (toIntAPI, symmetricRelationAPI)
import Algebra.Graph.Test.Generic

import qualified Algebra.Graph.Relation as R

tPoly :: Testsuite Relation Ord
tPoly = ("Symmetric.Relation.", symmetricRelationAPI)

t :: TestsuiteInt Relation
t = fmap toIntAPI tPoly

type RI  = R.Relation Int
type SRI = Relation Int

testSymmetricRelation :: IO ()
testSymmetricRelation = do
    putStrLn "\n============ Symmetric.Relation ============"
    test "Axioms of undirected graphs" $ size10 $ undirectedAxioms @SRI

    testConsistent    t
    testSymmetricShow t

    putStrLn $ "\n============ Symmetric.Relation.toSymmetric ============"
    test "toSymmetric (edge 1 2)         == edge 1 2" $
          toSymmetric (R.edge 1 2)       == edge 1 (2 :: Int)

    test "toSymmetric . fromSymmetric    == id" $ \(x :: SRI) ->
          (toSymmetric . fromSymmetric) x == id x

    test "fromSymmetric    . toSymmetric == symmetricClosure" $ \(x :: RI) ->
          (fromSymmetric . toSymmetric) x == R.symmetricClosure x

    test "vertexCount      . toSymmetric == vertexCount" $ \(x :: RI) ->
          vertexCount (toSymmetric x) == R.vertexCount x

    test "(*2) . edgeCount . toSymmetric >= edgeCount" $ \(x :: RI) ->
          ((*2) . edgeCount . toSymmetric) x >= R.edgeCount x

    putStrLn $ "\n============ Symmetric.Relation.fromSymmetric ============"
    test "fromSymmetric (edge 1 2)    == edges [(1,2), (2,1)]" $
          fromSymmetric (edge 1 2)    == R.edges [(1,2), (2,1 :: Int)]

    test "vertexCount . fromSymmetric == vertexCount" $ \(x :: SRI) ->
          (R.vertexCount . fromSymmetric) x == vertexCount x

    test "edgeCount   . fromSymmetric <= (*2) . edgeCount" $ \(x :: SRI) ->
          (R.edgeCount . fromSymmetric) x <= ((*2) . edgeCount) x

    testSymmetricBasicPrimitives t
    testSymmetricIsSubgraphOf    t
    testSymmetricToGraph         t
    testSymmetricGraphFamilies   t
    testSymmetricTransformations t
    testInduceJust               tPoly
