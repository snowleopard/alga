-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Fold
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Fold" and polymorphic functions defined in
-- "Algebra.Graph.Class".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Fold (
    -- * Testsuite
    testFold
  ) where

import Algebra.Graph.Fold
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

t :: Testsuite
t = testsuite "Fold." (empty :: Fold Int)

-- h :: HTestsuite
-- h = hTestsuite "Fold." (empty :: Fold Int)

type F  = Fold Int

testFold :: IO ()
testFold = do
    putStrLn "\n============ Fold ============"
    test "Axioms of graphs" (axioms :: GraphTestsuite F)

    testShow            t
    testBasicPrimitives t
    -- testToGraph         h
    testIsSubgraphOf    t
    testSize            t
    testProperties      t
    testGraphFamilies   t
    testTransformations t
    testSplitVertex     t
    testBind            t
    testSimplify        t
