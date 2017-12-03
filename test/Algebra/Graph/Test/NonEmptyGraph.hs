-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.NonEmptyGraph
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.NonEmpty".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.NonEmptyGraph (
    -- * Testsuite
    testNonEmptyGraph
  ) where

import Data.List.NonEmpty (NonEmpty (..))

import Algebra.Graph.NonEmpty
import Algebra.Graph.Test hiding (axioms, theorems)

import qualified Data.List.NonEmpty as NonEmpty

type G = NonEmptyGraph Int

axioms :: G -> G -> G -> Property
axioms x y z = conjoin
    [       x + y == y + x                      // "Overlay commutativity"
    , x + (y + z) == (x + y) + z                // "Overlay associativity"
    , x * (y * z) == (x * y) * z                // "Connect associativity"
    , x * (y + z) == x * y + x * z              // "Left distributivity"
    , (x + y) * z == x * z + y * z              // "Right distributivity"
    ,   x * y * z == x * y + x * z + y * z      // "Decomposition" ]

theorems :: G -> G -> Property
theorems x y = conjoin
    [         x + x == x                        // "Overlay idempotence"
    , x + y + x * y == x * y                    // "Absorption"
    ,         x * x == x * x * x                // "Connect saturation"
    ,             x <= x + y                    // "Overlay order"
    ,         x + y <= x * y                    // "Overlay-connect order" ]
  where
    (<=) = isSubgraphOf
    infixl 4 <=

testNonEmptyGraph :: IO ()
testNonEmptyGraph = do
    putStrLn "\n============ Graph.NonEmpty ============"
    test "Axioms of graphs"   axioms
    test "Theorems of graphs" theorems

    putStrLn $ "\n============ Graph.NonEmpty.vertex ============"
    test "hasVertex x (vertex x) == True" $ \(x :: Int) ->
          hasVertex x (vertex x) == True

    test "hasVertex 1 (vertex 2) == False" $
          hasVertex 1 (vertex 2) == False

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \(x :: Int) ->
          edgeCount   (vertex x) == 0

    putStrLn "\n============ Graph.NonEmpty.(===) ============"
    test "    x === x         == True" $ \(x :: G) ->
             (x === x)        == True

    test "x + y === x + y     == True" $ \(x :: G) y ->
         (x + y === x + y)    == True

    test "1 + 2 === 2 + 1     == False" $
         (1 + 2 === 2 + (1 :: G)) == False

    test "x + y === x * y     == False" $ \(x :: G) y ->
         (x + y === x * y)    == False
