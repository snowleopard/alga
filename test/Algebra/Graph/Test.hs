{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Basic testsuite infrastructure.
-----------------------------------------------------------------------------
module Algebra.Graph.Test (
    module Data.List,
    module Data.List.Extra,
    module Test.QuickCheck,
    module Test.QuickCheck.Function,

    GraphTestsuite, (//), axioms, theorems, undirectedAxioms, reflexiveAxioms,
    transitiveAxioms, preorderAxioms, test,
    ) where

import Data.List (sort)
import Data.List.Extra (nubOrd)
import Prelude hiding ((+), (*))
import System.Exit (exitFailure)
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Function

import Algebra.Graph.Class
import Algebra.Graph.Test.Arbitrary ()

test :: Testable a => String -> a -> IO ()
test str p = do
    result <- quickCheckWithResult (stdArgs { chatty = False }) p
    if isSuccess result
        then putStrLn $ "OK: " ++ str
        else do
            putStrLn $ "\nTest failure:\n    " ++ str ++ "\n"
            putStrLn $ output result
            exitFailure

(+) :: Graph g => g -> g -> g
(+) = overlay

(*) :: Graph g => g -> g -> g
(*) = connect

(//) :: Testable prop => prop -> String -> Property
p // s = label s $ counterexample ("Failed when checking '" ++ s ++ "'") p

infixl 1 //
infixl 6 +
infixl 7 *

type GraphTestsuite g = g -> g -> g -> Property

axioms :: (Eq g, Graph g) => GraphTestsuite g
axioms x y z = conjoin
    [       x + y == y + x                      // "Overlay commutativity"
    , x + (y + z) == (x + y) + z                // "Overlay associativity"
    ,   empty * x == x                          // "Left connect identity"
    ,   x * empty == x                          // "Right connect identity"
    , x * (y * z) == (x * y) * z                // "Connect associativity"
    , x * (y + z) == x * y + x * z              // "Left distributivity"
    , (x + y) * z == x * z + y * z              // "Right distributivity"
    ,   x * y * z == x * y + x * z + y * z      // "Decomposition" ]

theorems :: (Ord g, Graph g) => GraphTestsuite g
theorems x y z = conjoin
    [     x + empty == x                        // "Overlay identity"
    ,         x + x == x                        // "Overlay idempotence"
    , x + y + x * y == x * y                    // "Absorption"
    ,     x * y * z == x * y + x * z + y * z
                     + x + y + z + empty        // "Full decomposition"
    ,         x * x == x * x * x                // "Connect saturation"
    ,         empty <= x                        // "Lower bound"
    ,             x <= x + y                    // "Overlay order"
    ,         x + y <= x * y                    // "Overlay-connect order" ]

undirectedAxioms :: (Eq g, Graph g) => GraphTestsuite g
undirectedAxioms x y z = conjoin
    [ axioms x y z
    , x * y == y * x                            // "Connect commutativity" ]

reflexiveAxioms :: (Eq g, Graph g, Arbitrary (Vertex g), Show (Vertex g)) => GraphTestsuite g
reflexiveAxioms x y z = conjoin
    [ axioms x y z
    , forAll arbitrary (\v -> vertex v `asTypeOf` x == vertex v * vertex v)
                                                // "Vertex self-loop" ]

transitiveAxioms :: (Eq g, Graph g) => GraphTestsuite g
transitiveAxioms x y z = conjoin
    [ axioms x y z
    , y == empty || x * y * z == x * y + y * z  // "Closure" ]

preorderAxioms :: (Eq g, Graph g, Arbitrary (Vertex g), Show (Vertex g)) => GraphTestsuite g
preorderAxioms x y z = conjoin
    [ axioms x y z
    , forAll arbitrary (\v -> vertex v `asTypeOf` x == vertex v * vertex v)
                                                // "Vertex self-loop"
    , y == empty || x * y * z == x * y + y * z  // "Closure" ]
