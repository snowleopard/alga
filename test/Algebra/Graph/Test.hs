{-# LANGUAGE RankNTypes #-}
module Algebra.Graph.Test (
    module Data.List,
    module Data.List.Extra,
    module Test.QuickCheck,
    module Test.QuickCheck.Function,

    GraphTestsuite, axioms, theorems, undirectedAxioms, reflexiveAxioms,
    transitiveAxioms, preorderAxioms, test,
    ) where

import Data.List (sort)
import Data.List.Extra (nubOrd)
import Prelude hiding ((+), (*), (<=))
import Test.QuickCheck
import Test.QuickCheck.Function

import Algebra.Graph.Class
import Algebra.Graph.Test.Arbitrary ()

test :: Testable a => String -> a -> IO ()
test str p = putStr (str ++ " : ") >> quickCheck p

(+) :: Graph g => g -> g -> g
(+) = overlay

(*) :: Graph g => g -> g -> g
(*) = connect

(<=) :: (Eq g, Graph g) => g -> g -> Bool
(<=) = isSubgraphOf

(//) :: Testable prop => prop -> String -> Property
p // s = label s $ counterexample ("Failed when checking '" ++ s ++ "'") p

infixl 1 //
infixl 4 <=
infixl 6 +
infixl 7 *

type GraphTestsuite g = (Eq g, Graph g) => g -> g -> g -> Property

axioms :: GraphTestsuite g
axioms x y z = conjoin
    [       x + y == y + x                      // "Overlay commutativity"
    , x + (y + z) == (x + y) + z                // "Overlay associativity"
    ,   empty * x == x                          // "Left connect identity"
    ,   x * empty == x                          // "Right connect identity"
    , x * (y * z) == (x * y) * z                // "Connect associativity"
    , x * (y + z) == x * y + x * z              // "Left distributivity"
    , (x + y) * z == x * z + y * z              // "Right distributivity"
    ,   x * y * z == x * y + x * z + y * z      // "Decomposition" ]

theorems :: GraphTestsuite g
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

undirectedAxioms :: GraphTestsuite g
undirectedAxioms x y z = conjoin
    [ axioms x y z
    , x * y == y * x                            // "Connect commutativity" ]

reflexiveAxioms :: (Arbitrary (Vertex g), Show (Vertex g)) => GraphTestsuite g
reflexiveAxioms x y z = conjoin
    [ axioms x y z
    , forAll arbitrary (\v -> vertex v `asTypeOf` x == vertex v * vertex v)
                                                // "Vertex self-loop" ]

transitiveAxioms :: Eq g => GraphTestsuite g
transitiveAxioms x y z = conjoin
    [ axioms x y z
    , y == empty || x * y * z == x * y + y * z  // "Closure" ]

preorderAxioms :: (Arbitrary (Vertex g), Eq g, Show (Vertex g)) => GraphTestsuite g
preorderAxioms x y z = conjoin
    [ axioms x y z
    , forAll arbitrary (\v -> vertex v `asTypeOf` x == vertex v * vertex v)
                                                // "Vertex self-loop"
    , y == empty || x * y * z == x * y + y * z  // "Closure" ]
