{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Label
-- Copyright  : (c) Andrey Mokhov 2016-2025
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Label".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Label (
  -- * Testsuite
  testLabel
  ) where

import Algebra.Graph.Test
import Algebra.Graph.Label
import Data.Monoid

type Unary a          = a -> a
type Binary a         = a -> a -> a
type Additive a       = Binary a
type Multiplicative a = Binary a
type Star a           = Unary a
type Identity a       = a
type Zero a           = a
type One a            = a

associative :: Eq a => Binary a -> a -> a -> a -> Property
associative (<>) a b c = (a <> b) <> c == a <> (b <> c) // "Associative"

commutative :: Eq a => Binary a -> a -> a -> Property
commutative (<>) a b = a <> b == b <> a // "Commutative"

idempotent :: Eq a => Binary a -> a -> Property
idempotent (<>) a = a <> a == a // "Idempotent"

annihilatingZero :: Eq a => Binary a -> Zero a -> a -> Property
annihilatingZero (<>) z a = conjoin
    [ a <> z == z // "Left"
    , z <> a == z // "Right" ] // "Annihilating zero"

closure :: Eq a => Additive a -> Multiplicative a -> One a -> Star a -> a -> Property
closure (+) (*) o s a = conjoin
    [ s a == o + (a * s a) // "Left"
    , s a == o + (s a * a) // "Right" ] // "Closure"

leftDistributive :: Eq a => Additive a -> Multiplicative a -> a -> a -> a -> Property
leftDistributive (+) (*) a b c =
    a * (b + c) == (a * b) + (a * c) // "Left distributive"

rightDistributive :: Eq a => Additive a -> Multiplicative a -> a -> a -> a -> Property
rightDistributive (+) (*) a b c =
    (a + b) * c == (a * c) + (b * c) // "Right distributive"

distributive :: Eq a => Additive a -> Multiplicative a -> a -> a -> a -> Property
distributive p m a b c = conjoin
    [ leftDistributive p m a b c
    , rightDistributive p m a b c ] // "Distributive"

identity :: Eq a => Binary a -> Identity a -> a -> Property
identity (<>) e a = conjoin
    [ a <> e == a // "Left"
    , e <> a == a // "Right" ] // "Identity"

semigroup :: Eq a => Binary a -> a -> a -> a -> Property
semigroup f a b c = associative f a b c // "Semigroup"

monoid :: Eq a => Binary a -> Identity a -> a -> a -> a -> Property
monoid f e a b c = conjoin
    [ semigroup f a b c
    , identity f e a ] // "Monoid"

commutativeMonoid :: Eq a => Binary a -> Identity a -> a -> a -> a -> Property
commutativeMonoid f e a b c = conjoin
    [ monoid f e a b c
    , commutative f a b ] // "Commutative monoid"

leftNearRing :: Eq a => Additive a -> Zero a -> Multiplicative a -> One a -> a -> a -> a -> Property
leftNearRing (+) z (*) o a b c = conjoin
    [ commutativeMonoid (+) z a b c
    , monoid (*) o a b c
    , leftDistributive (+) (*) a b c
    , annihilatingZero (*) z a ] // "Left near ring"

semiring :: Eq a => Additive a -> Zero a -> Multiplicative a -> One a -> a -> a -> a -> Property
semiring (+) z (*) o a b c = conjoin
    [ commutativeMonoid (+) z a b c
    , monoid (*) o a b c
    , distributive (+) (*) a b c
    , annihilatingZero (*) z a ] // "Semiring"

dioid :: Eq a => Additive a -> Zero a -> Multiplicative a -> One a -> a -> a -> a -> Property
dioid (+) z (*) o a b c = conjoin
    [ semiring (+) z (*) o a b c
    , idempotent (+) a ] // "Dioid"

starSemiring :: Eq a => Additive a -> Zero a -> Multiplicative a -> One a -> Star a -> a -> a -> a -> Property
starSemiring (+) z (*) o s a b c = conjoin
    [ semiring (+) z (*) o a b c
    , closure (+) (*) o s a ] // "Star semiring"

testLeftNearRing :: (Eq a, Semiring a) => a -> a -> a -> Property
testLeftNearRing = leftNearRing (<+>) zero (<.>) one

testSemiring :: (Eq a, Semiring a) => a -> a -> a -> Property
testSemiring = semiring (<+>) zero (<.>) one

testDioid :: (Eq a, Dioid a) => a -> a -> a -> Property
testDioid = dioid (<+>) zero (<.>) one

testStarSemiring :: (Eq a, StarSemiring a) => a -> a -> a -> Property
testStarSemiring = starSemiring (<+>) zero (<.>) one star

testLabel :: IO ()
testLabel = do
    putStrLn "\n============ Graph.Label ============"
    putStrLn "\n============ Any: instances ============"
    test "Semiring"     $ testSemiring     @Any
    test "StarSemiring" $ testStarSemiring @Any
    test "Dioid"        $ testDioid        @Any

    putStrLn "\n============ Distance Int: instances ============"
    test "Semiring"     $ testSemiring     @(Distance Int)
    test "StarSemiring" $ testStarSemiring @(Distance Int)
    test "Dioid"        $ testDioid        @(Distance Int)

    putStrLn "\n============ Capacity Int: instances ============"
    test "Semiring"     $ testSemiring     @(Capacity Int)
    test "StarSemiring" $ testStarSemiring @(Capacity Int)
    test "Dioid"        $ testDioid        @(Capacity Int)

    putStrLn "\n============ Minimum (Path Int): instances ============"
    test "LeftNearRing" $ testLeftNearRing @(Minimum (Path Int))

    putStrLn "\n============ PowerSet (Path Int): instances ============"
    test "Semiring" $ size10 $ testSemiring @(PowerSet (Path Int))
    test "Dioid"    $ size10 $ testDioid    @(PowerSet (Path Int))

    putStrLn "\n============ Count Int: instances ============"
    test "Semiring"     $ testSemiring     @(Count Int)
    test "StarSemiring" $ testStarSemiring @(Count Int)
