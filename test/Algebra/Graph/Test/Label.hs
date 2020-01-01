{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Label
-- License    : MIT (see the file LICENSE)
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Label".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Label (
  -- * Testsuite
  testLabel
  ) where

import Algebra.Graph.Test hiding (NonNegative)
import Algebra.Graph.Test.Generic (size10)
import Algebra.Graph.Label
import Data.Monoid

type UnaryFn a = a -> a
type BinaryFn a = a -> a -> a
type Plus a = BinaryFn a
type Mult a = BinaryFn a
type Star a = UnaryFn a
type Identity a = a
type Zero a = a
type One a = a
type Annihilator a = a

associative :: Eq a => BinaryFn a -> a -> a -> a -> Property
associative (<>) a b c = (a <> b) <> c == a <> (b <> c) // "Associative"

commutative :: Eq a => BinaryFn a -> a -> a -> Property
commutative (<>) a b = a <> b == b <> a // "Commutative"

idempotent :: Eq a => BinaryFn a -> a -> Property
idempotent (<>) a = a <> a == a // "Idempotent"

annihilator :: Eq a => BinaryFn a -> Annihilator a -> a -> Property
annihilator (<>) z a = conjoin
  [ a <> z == z // "Left" 
  , z <> a == z // "Right" ] // "Annihilator"

closure :: Eq a => Plus a -> Mult a -> One a -> Star a -> a -> Property
closure (+) (*) o s a = conjoin
  [ s a == o + (a * s a) // "Left"
  , s a == o + (s a * a) // "Right" ] // "Closure"

leftDistributive :: Eq a => Plus a -> Mult a -> a -> a -> a -> Property
leftDistributive (+) (*) a b c = a * (b + c) == (a * b) + (a * c) // "Left distributive"

rightDistributive :: Eq a => Plus a -> Mult a -> a -> a -> a -> Property
rightDistributive (+) (*) a b c = (a + b) * c == (a * c) + (b * c) // "Right distributive"

distributive :: Eq a => Plus a -> Mult a -> a -> a -> a -> Property
distributive p m a b c = conjoin
  [ leftDistributive p m a b c
  , rightDistributive p m a b c ] // "Distributive"

identity :: Eq a => BinaryFn a -> Identity a -> a -> Property
identity (<>) e a = conjoin
  [ a <> e == a // "Left"
  , e <> a == a // "Right" ] // "Identity"

semigroup :: Eq a => BinaryFn a -> a -> a -> a -> Property
semigroup f a b c = associative f a b c // "Semigroup"

monoid :: Eq a => BinaryFn a -> Identity a -> a -> a -> a -> Property
monoid f e a b c = conjoin
  [ associative f a b c
  , identity f e a ] // "Monoid"

commutativeMonoid :: Eq a => BinaryFn a -> Identity a -> a -> a -> a -> Property
commutativeMonoid f e a b c = conjoin
  [ monoid f e a b c
  , commutative f a b ] // "Commutative monoid"

leftNearRing :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
leftNearRing p z m o a b c = conjoin
  [ commutativeMonoid p z a b c
  , monoid m o a b c
  , leftDistributive p m a b c
  , annihilator m z a ] // "Left near ring"

rightNearRing :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
rightNearRing p z m o a b c = conjoin
  [ commutativeMonoid p z a b c
  , monoid m o a b c
  , rightDistributive p m a b c
  , annihilator m z a ] // "Right near ring"

semiring :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
semiring p z m o a b c = conjoin
  [ commutativeMonoid p z a b c
  , monoid m o a b c
  , distributive p m a b c
  , annihilator m z a ] // "Semiring"

dioid :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
dioid p z m o a b c = conjoin
  [ semiring p z m o a b c
  , idempotent p a ] // "Dioid"

starSemiring :: Eq a => Plus a -> Zero a -> Mult a -> One a -> Star a -> a -> a -> a -> Property
starSemiring p z m o s a b c = conjoin
  [ semiring p z m o a b c
  , closure p m o s a ] // "Star semiring"

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
    putStrLn "\n============ Any ============"
    test "Semiring"     $ \(a :: Any) b c -> testSemiring a b c
    test "StarSemiring" $ \(a :: Any) b c -> testStarSemiring a b c
    test "Dioid"        $ \(a :: Any) b c -> testDioid a b c

    putStrLn "\n============ Distance Int ============"
    test "Semiring"     $ \(a :: Distance Int) b c -> testSemiring a b c
    test "StarSemiring" $ \(a :: Distance Int) b c -> testStarSemiring a b c
    test "Dioid"        $ \(a :: Distance Int) b c -> testDioid a b c

    putStrLn "\n============ Capacity Int ============"
    test "Semiring"     $ \(a :: Capacity Int) b c -> testSemiring a b c
    test "StarSemiring" $ \(a :: Capacity Int) b c -> testStarSemiring a b c
    test "Dioid"        $ \(a :: Capacity Int) b c -> testDioid a b c

    putStrLn "\n============ Minimum (Path Int) ============"
    test "LeftNearRing" $ \(a :: Minimum (Path Int)) b c -> testLeftNearRing a b c

    putStrLn "\n============ PowerSet (Path Int) ============"
    test "Semiring" $ size10 $ \(a :: PowerSet (Path Int)) b c -> testSemiring a b c
    test "Dioid"    $ size10 $ \(a :: PowerSet (Path Int)) b c -> testDioid a b c

    putStrLn "\n============ Count Int ============"
    test "Semiring"     $ \(a :: Count Int) b c -> testSemiring a b c
    test "StarSemiring" $ \(a :: Count Int) b c -> testStarSemiring a b c
