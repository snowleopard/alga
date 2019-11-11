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
associative f a b c = (a `f` b) `f` c == a `f` (b `f` c) // "Associative property"

commutative :: Eq a => BinaryFn a -> a -> a -> Property
commutative f a b = a `f` b == b `f` a // "Commutative property"

idempotent :: Eq a => BinaryFn a -> a -> Property
idempotent f a = a `f` a == a // "Idempotence property"

annihilator :: Eq a => BinaryFn a -> Annihilator a -> a -> Property
annihilator f z a = conjoin
  [ a `f` z == z // "Annihilator property right"
  , z `f` a == z // "Annihilator property left" ]

closure :: Eq a => Plus a -> Mult a -> One a -> Star a -> a -> Property
closure p m o s a = conjoin
  [ s a == o `p` (a `m` s a) // "Star property right"
  , s a == o `p` (s a `m` a) // "Star property left" ]

leftDistributive :: Eq a => Plus a -> Mult a -> a -> a -> a -> Property
leftDistributive p m a b c = a `m` (b `p` c) == (a `m` b) `p` (a `m` c) // "Left distributive"

rightDistributive :: Eq a => Plus a -> Mult a -> a -> a -> a -> Property
rightDistributive p m a b c = (a `p` b) `m` c == (a `m` c) `p` (b `m` c) // "Right distributive"

distributive :: Eq a => Plus a -> Mult a -> a -> a -> a -> Property
distributive p m a b c = conjoin
  [ leftDistributive p m a b c
  , rightDistributive p m a b c ]

identity :: Eq a => BinaryFn a -> Identity a -> a -> Property
identity f e a = conjoin
  [ a `f` e == a // "Identity property right"
  , e `f` a == a // "Identity property left" ]

semigroup :: Eq a => BinaryFn a -> a -> a -> a -> Property
semigroup = associative

monoid :: Eq a => BinaryFn a -> Identity a -> a -> a -> a -> Property
monoid f e a b c = conjoin
  [ associative f a b c
  , identity f e a ]

commutativeMonoid :: Eq a => BinaryFn a -> Identity a -> a -> a -> a -> Property
commutativeMonoid f e a b c = conjoin
  [ monoid f e a b c
  , commutative f a b ]

leftNearRing :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
leftNearRing p z m o a b c = conjoin
  [ commutativeMonoid p z a b c // "Commutative monoid"
  , monoid m o a b c            // "Monoid"
  , leftDistributive p m a b c
  , annihilator m z a ]

rightNearRing :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
rightNearRing p z m o a b c = conjoin
  [ commutativeMonoid p z a b c // "Commutative monoid"
  , monoid m o a b c            // "Monoid"
  , rightDistributive p m a b c
  , annihilator m z a ]

semiring :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
semiring p z m o a b c = conjoin
  [ commutativeMonoid p z a b c // "Commutative monoid"
  , monoid m o a b c            // "Monoid"
  , distributive p m a b c
  , annihilator m z a ]

dioid :: Eq a => Plus a -> Zero a -> Mult a -> One a -> a -> a -> a -> Property
dioid p z m o a b c = conjoin
  [ semiring p z m o a b c
  , idempotent p a ]

starSemiring :: Eq a => Plus a -> Zero a -> Mult a -> One a -> Star a -> a -> a -> a -> Property
starSemiring p z m o s a b c = conjoin
  [ semiring p z m o a b c
  , closure p m o s a ]

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
    {-# SCC "Ann1.Any_Semiring" #-}     test "Semiring"     $ \(a :: Any) b c -> testSemiring a b c
    {-# SCC "Ann1.Any_StarSemiring" #-} test "StarSemiring" $ \(a :: Any) b c -> testStarSemiring a b c
    {-# SCC "Ann1.Any_Dioid" #-}        test "Dioid"        $ \(a :: Any) b c -> testDioid a b c

    putStrLn "\n============ Distance ============"
    {-# SCC "Ann1.Distance_Semiring" #-}     test "Semiring"     $ \(a :: Distance Int) b c -> testSemiring a b c
    {-# SCC "Ann1.Distance_StarSemiring" #-} test "StarSemiring" $ \(a :: Distance Int) b c -> testStarSemiring a b c
    {-# SCC "Ann1.Distance_Dioid" #-}        test "Dioid"        $ \(a :: Distance Int) b c -> testDioid a b c

    putStrLn "\n============ Capacity ============"
    {-# SCC "Ann1.Capacity_Semiring" #-}     test "Semiring"     $ \(a :: Capacity Int) b c -> testSemiring a b c
    {-# SCC "Ann1.Capacity_StarSemiring" #-} test "StarSemiring" $ \(a :: Capacity Int) b c -> testStarSemiring a b c
    {-# SCC "Ann1.Capacity_Dioid" #-}        test "Dioid"        $ \(a :: Capacity Int) b c -> testDioid a b c

    putStrLn "\n============ Minimum ============"
    {-# SCC "Ann1.Minimum_LeftNearRing" #-} test "LeftNearRing" $ \(a :: Minimum (Path Int)) b c -> testLeftNearRing a b c

    putStrLn "\n============ PowerSet ============"
    {-# SCC "Ann1.PowerSet_Semiring" #-} test "Semiring" $ size10 $ \(a :: PowerSet (Path Int)) b c -> testSemiring a b c
    {-# SCC "Ann1.PowerSet_Dioid" #-}    test "Dioid"    $ size10 $ \(a :: PowerSet (Path Int)) b c -> testDioid a b c

    putStrLn "\n============ Count ============"
    {-# SCC "Ann1.Count_Semiring" #-}     test "Semiring"     $ \(a :: Count Int) b c -> testSemiring a b c
    {-# SCC "Ann1.Count_StarSemiring" #-} test "StarSemiring" $ \(a :: Count Int) b c -> testStarSemiring a b c
