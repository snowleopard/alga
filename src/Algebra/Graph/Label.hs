-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Label
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides basic data types and type classes for representing edge
-- labels in edge-labelled graphs, e.g. see "Algebra.Graph.Labelled".
--
-----------------------------------------------------------------------------
module Algebra.Graph.Label (
    -- * Semirings and dioids
    Semiring (..), zero, (<+>), StarSemiring (..), Dioid,

    -- * Data types for edge labels
    NonNegative (..), Count, count, getCount, Distance, distance, getDistance,
    Capacity, capacity, getCapacity, RE (..)
    ) where

import Prelude ()
import Prelude.Compat

import Data.Monoid (Any (..), Monoid (..), Sum (..))
import Data.Semigroup (Min (..), Max (..), Semigroup (..))

{-| A /semiring/ extends a commutative 'Monoid' with an additional operation
'<.>' that acts similarly to multiplication over the underlying (additive)
monoid and has 'one' as the identity. Instances of this type class must satisfy
the following semiring laws:

    * Commutativity:

        > x <+> y == y <+> x

    * Associativity:

        > x <+> (y <+> z) == (x <+> y) <+> z

    * Identity:

        > x <+> 'zero' == x

    * Associativity:

        > x <.> (y <.> z) == (x <.> y) <.> z

    * Identity:

        > x <.> 'one' == x
        > 'one' <.> x == x

    * Annihilating 'zero':

        > x <.> 'zero' == 'zero'
        > 'zero' <.> x == 'zero'

    * Distributivity:

        > x <.> (y <+> z) == x <.> y <+> x <.> z
        > (x <+> y) <.> z == x <.> z <+> y <.> z
-}
class (Monoid a, Semigroup a) => Semiring a where
    one   :: a
    (<.>) :: a -> a -> a

{-| A /star semiring/ is a 'Semiring' with an additional unary operator 'star'
satisfying the following two laws:

    * Star:

        > star a = one <+> a <.> star a
        > star a = one <+> star a <.> a
-}
class Semiring a => StarSemiring a where
    star :: a -> a

{-| A /dioid/ is an /idempotent semiring/, i.e. it satisfies the following law
in addition to the 'Semiring' laws:

    * Idempotence:

        > x <+> x == x
-}
class Semiring a => Dioid a

-- | An alias for 'mempty'.
zero :: Monoid a => a
zero = mempty

-- | An alias for '<>'.
(<+>) :: Semigroup a => a -> a -> a
(<+>) = (<>)

infixr 6 <+>
infixr 7 <.>

instance Semiring Any where
    one             = Any True
    Any x <.> Any y = Any (x && y)

instance StarSemiring Any where
    star _ = Any True

instance Dioid Any

-- | A non-negative value that can be 'Finite' or 'Infinite'.
data NonNegative a = Finite a | Infinite deriving (Eq, Ord, Show)

instance Num a => Bounded (NonNegative a) where
    minBound = Finite 0
    maxBound = Infinite

instance Num a => Num (NonNegative a) where
    fromInteger = Finite . fromInteger

    Infinite + _        = Infinite
    _        + Infinite = Infinite
    Finite x + Finite y = Finite (x + y)

    Infinite * _        = Infinite
    _        * Infinite = Infinite
    Finite x * Finite y = Finite (x * y)

    negate _ = error "NonNegative values cannot be negated"

    signum (Finite x) = Finite (signum x)
    signum _          = 1

    abs = id

-- | A /distance/ is a non-negative value that can be 'Finite' or 'Infinite'.
-- Distances form a 'Dioid' as follows:
-- * 'zero' = 'distance' 'Infinite'
-- * '<+>'  = 'min'
-- * 'one'  = 'distance' 0
-- * '<.>'  = '+'
newtype Distance a = Distance (Min (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup, Show)

distance :: NonNegative a -> Distance a
distance = Distance . Min

getDistance :: Distance a -> NonNegative a
getDistance (Distance (Min x)) = x

instance (Num a, Ord a) => Semiring (Distance a) where
    one   = 0
    (<.>) = (+)

instance (Num a, Ord a) => StarSemiring (Distance a) where
    star _ = one

instance (Num a, Ord a) => Dioid (Distance a)

-- | A /capacity/ is a non-negative value that can be 'Finite' or 'Infinite'.
-- Capacities form a 'Dioid' as follows:
-- * 'zero' = 'capacity' 0
-- * '<+>'  = 'max'
-- * 'one'  = 'capacity' 'Infinite'
-- * '<.>'  = 'min'
newtype Capacity a = Capacity (Max (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup, Show)

capacity :: NonNegative a -> Capacity a
capacity = Capacity . Max

getCapacity :: Capacity a -> NonNegative a
getCapacity (Capacity (Max x)) = x

instance (Num a, Ord a) => Semiring (Capacity a) where
    one   = capacity Infinite
    (<.>) = min

instance (Num a, Ord a) => StarSemiring (Capacity a) where
    star _ = one

instance (Num a, Ord a) => Dioid (Capacity a)

-- | A /count/ is a non-negative value that can be 'Finite' or 'Infinite'.
-- Counts form a 'Semiring' as follows:
-- * 'zero' = 'count' 0
-- * '<+>'  = '+'
-- * 'one'  = 'count' 1
-- * '<.>'  = '*'
newtype Count a = Count (Sum (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup, Show)

count :: NonNegative a -> Count a
count = Count . Sum

getCount :: Count a -> NonNegative a
getCount (Count (Sum x)) = x

instance Num a => Semiring (Count a) where
    one   = 1
    (<.>) = (*)

instance (Eq a, Num a) => StarSemiring (Count a) where
    star x | x == zero = one
           | otherwise = count Infinite

type Path a = [(a, a)]

data ShortestPath e a = ShortestPath (Distance e) (Path a)

instance (Ord a, Ord e) => Semigroup (ShortestPath e a) where
    ShortestPath d1 p1 <> ShortestPath d2 p2
        | d1 < d2 || (d1 == d2 && p1 < p2) = ShortestPath d1 p1
        | otherwise                        = ShortestPath d2 p2

instance (Num e, Ord a, Ord e) => Monoid (ShortestPath e a) where
    mempty  = ShortestPath mempty []
    mappend = (<>)

instance (Num e, Ord a, Ord e) => Semiring (ShortestPath e a) where
    one = ShortestPath one mempty

    ShortestPath d1 p1 <.> ShortestPath d2 p2 = ShortestPath (d1 <.> d2) (p1 ++ p2)

instance (Num e, Ord a, Ord e) => StarSemiring (ShortestPath e a) where
    star _ = one

instance (Num e, Ord a, Ord e) => Dioid (ShortestPath e a)

data RE a = Var a
          | RE a :+: RE a
          | RE a :*: RE a
          | Star (RE a)
          | None
          | Unit
          deriving (Eq, Ord, Show)

infixl 6 :+:
infixl 7 :*:

instance Semigroup (RE a) where
    None   <> x      = x
    x      <> None   = x
    Unit   <> Unit   = Unit
    Unit   <> Star x = Star x
    Star x <> Unit   = Star x
    x      <> y      = x :+: y

instance Monoid (RE a) where
    mempty  = None
    mappend = (<>)

instance Semiring (RE a) where
    one = Unit

    Unit <.> x    = x
    x    <.> Unit = x
    None <.> _    = None
    _    <.> None = None
    x    <.> y    = x :*: y

instance StarSemiring (RE a) where
    star None     = Unit
    star Unit     = Unit
    star (Star x) = star x
    star x        = Star x
