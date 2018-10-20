{-# LANGUAGE DeriveFunctor, OverloadedLists #-}
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
    NonNegative, finite, finiteWord, unsafeFinite, infinite, getFinite, Count,
    count, getCount, Distance, distance, getDistance, Capacity, capacity,
    getCapacity, Label (..), RegularExpression
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (liftA2)
import Data.Monoid (Any (..), Monoid (..), Sum (..))
import Data.Semigroup (Min (..), Max (..), Semigroup (..))
import GHC.Exts (IsList (..))

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

-- | A non-negative value that can be 'finite' or 'infinite'. Note: the current
-- implementation of the 'Num' instance raises an error on negative literals
-- and on the 'negate' method.
newtype NonNegative a = NonNegative (Maybe a)
    deriving (Applicative, Functor, Eq, Monad)

instance Ord a => Ord (NonNegative a) where
    compare (NonNegative Nothing ) (NonNegative Nothing ) = EQ
    compare (NonNegative Nothing ) (NonNegative _       ) = GT
    compare (NonNegative _       ) (NonNegative Nothing ) = LT
    compare (NonNegative (Just x)) (NonNegative (Just y)) = compare x y

instance (Num a, Show a) => Show (NonNegative a) where
    show (NonNegative Nothing)  = "infinite"
    show (NonNegative (Just x)) = show x

instance Num a => Bounded (NonNegative a) where
    minBound = unsafeFinite 0
    maxBound = infinite

instance (Num a, Ord a) => Num (NonNegative a) where
    fromInteger x | f < 0     = error "NonNegative values cannot be negative"
                  | otherwise = unsafeFinite f
      where
        f = fromInteger x

    (+) = liftA2 (+)
    (*) = liftA2 (*)

    negate _ = error "NonNegative values cannot be negated"

    signum (NonNegative Nothing) = 1
    signum x                     = signum <$> x

    abs = id

-- | A finite non-negative value or @Nothing@ if the argument is negative.
finite :: (Num a, Ord a) => a -> Maybe (NonNegative a)
finite x | x < 0      = Nothing
         | otherwise  = Just (unsafeFinite x)

-- | A finite 'Word'.
finiteWord :: Word -> NonNegative Word
finiteWord = unsafeFinite

-- | Create a non-negative finite value /unsafely/: the argument is not checked
-- for being non-negative.
unsafeFinite :: a -> NonNegative a
unsafeFinite = NonNegative . Just

-- | The (non-negative) infinite value.
infinite :: NonNegative a
infinite = NonNegative Nothing

-- | Get a finite value or @Nothing@ if the value is infinite.
getFinite :: NonNegative a -> Maybe a
getFinite (NonNegative x) = x

-- | A /distance/ is a non-negative value that can be 'finite' or 'infinite'.
-- Distances form a 'Dioid' as follows:
-- * 'zero' = 'distance' 'infinite'
-- * '<+>'  = 'min'
-- * 'one'  = 'distance' 0
-- * '<.>'  = '+'
newtype Distance a = Distance (Min (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup)

instance Show a => Show (Distance a) where
    show (Distance (Min (NonNegative (Just x)))) = show x
    show _                                       = "distance infinite"

instance (Num a, Ord a) => Semiring (Distance a) where
    one   = 0
    (<.>) = (+)

instance (Num a, Ord a) => StarSemiring (Distance a) where
    star _ = one

instance (Num a, Ord a) => Dioid (Distance a)

-- | A non-negative distance.
distance :: NonNegative a -> Distance a
distance = Distance . Min

-- | Get the value of a distance.
getDistance :: Distance a -> NonNegative a
getDistance (Distance (Min x)) = x

-- | A /capacity/ is a non-negative value that can be 'finite' or 'infinite'.
-- Capacities form a 'Dioid' as follows:
-- * 'zero' = 'capacity' 0
-- * '<+>'  = 'max'
-- * 'one'  = 'capacity' 'infinite'
-- * '<.>'  = 'min'
newtype Capacity a = Capacity (Max (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup)

instance Show a => Show (Capacity a) where
    show (Capacity (Max (NonNegative (Just x)))) = show x
    show _                                       = "capacity infinite"

instance (Num a, Ord a) => Semiring (Capacity a) where
    one   = capacity infinite
    (<.>) = min

instance (Num a, Ord a) => StarSemiring (Capacity a) where
    star _ = one

instance (Num a, Ord a) => Dioid (Capacity a)

-- | A non-negative capacity.
capacity :: NonNegative a -> Capacity a
capacity = Capacity . Max

-- | Get the value of a capacity.
getCapacity :: Capacity a -> NonNegative a
getCapacity (Capacity (Max x)) = x

-- | A /count/ is a non-negative value that can be 'finite' or 'infinite'.
-- Counts form a 'Semiring' as follows:
-- * 'zero' = 'count' 0
-- * '<+>'  = '+'
-- * 'one'  = 'count' 1
-- * '<.>'  = '*'
newtype Count a = Count (Sum (NonNegative a))
    deriving (Bounded, Eq, Monoid, Num, Ord, Semigroup)

instance Show a => Show (Count a) where
    show (Count (Sum (NonNegative (Just x)))) = show x
    show _                                    = "count infinite"

instance (Num a, Ord a) => Semiring (Count a) where
    one   = 1
    (<.>) = (*)

instance (Num a, Ord a) => StarSemiring (Count a) where
    star x | x == zero = one
           | otherwise = count infinite

-- | A non-negative count.
count :: NonNegative a -> Count a
count = Count . Sum

-- | Get the value of a count.
getCount :: Count a -> NonNegative a
getCount (Count (Sum x)) = x

-- | A /path/ is a list of edges.
type Path a = [(a, a)]

-- | A /shortest path/ is a shortest 'Distance' and a corresponding 'Path'.
-- * 'zero' = 'ShortestPath' zero []
-- * 'one'  = 'ShortestPath' one []
-- * '<+>' picks the shortest of the two paths, or the lexicographically smaller
--   one in case of a tie.
-- * '<.>' adds distances and concatenates paths.
data ShortestPath e a = ShortestPath (Distance e) (Path a)

instance (Ord a, Ord e) => Semigroup (ShortestPath e a) where
    ShortestPath d1 p1 <> ShortestPath d2 p2
        | d1 < d2 || (d1 == d2 && p1 < p2) = ShortestPath d1 p1
        | otherwise                        = ShortestPath d2 p2

instance (Num e, Ord a, Ord e) => Monoid (ShortestPath e a) where
    mempty  = ShortestPath mempty []
    mappend = (<>)

instance (Num e, Ord a, Ord e) => Semiring (ShortestPath e a) where
    one = ShortestPath one []

    ShortestPath d1 p1 <.> ShortestPath d2 p2 = ShortestPath (d1 <.> d2) (p1 ++ p2)

instance (Num e, Ord a, Ord e) => StarSemiring (ShortestPath e a) where
    star _ = one

instance (Num e, Ord a, Ord e) => Dioid (ShortestPath e a)

-- | The type of /free labels/ over the underlying set of symbols @a@. This data
-- type is an instance of classes 'StarSemiring' and 'Dioid'.
data Label a = Zero
             | One
             | Symbol a
             | Label a :+: Label a
             | Label a :*: Label a
             | Star (Label a)
             deriving (Eq, Functor, Ord)

-- | A type synonym for /regular expressions/, built on top of /free labels/.
type RegularExpression a = Label a

instance IsList (Label a) where
    type Item (Label a) = a
    fromList = foldr ((<>) . Symbol) Zero
    toList   = error "Label.toList cannot be given a reasonable definition"

instance Show a => Show (Label a) where
    showsPrec p label = case label of
        Zero     -> shows (0 :: Int)
        One      -> shows (1 :: Int)
        Symbol x -> shows x
        x :+: y  -> showParen (p >= 6) $ showsPrec 6 x . (" | " ++) . showsPrec 6 y
        x :*: y  -> showParen (p >= 7) $ showsPrec 7 x . (" ; " ++) . showsPrec 7 y
        Star x   -> showParen (p >= 8) $ showsPrec 8 x . ("*"   ++)

infixl 6 :+:
infixl 7 :*:

instance Semigroup (Label a) where
    Zero   <> x      = x
    x      <> Zero   = x
    One    <> One    = One
    One    <> Star x = Star x
    Star x <> One    = Star x
    x      <> y      = x :+: y

instance Monoid (Label a) where
    mempty  = Zero
    mappend = (<>)

instance Semiring (Label a) where
    one = One

    One  <.> x    = x
    x    <.> One  = x
    Zero <.> _    = Zero
    _    <.> Zero = Zero
    x    <.> y    = x :*: y

instance StarSemiring (Label a) where
    star Zero     = One
    star One      = One
    star (Star x) = star x
    star x        = Star x
