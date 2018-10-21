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
    NonNegative, finite, finiteWord, unsafeFinite, infinite, getFinite,
    Capacity, capacity, getCapacity, Count, count, getCount, Distance, distance,
    getDistance, Power (..), ShortestPath, Path, edgePath, getShortestDistance,
    getShortestPath, Label, isZero, RegularExpression
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (liftA2)
import Data.Maybe
import Data.Monoid (Any (..), Monoid (..), Sum (..))
import Data.Semigroup (Min (..), Max (..), Semigroup (..))
import Data.Set (Set)
import GHC.Exts (IsList (..))

import Algebra.Graph.Internal

import qualified Data.Set as Set

{-| A /semiring/ extends a commutative 'Monoid' with operation '<.>' that acts
similarly to multiplication over the underlying (additive) monoid and has 'one'
as the identity. This module also provides two convenient aliases: 'zero' for
'mempty', and '<+>' for '<>', which makes the interface more uniform.

Instances of this type class must satisfy the following semiring laws:

    * Associativity of '<+>' and '<.>':

        > x <+> (y <+> z) == (x <+> y) <+> z
        > x <.> (y <.> z) == (x <.> y) <.> z

    * Identities of '<+>' and '<.>':

        > zero <+> x == x == x <+> zero
        >  one <.> x == x == x <.> one

    * Commutativity of '<+>':

        > x <+> y == y <+> x

    * Annihilating 'zero':

        > x <.> zero == zero
        > zero <.> x == zero

    * Distributivity:

        > x <.> (y <+> z) == x <.> y <+> x <.> z
        > (x <+> y) <.> z == x <.> z <+> y <.> z
-}
class (Monoid a, Semigroup a) => Semiring a where
    one   :: a
    (<.>) :: a -> a -> a

{-| A /star semiring/ is a 'Semiring' with an additional unary operator 'star'
satisfying the following two laws:

    > star a = one <+> a <.> star a
    > star a = one <+> star a <.> a
-}
class Semiring a => StarSemiring a where
    star :: a -> a

{-| A /dioid/ is an /idempotent semiring/, i.e. it satisfies the following
/idempotence/ law in addition to the 'Semiring' laws:

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

-- | A non-negative finite value, created /unsafely/: the argument is not
-- checked for being non-negative, so @unsafeFinite (-1)@ compiles just fine.
unsafeFinite :: a -> NonNegative a
unsafeFinite = NonNegative . Just

-- | The (non-negative) infinite value.
infinite :: NonNegative a
infinite = NonNegative Nothing

-- | Get a finite value or @Nothing@ if the value is infinite.
getFinite :: NonNegative a -> Maybe a
getFinite (NonNegative x) = x

-- | A /capacity/ is a non-negative value that can be 'finite' or 'infinite'.
-- Capacities form a 'Dioid' as follows:
--
-- @
-- 'zero'  = 0
-- 'one'   = 'capacity' 'infinite'
-- ('<+>') = 'max'
-- ('<.>') = 'min'
-- @
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
--
-- @
-- 'zero'  = 0
-- 'one'   = 1
-- ('<+>') = ('+')
-- ('<.>') = ('*')
-- @
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

-- | A /distance/ is a non-negative value that can be 'finite' or 'infinite'.
-- Distances form a 'Dioid' as follows:
--
-- @
-- 'zero'  = 'distance' 'infinite'
-- 'one'   = 0
-- ('<+>') = 'min'
-- ('<.>') = ('+')
-- @
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

-- | The /power set/ over the underlying set of elements @a@. If @a@ is a
-- monoid, then the power set forms a 'Dioid' as follows:
--
-- @
-- 'zero'  = Set.'Set.empty'
-- 'one'   = Set.'Set.singleton' 'mempty'
-- ('<+>') = Set.'Set.union'
-- ('<.>') = 'setProductWith' ('<>')
-- @
newtype Power a = Power { getSet :: Set a }
    deriving (Eq, Monoid, Ord, Semigroup, Show)

instance (Monoid a, Ord a) => Semiring (Set a) where
    one   = Set.singleton mempty
    (<.>) = setProductWith (<>)

instance (Monoid a, Ord a) => Dioid (Set a) where

-- | A /path/ is a list of edges.
type Path a = [(a, a)]

-- | A /shortest path/ encapsulating a shortest 'Distance' and the corresponding
-- 'Path'. This is an abstract datatype, whose semiring instance has the
-- following meaning:
--
-- * 'zero' is the /empty path/ of zero distance.
-- * 'one'  is the lack of any path, or a path of infinite distance.
-- * '<+>' picks the shortest of the two paths, or the lexicographically smaller
--   one in case of a tie.
-- * '<.>' concatenates two paths, adding their distances.
data ShortestPath e a = ShortestPath (Distance e) (Path a)

instance (Ord a, Ord e) => Semigroup (ShortestPath e a) where
    ShortestPath d1 p1 <> ShortestPath d2 p2
        | d1 < d2 || d1 == d2 && p1 < p2 = ShortestPath d1 p1
        | otherwise                      = ShortestPath d2 p2

instance (Num e, Ord a, Ord e) => Monoid (ShortestPath e a) where
    mempty  = ShortestPath zero []
    mappend = (<>)

instance (Num e, Ord a, Ord e) => Semiring (ShortestPath e a) where
    one = ShortestPath one []

    ShortestPath d1 p1 <.> ShortestPath d2 p2 = ShortestPath (d1 <.> d2) (p1 ++ p2)

instance (Num e, Ord a, Ord e) => StarSemiring (ShortestPath e a) where
    star _ = one

instance (Num e, Ord a, Ord e) => Dioid (ShortestPath e a)

-- | A path comprising a single edge of non-negative distance between two (not
-- necessarily distinct) vertices.
edgePath :: NonNegative e -> a -> a -> ShortestPath e a
edgePath d x y = ShortestPath (distance d) [(x, y)]

-- | Get the distance corresponding to a shortest path.
getShortestDistance :: ShortestPath e a -> NonNegative e
getShortestDistance (ShortestPath e _) = getDistance e

-- | Get the shortest path or @Nothing@ if there is no path of finite distance.
getShortestPath :: ShortestPath e a -> Maybe (Path a)
getShortestPath (ShortestPath e p)
    | isNothing (getFinite (getDistance e)) = Nothing
    | otherwise                             = Just p

-- | The type of /free labels/ over the underlying set of symbols @a@. This data
-- type is an instance of classes 'StarSemiring' and 'Dioid'.
data Label a = Zero
             | One
             | Symbol a
             | Label a :+: Label a
             | Label a :*: Label a
             | Star (Label a)
             deriving Functor

-- | Check if a 'Label' is 'zero'.
isZero :: Label a -> Bool
isZero Zero = True
isZero _    = False

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
