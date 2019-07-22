{-# LANGUAGE CPP, DeriveFunctor, OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Label
-- Copyright  : (c) Andrey Mokhov 2016-2019
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
    Distance, distance, getDistance, Capacity, capacity, getCapacity,
    Count, count, getCount, PowerSet (..), Minimum, getMinimum, noMinimum,
    Path, Label, isZero, RegularExpression,

    -- * Combining edge labels
    Optimum (..), ShortestPath, AllShortestPaths, CountShortestPaths, WidestPath
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid (Any (..), Sum (..))
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
newtype NonNegative a = NonNegative (Extended a)
    deriving (Applicative, Eq, Functor, Ord, Monad)

instance (Num a, Show a) => Show (NonNegative a) where
    show (NonNegative Infinite  ) = "infinite"
    show (NonNegative (Finite x)) = show x

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

    signum (NonNegative Infinite) = 1
    signum x = signum <$> x

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
unsafeFinite = NonNegative . Finite

-- | The (non-negative) infinite value.
infinite :: NonNegative a
infinite = NonNegative Infinite

-- | Get a finite value or @Nothing@ if the value is infinite.
getFinite :: NonNegative a -> Maybe a
getFinite (NonNegative x) = fromExtended x

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
    show (Capacity (Max (NonNegative (Finite x)))) = show x
    show _ = "capacity infinite"

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
    show (Count (Sum (NonNegative (Finite x)))) = show x
    show _ = "count infinite"

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
    show (Distance (Min (NonNegative (Finite x)))) = show x
    show _ = "distance infinite"

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

-- This data type extends the underlying type @a@ with a new 'Infinite' value.
data Extended a = Finite a | Infinite
    deriving (Eq, Functor, Ord, Show)

instance Applicative Extended where
    pure  = Finite
    (<*>) = ap

instance Monad Extended where
    return = pure

    Infinite >>= _ = Infinite
    Finite x >>= f = f x

-- Extract the finite value or @Nothing@ if the value is 'Infinite'.
fromExtended :: Extended a -> Maybe a
fromExtended (Finite a) = Just a
fromExtended Infinite   = Nothing

instance Num a => Num (Extended a) where
    fromInteger = Finite . fromInteger

    (+) = liftA2 (+)
    (*) = liftA2 (*)

    negate = fmap negate
    signum = fmap signum
    abs    = fmap abs

-- | If @a@ is a monoid, 'Minimum' @a@ forms the following 'Dioid':
--
-- @
-- 'zero'  = 'pure' 'mempty'
-- 'one'   = 'noMinimum'
-- ('<+>') = 'liftA2' 'min'
-- ('<.>') = 'liftA2' 'mappend'
-- @
--
-- To create a singleton value of type 'Minimum' @a@ use the 'pure' function.
-- For example:
--
-- @
-- getMinimum ('pure' "Hello, " '<+>' 'pure' "World!") == Just "Hello, "
-- getMinimum ('pure' "Hello, " '<.>' 'pure' "World!") == Just "Hello, World!"
-- @
newtype Minimum a = Minimum (Extended a)
    deriving (Applicative, Eq, Functor, Ord, Monad)

-- | Extract the minimum or @Nothing@ if it does not exist.
getMinimum :: Minimum a -> Maybe a
getMinimum (Minimum x) = fromExtended x

-- | The value corresponding to the lack of minimum, e.g. the minimum of the
-- empty set.
noMinimum :: Minimum a
noMinimum = Minimum Infinite

instance Ord a => Semigroup (Minimum a) where
    (<>) = liftA2 min

instance (Monoid a, Ord a) => Monoid (Minimum a) where
    mempty = pure mempty
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif

instance (Monoid a, Ord a) => Semiring (Minimum a) where
    one = noMinimum
    (<.>) = liftA2 mappend

instance (Monoid a, Ord a) => Dioid (Minimum a)

instance (Num a, Show a) => Show (Minimum a) where
    show (Minimum Infinite  ) = "one"
    show (Minimum (Finite x)) = show x

instance IsList a => IsList (Minimum a) where
    type Item (Minimum a) = Item a
    fromList = Minimum . Finite . fromList
    toList (Minimum x) = toList $ fromMaybe errorMessage (fromExtended x)
      where
        errorMessage = error "Minimum.toList applied to noMinimum value."

-- | The /power set/ over the underlying set of elements @a@. If @a@ is a
-- monoid, then the power set forms a 'Dioid' as follows:
--
-- @
-- 'zero'    = PowerSet Set.'Set.empty'
-- 'one'     = PowerSet $ Set.'Set.singleton' 'mempty'
-- x '<+>' y = PowerSet $ Set.'Set.union' (getPowerSet x) (getPowerSet y)
-- x '<.>' y = PowerSet $ 'setProductWith' 'mappend' (getPowerSet x) (getPowerSet y)
-- @
newtype PowerSet a = PowerSet { getPowerSet :: Set a }
    deriving (Eq, Monoid, Ord, Semigroup, Show)

instance (Monoid a, Ord a) => Semiring (PowerSet a) where
    one                       = PowerSet (Set.singleton mempty)
    PowerSet x <.> PowerSet y = PowerSet (setProductWith mappend x y)

instance (Monoid a, Ord a) => StarSemiring (PowerSet a) where
    star _ = one

instance (Monoid a, Ord a) => Dioid (PowerSet a) where

-- | The type of /free labels/ over the underlying set of symbols @a@. This data
-- type is an instance of classes 'StarSemiring' and 'Dioid'.
data Label a = Zero
             | One
             | Symbol a
             | Label a :+: Label a
             | Label a :*: Label a
             | Star (Label a)
             deriving Functor

infixl 6 :+:
infixl 7 :*:

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

-- | Check if a 'Label' is 'zero'.
isZero :: Label a -> Bool
isZero Zero = True
isZero _    = False

-- | A type synonym for /regular expressions/, built on top of /free labels/.
type RegularExpression a = Label a

-- | An /optimum semiring/ obtained by combining a semiring @o@ that defines an
-- /optimisation criterion/, and a semiring @a@ that describes the /arguments/
-- of an optimisation problem. For example, by choosing @o = 'Distance' Int@ and
-- and @a = 'Minimum' ('Path' String)@, we obtain the /shortest path semiring/
-- for computing the shortest path in an @Int@-labelled graph with @String@
-- vertices.
--
-- We assume that the semiring @o@ is /selective/ i.e. for all @x@ and @y@:
--
-- > x <+> y == x || x <+> y == y
--
-- In words, the operation '<+>' always simply selects one of its arguments. For
-- example, the 'Capacity' and 'Distance' semirings are selective, whereas the
-- the 'Count' semiring is not.
data Optimum o a = Optimum { getOptimum :: o, getArgument :: a }
    deriving (Eq, Ord, Show)

-- This is similar to geodetic semirings.
-- See http://vlado.fmf.uni-lj.si/vlado/papers/SemiRingSNA.pdf
instance (Eq o, Monoid a, Monoid o) => Semigroup (Optimum o a) where
    Optimum o1 a1 <> Optimum o2 a2
        | o1 == o2  = Optimum o1 (mappend a1 a2)
        | otherwise = Optimum o a
            where
              o = mappend o1 o2
              a = if o == o1 then a1 else a2

instance (Eq o, Monoid a, Monoid o) => Monoid (Optimum o a) where
    mempty  = Optimum mempty mempty
    mappend = (<>)

instance (Eq o, Semiring a, Semiring o) => Semiring (Optimum o a) where
    one = Optimum one one
    Optimum o1 a1 <.> Optimum o2 a2 = Optimum (o1 <.> o2) (a1 <.> a2)

instance (Eq o, StarSemiring a, StarSemiring o) => StarSemiring (Optimum o a) where
    star (Optimum o a) = Optimum (star o) (star a)

instance (Eq o, Dioid a, Dioid o) => Dioid (Optimum o a) where

-- | A /path/ is a list of edges.
type Path a = [(a, a)]

-- | The 'Optimum' semiring specialised to
-- /finding the lexicographically smallest shortest path/.
type ShortestPath e a = Optimum (Distance e) (Minimum (Path a))

-- | The 'Optimum' semiring specialised to /finding all shortest paths/.
type AllShortestPaths e a = Optimum (Distance e) (PowerSet (Path a))

-- | The 'Optimum' semiring specialised to /counting all shortest paths/.
type CountShortestPaths e a = Optimum (Distance e) (Count Integer)

-- | The 'Optimum' semiring specialised to
-- /finding the lexicographically smallest widest path/.
type WidestPath e a = Optimum (Capacity e) (Minimum (Path a))
