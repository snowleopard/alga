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
    -- * Dioids
    Dioid (..), zero, (\/),

    -- * Data types for edge labels
    Distance (..)
  ) where

import Prelude ()
import Prelude.Compat

import Data.Monoid (Any (..), Monoid (..))
import Data.Semigroup (Semigroup (..), (<>))

{-| A /dioid/ is an /idempotent semiring/, i.e. it satisfies the following laws:

    * Commutativity:

        > x \/ y == y \/ x

    * Associativity:

        > x \/ (y \/ z) == (x \/ y) \/ z

    * Identity:

        > x \/ zero == x

    * Idempotence:

        > x \/ x == x



    * Associativity:

        > x /\ (y /\ z) == (x /\ y) /\ z

    * Identity:

        > x /\ one == x
        > one /\ x == x

    * Annihilating zero:

        > x /\ zero == zero
        > zero /\ x == zero

    * Distributivity:

        > x /\ (y \/ z) == x /\ y \/ x /\ z
        > (x \/ y) /\ z == x /\ z \/ y /\ z
-}
class (Monoid a, Semigroup a) => Dioid a where
    one  :: a
    (/\) :: a -> a -> a

-- | An alias for 'mempty'.
zero :: Dioid a => a
zero = mempty

-- | An alias for '<>'.
(\/) :: Dioid a => a -> a -> a
(\/) = (<>)

infixl 6 \/
infixl 7 /\

instance Dioid Any where
    one            = Any True
    Any x /\ Any y = Any (x && y)

-- | A /distance/ is a non-negative value that can be 'Finite' or 'Infinite'.
data Distance a = Finite a | Infinite deriving (Eq, Ord, Show)

instance (Ord a, Num a) => Num (Distance a) where
    fromInteger = Finite . fromInteger

    Infinite + _        = Infinite
    _        + Infinite = Infinite
    Finite x + Finite y = Finite (x + y)

    Infinite * _        = Infinite
    _        * Infinite = Infinite
    Finite x * Finite y = Finite (x * y)

    negate _ = error "Negative distances not allowed"

    signum (Finite 0) = 0
    signum _          = 1

    abs = id

instance Ord a => Semigroup (Distance a) where
    Infinite <> x        = x
    x        <> Infinite = x
    Finite x <> Finite y = Finite (min x y)

instance Ord a => Monoid (Distance a) where
    mempty = Infinite

instance (Num a, Ord a) => Dioid (Distance a) where
    one = Finite 0

    Infinite /\ _        = Infinite
    _        /\ Infinite = Infinite
    Finite x /\ Finite y = Finite (x + y)
