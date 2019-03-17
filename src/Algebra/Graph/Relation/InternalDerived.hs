-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.InternalDerived
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of derived binary relation data types.
-- The API is unstable and unsafe, and is exposed only for documentation. You
-- should use the non-internal modules "Algebra.Graph.Relation.Reflexive",
-- "Algebra.Graph.Relation.Symmetric", "Algebra.Graph.Relation.Transitive" and
-- "Algebra.Graph.Relation.Preorder" instead.
-----------------------------------------------------------------------------

module Algebra.Graph.Relation.InternalDerived (
    -- * Implementation of derived binary relations
    ReflexiveRelation (..), TransitiveRelation (..),
    PreorderRelation (..)
  ) where

import Control.DeepSeq (NFData (..))

import Algebra.Graph.Relation.Internal hiding (empty, vertex, overlay, connect, referredToVertexSet, consistent)
import Algebra.Graph.Relation (reflexiveClosure,
                               transitiveClosure, closure)

{-| The 'ReflexiveRelation' data type represents a /reflexive binary relation/
over a set of elements. Reflexive relations satisfy all laws of the
'Reflexive' type class and, in particular, the /self-loop/ axiom:

@'vertex' x == 'vertex' x * 'vertex' x@

The 'Show' instance produces reflexively closed expressions:

@show (1     :: ReflexiveRelation Int) == "edge 1 1"
show (1 * 2 :: ReflexiveRelation Int) == "edges [(1,1),(1,2),(2,2)]"@
-}
newtype ReflexiveRelation a = ReflexiveRelation { fromReflexive :: Relation a }
    deriving (Num, NFData)

instance Ord a => Eq (ReflexiveRelation a) where
    x == y = reflexiveClosure (fromReflexive x) == reflexiveClosure (fromReflexive y)

instance (Ord a, Show a) => Show (ReflexiveRelation a) where
    show = show . reflexiveClosure . fromReflexive

{-| The 'TransitiveRelation' data type represents a /transitive binary relation/
over a set of elements. Transitive relations satisfy all laws of the
'Transitive' type class and, in particular, the /closure/ axiom:

@y /= 'empty' ==> x * y + x * z + y * z == x * y + y * z@

For example, the following holds:

@'path' xs == ('clique' xs :: TransitiveRelation Int)@

The 'Show' instance produces transitively closed expressions:

@show (1 * 2         :: TransitiveRelation Int) == "edge 1 2"
show (1 * 2 + 2 * 3 :: TransitiveRelation Int) == "edges [(1,2),(1,3),(2,3)]"@
-}
newtype TransitiveRelation a = TransitiveRelation { fromTransitive :: Relation a }
    deriving (Num, NFData)

instance Ord a => Eq (TransitiveRelation a) where
    x == y = transitiveClosure (fromTransitive x) == transitiveClosure (fromTransitive y)

instance (Ord a, Show a) => Show (TransitiveRelation a) where
    show = show . transitiveClosure . fromTransitive

{-| The 'PreorderRelation' data type represents a
/binary relation that is both reflexive and transitive/. Preorders satisfy all
laws of the 'Preorder' type class and, in particular, the /self-loop/ axiom:

@'vertex' x == 'vertex' x * 'vertex' x@

and the /closure/ axiom:

@y /= 'empty' ==> x * y + x * z + y * z == x * y + y * z@

For example, the following holds:

@'path' xs == ('clique' xs :: PreorderRelation Int)@

The 'Show' instance produces reflexively and transitively closed expressions:

@show (1             :: PreorderRelation Int) == "edge 1 1"
show (1 * 2         :: PreorderRelation Int) == "edges [(1,1),(1,2),(2,2)]"
show (1 * 2 + 2 * 3 :: PreorderRelation Int) == "edges [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]"@
-}
newtype PreorderRelation a = PreorderRelation { fromPreorder :: Relation a }
    deriving (Num, NFData)

instance (Ord a, Show a) => Show (PreorderRelation a) where
    show = show . closure . fromPreorder

instance Ord a => Eq (PreorderRelation a) where
    x == y = closure (fromPreorder x) == closure (fromPreorder y)