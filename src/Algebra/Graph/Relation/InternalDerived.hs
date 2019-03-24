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

import Algebra.Graph.Class
import Algebra.Graph.Relation.Internal hiding (empty, vertex, overlay, connect, referredToVertexSet, consistent)
import qualified Algebra.Graph.Relation as R (reflexiveClosure, transitiveClosure, closure, 
                                             connect, overlay, empty, vertex)

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
    x == y = R.reflexiveClosure (fromReflexive x) == R.reflexiveClosure (fromReflexive y)

instance (Ord a, Show a) => Show (ReflexiveRelation a) where
    show = show . R.reflexiveClosure . fromReflexive

-- TODO: Optimise the implementation by caching the results of reflexive closure.
instance Ord a => Graph (ReflexiveRelation a) where
    type Vertex (ReflexiveRelation a) = a
    empty       = ReflexiveRelation R.empty
    vertex      = ReflexiveRelation . R.vertex
    overlay x y = ReflexiveRelation $ fromReflexive x `R.overlay` fromReflexive y
    connect x y = ReflexiveRelation $ fromReflexive x `R.connect` fromReflexive y

instance Ord a => Reflexive (ReflexiveRelation a)

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
    x == y = R.transitiveClosure (fromTransitive x) == R.transitiveClosure (fromTransitive y)

instance (Ord a, Show a) => Show (TransitiveRelation a) where
    show = show . R.transitiveClosure . fromTransitive

-- TODO: Optimise the implementation by caching the results of transitive closure.
instance Ord a => Graph (TransitiveRelation a) where
    type Vertex (TransitiveRelation a) = a
    empty       = TransitiveRelation R.empty
    vertex      = TransitiveRelation . R.vertex
    overlay x y = TransitiveRelation $ fromTransitive x `R.overlay` fromTransitive y
    connect x y = TransitiveRelation $ fromTransitive x `R.connect` fromTransitive y

instance Ord a => Transitive (TransitiveRelation a)

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
    show = show . R.closure . fromPreorder

instance Ord a => Eq (PreorderRelation a) where
    x == y = R.closure (fromPreorder x) == R.closure (fromPreorder y)

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (PreorderRelation a) where
    type Vertex (PreorderRelation a) = a
    empty       = PreorderRelation R.empty
    vertex      = PreorderRelation . R.vertex
    overlay x y = PreorderRelation $ fromPreorder x `R.overlay` fromPreorder y
    connect x y = PreorderRelation $ fromPreorder x `R.connect` fromPreorder y

instance Ord a => Reflexive  (PreorderRelation a)
instance Ord a => Transitive (PreorderRelation a)
instance Ord a => Preorder   (PreorderRelation a)

