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
    ReflexiveRelation (..), SymmetricRelation (..), TransitiveRelation (..),
    PreorderRelation (..)
  ) where


import Control.DeepSeq (NFData (..))

import Algebra.Graph.Class
import Algebra.Graph.Relation (Relation, reflexiveClosure, symmetricClosure,
                               transitiveClosure, closure, domain, relation)
import qualified Data.Set as Set
import Data.Monoid (mconcat)

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

instance Ord a => Graph (ReflexiveRelation a) where
    type Vertex (ReflexiveRelation a) = a
    empty       = ReflexiveRelation empty
    vertex      = ReflexiveRelation . vertex
    overlay x y = ReflexiveRelation $ fromReflexive x `overlay` fromReflexive y
    connect x y = ReflexiveRelation $ fromReflexive x `connect` fromReflexive y

instance Ord a => Reflexive (ReflexiveRelation a)

-- TODO: Optimise the implementation by caching the results of symmetric closure.
{-|  The 'SymmetricRelation' data type represents a /symmetric binary relation/
over a set of elements. Symmetric relations satisfy all laws of the
'Undirected' type class and, in particular, the
commutativity of connect:

@'connect' x y == 'connect' y x@

The 'Show' instance produces symmetrically closed expressions:

@show (1     :: SymmetricRelation Int) == "vertex 1"
show (1 * 2 :: SymmetricRelation Int) == "edges [(1,2),(2,1)]"@

The 'Graph' instance respects the comutativity of connect.

The 'Num' instance respects the comutativity of connect.

The total order on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Here are a few examples:

@'vertex' 1 < 'vertex' 2
'vertex' 3 < 'Algebra.Graph.AdjacencyMap.edge' 1 2
'vertex' 1 < 'Algebra.Graph.AdjacencyMap.edge' 1 1
'Algebra.Graph.AdjacencyMap.edge' 1 1 < 'Algebra.Graph.AdjacencyMap.edge' 1 2
'Algebra.Graph.AdjacencyMap.edge' 1 2 < 'Algebra.Graph.AdjacencyMap.edge' 1 1 + 'Algebra.Graph.AdjacencyMap.edge' 2 2
'Algebra.Graph.AdjacencyMap.edge' 1 2 < 'Algebra.Graph.AdjacencyMap.edge' 1 3@

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'Algebra.Graph.AdjacencyMap.isSubgraphOf' x y ==> x <= y@

@'empty' <= x
x     <= x + y
x + y <= x * y@
-}
newtype SymmetricRelation a = SymmetricRelation { fromSymmetric :: Relation a }
    deriving NFData

instance Ord a => Eq (SymmetricRelation a) where
    x == y = symmetricClosure (fromSymmetric x) == symmetricClosure (fromSymmetric y)

instance (Ord a, Show a) => Show (SymmetricRelation a) where
    show = show . symmetricClosure . fromSymmetric

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (SymmetricRelation a) where
    type Vertex (SymmetricRelation a) = a
    empty       = SymmetricRelation empty
    vertex      = SymmetricRelation . vertex
    overlay x y = SymmetricRelation $ fromSymmetric x `overlay` fromSymmetric y
    connect x y = SymmetricRelation . symmetricClosure $ fromSymmetric x `connect` fromSymmetric y

instance (Ord a, Num a) => Num (SymmetricRelation a) where
    fromInteger = vertex . fromInteger
    x + y       = SymmetricRelation $ fromSymmetric x `overlay` fromSymmetric y
    x * y       = SymmetricRelation . symmetricClosure $ fromSymmetric x `connect` fromSymmetric y
    signum      = empty
    abs         = id
    negate      = id

instance Ord a => Ord (SymmetricRelation a) where
    compare x y = mconcat
        [ compare (Set.size . domain . fromSymmetric $   x) (Set.size . domain . fromSymmetric $   y)
        , compare (           domain . fromSymmetric $   x) (           domain . fromSymmetric $   y)
        , compare (Set.size . relation . fromSymmetric $ x) (Set.size . relation . fromSymmetric $ y)
        , compare (           relation . fromSymmetric $ x) (           relation . fromSymmetric $ y) ]

instance Ord a => Undirected (SymmetricRelation a)

-- TODO: Optimise the implementation by caching the results of transitive closure.
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

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (TransitiveRelation a) where
    type Vertex (TransitiveRelation a) = a
    empty       = TransitiveRelation empty
    vertex      = TransitiveRelation . vertex
    overlay x y = TransitiveRelation $ fromTransitive x `overlay` fromTransitive y
    connect x y = TransitiveRelation $ fromTransitive x `connect` fromTransitive y

instance Ord a => Transitive (TransitiveRelation a)

-- TODO: Optimise the implementation by caching the results of preorder closure.
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

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (PreorderRelation a) where
    type Vertex (PreorderRelation a) = a
    empty       = PreorderRelation empty
    vertex      = PreorderRelation . vertex
    overlay x y = PreorderRelation $ fromPreorder x `overlay` fromPreorder y
    connect x y = PreorderRelation $ fromPreorder x `connect` fromPreorder y

instance Ord a => Reflexive  (PreorderRelation a)
instance Ord a => Transitive (PreorderRelation a)
instance Ord a => Preorder   (PreorderRelation a)
