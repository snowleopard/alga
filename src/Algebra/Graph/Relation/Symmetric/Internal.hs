-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Symmetric.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of derived binary relation data types.
-- The API is unstable and unsafe, and is exposed only for documentation. You
-- should use the non-internal module "Algebra.Graph.Relation.Symmetric" instead.
-----------------------------------------------------------------------------

module Algebra.Graph.Relation.Symmetric.Internal (
    -- * Implementation of symmetric binary relations
    Relation (..), empty, vertex, overlay, connect, consistent,
    referredToVertexSet, deduplicateSet, deduplicate
  ) where

import Control.DeepSeq (NFData (..))

import qualified Algebra.Graph.Relation.Internal as RI
import qualified Algebra.Graph.Relation          as R
import qualified Data.Set as Set
import Data.Monoid (mconcat)

{-|  This 'Relation' data type represents a /symmetric binary relation/
over a set of elements. Symmetric relations satisfy all laws of the
'Undirected' type class and, in addition, the
commutativity of connect:

@'connect' x y == 'connect' y x@

The 'Show' instance lists the vertices of an edge in non-decreasing order:

@show (1     :: Relation Int) == "vertex 1"
show (1 * 2 :: Relation Int) == "edge 1 2"
show (2 * 1 :: Relation Int) == "edge 1 2"
show (1 * 2 * 1 :: Relation Int) == "edges [(1,1),(1,2)]"
show (3 * 2 * 1 :: Relation Int) == "edges [(1,2),(1,3),(2,3)]"@

The total order on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Here are a few examples:

@'vertex' 1 < 'vertex' 2
'vertex' 3 < 'Algebra.Graph.Relation.Symmetric.edge' 1 2
'vertex' 1 < 'Algebra.Graph.Relation.Symmetric.edge' 1 1
'Algebra.Graph.Relation.Symmetric.edge' 1 2 == 'Algebra.Graph.Relation.Symmetric.edge' 2 1
'Algebra.Graph.Relation.Symmetric.edge' 1 1 < 'Algebra.Graph.Relation.Symmetric.edge' 1 2
'Algebra.Graph.Relation.Symmetric.edge' 1 2 < 'Algebra.Graph.Relation.Symmetric.edge' 1 1 + 'Algebra.Graph.Relation.Symmetric.edge' 2 2
'Algebra.Graph.Relation.Symmetric.edge' 1 2 < 'Algebra.Graph.Relation.Symmetric.edge' 1 3@

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'Algebra.Graph.Relation.Symmetric.isSubgraphOf' x y ==> x <= y@

@'empty' <= x
x     <= x + y
x + y <= x * y@
-}
newtype Relation a = SR { fromSymmetric :: RI.Relation a }
    deriving NFData

instance Ord a => Eq (Relation a) where
    x == y = fromSymmetric x == fromSymmetric y

instance (Ord a, Show a) => Show (Relation a) where
    show = show . filtered . fromSymmetric
        where
            filtered (RI.Relation d r) = RI.Relation d (deduplicateSet r)

instance Ord a => Ord (Relation a) where
    compare x y = mconcat
        [ compare (Set.size . R.domain . fromSymmetric $   x) (Set.size . R.domain . fromSymmetric $   y)
        , compare (           R.domain . fromSymmetric $   x) (           R.domain . fromSymmetric $   y)
        , compare (Set.size . deduplicateSet . R.relation . fromSymmetric $ x) 
                  (Set.size . deduplicateSet . R.relation . fromSymmetric $ y)
        , compare (           R.relation . fromSymmetric $ x) (           R.relation . fromSymmetric $ y) ]

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.Relation.Symmetric.isEmpty'     empty == True
-- 'Algebra.Graph.Relation.Symmetric.hasVertex' x empty == False
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' empty == 0
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   empty == 0
-- @
empty :: Relation a
empty = SR $ RI.Relation Set.empty Set.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.Relation.Symmetric.isEmpty'     (vertex x) == False
-- 'Algebra.Graph.Relation.Symmetric.hasVertex' x (vertex x) == True
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' (vertex x) == 1
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (vertex x) == 0
-- @
vertex :: a -> Relation a
vertex x = SR $ RI.Relation (Set.singleton x) Set.empty

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'Algebra.Graph.Relation.Symmetric.isEmpty'     (overlay x y) == 'Algebra.Graph.Relation.Symmetric.isEmpty'   x   && 'Algebra.Graph.Relation.Symmetric.isEmpty'   y
-- 'Algebra.Graph.Relation.Symmetric.hasVertex' z (overlay x y) == 'Algebra.Graph.Relation.Symmetric.hasVertex' z x || 'Algebra.Graph.Relation.Symmetric.hasVertex' z y
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' (overlay x y) >= 'Algebra.Graph.Relation.Symmetric.vertexCount' x
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' (overlay x y) <= 'Algebra.Graph.Relation.Symmetric.vertexCount' x + 'Algebra.Graph.Relation.Symmetric.vertexCount' y
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (overlay x y) >= 'Algebra.Graph.Relation.Symmetric.edgeCount' x
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (overlay x y) <= 'Algebra.Graph.Relation.Symmetric.edgeCount' x   + 'Algebra.Graph.Relation.Symmetric.edgeCount' y
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' (overlay 1 2) == 2
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Ord a => Relation a -> Relation a -> Relation a
overlay (SR x) (SR y) = SR $ RI.Relation 
                                        (R.domain x   `Set.union` R.domain y)
                                        (R.relation x `Set.union` R.relation y)

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2 + n2 * n1)/.
--
-- @
-- connect x y == connect y x
-- 'Algebra.Graph.Relation.Symmetric.isEmpty'     (connect x y) == 'Algebra.Graph.Relation.Symmetric.isEmpty'   x   && 'Algebra.Graph.Relation.Symmetric.isEmpty'   y
-- 'Algebra.Graph.Relation.Symmetric.hasVertex' z (connect x y) == 'Algebra.Graph.Relation.Symmetric.hasVertex' z x || 'Algebra.Graph.Relation.Symmetric.hasVertex' z y
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' (connect x y) >= 'Algebra.Graph.Relation.Symmetric.vertexCount' x
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' (connect x y) <= 'Algebra.Graph.Relation.Symmetric.vertexCount' x + 'Algebra.Graph.Relation.Symmetric.vertexCount' y
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (connect x y) >= 'Algebra.Graph.Relation.Symmetric.edgeCount' x
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (connect x y) >= 'Algebra.Graph.Relation.Symmetric.edgeCount' y
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (connect x y) >= 'Algebra.Graph.Relation.Symmetric.vertexCount' x * 'Algebra.Graph.Relation.Symmetric.vertexCount' y `div` 2
-- 'Algebra.Graph.Relation.Symmetric.vertexCount' (connect 1 2) == 2
-- 'Algebra.Graph.Relation.Symmetric.edgeCount'   (connect 1 2) == 1
-- @
connect :: Ord a => Relation a -> Relation a -> Relation a
connect (SR x) (SR y) = SR $ RI.Relation 
                                         (R.domain x `Set.union` R.domain y)
                                         (R.relation x `Set.union` R.relation y 
                                         `Set.union` 
                                         (R.domain x `RI.setProduct` R.domain y)
                                         `Set.union` 
                                         (R.domain y `RI.setProduct` R.domain x))

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    x + y       = x `overlay` y
    x * y       = x `connect` y
    signum      = const empty
    abs         = id
    negate      = id

-- | Check if the internal representation of a symmetric relation is consistent, i.e. if all
-- pairs of elements in the 'relation' refer to existing elements in the 'domain' and that if all pair of edges have their symmetric counterparts.
-- It should be impossible to create an inconsistent 'Relation', and we use this
-- function in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent 'Algebra.Graph.Relation.Symmetric.empty'         == True
-- consistent ('Algebra.Graph.Relation.Symmetric.vertex' x)    == True
-- consistent ('Algebra.Graph.Relation.Symmetric.overlay' x y) == True
-- consistent ('Algebra.Graph.Relation.Symmetric.connect' x y) == True
-- consistent ('Algebra.Graph.Relation.Symmetric.edge' x y)    == True
-- consistent ('Algebra.Graph.Relation.Symmetric.edges' xs)    == True
-- consistent ('Algebra.Graph.Relation.Symmetric.stars' xs)    == True
-- @
consistent :: Ord a => Relation a -> Bool
consistent (SR r) = (referredToVertexSet (R.relation r) `Set.isSubsetOf` R.domain r) && (r == R.transpose r)

-- | The set of elements that appear in a given set of pairs.
-- /Note: this function is for internal use only/.
referredToVertexSet :: Ord a => Set.Set (a, a) -> Set.Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . Set.toAscList

-- | Auxiliary function to deduplicate edges on sets.
-- /Note: this function is for internal use only/.
deduplicateSet :: Ord a => Set.Set (a, a) -> Set.Set (a, a)
deduplicateSet = Set.filter (uncurry (<=))

-- | Auxiliary function to deduplicate edges on lists.
-- /Note: this function is for internal use only/.
deduplicate :: Ord a => [(a, a)] -> [(a, a)]
deduplicate = filter (uncurry (<=))
