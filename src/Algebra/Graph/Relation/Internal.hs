-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of the 'Relation' data type. The API
-- is unstable and unsafe, and is exposed only for documentation. You should
-- use the non-internal module "Algebra.Graph.Relation" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Internal (
    -- * Binary relation implementation
    Relation (..), consistent, setProduct, referredToVertexSet
  ) where

import Data.Set (Set, union)

import Algebra.Graph.Class

import qualified Data.Set as Set

{-| The 'Relation' data type represents a graph as a /binary relation/. We
define a 'Num' instance as a convenient notation for working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show (empty     :: Relation Int) == "empty"
show (1         :: Relation Int) == "vertex 1"
show (1 + 2     :: Relation Int) == "vertices [1,2]"
show (1 * 2     :: Relation Int) == "edge 1 2"
show (1 * 2 * 3 :: Relation Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: Relation Int) == "overlay (vertex 3) (edge 1 2)"@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'Algebra.Graph.Relation.overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'Algebra.Graph.Relation.connect' is associative and has
    'Algebra.Graph.Relation.empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'Algebra.Graph.Relation.connect' distributes over
    'Algebra.Graph.Relation.overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'Algebra.Graph.Relation.connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'Algebra.Graph.Relation.overlay' has 'Algebra.Graph.Relation.empty' as the
    identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'Algebra.Graph.Relation.connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.
-}
data Relation a = Relation {
    -- | The /domain/ of the relation.
    domain :: Set a,
    -- | The set of pairs of elements that are /related/. It is guaranteed that
    -- each element belongs to the domain.
    relation :: Set (a, a)
  } deriving Eq

instance (Ord a, Show a) => Show (Relation a) where
    show (Relation d r)
        | Set.null d = "empty"
        | Set.null r = vshow (Set.toAscList d)
        | d == used  = eshow (Set.toAscList r)
        | otherwise  = "overlay (" ++ vshow (Set.toAscList $ Set.difference d used)
                    ++ ") (" ++ eshow (Set.toAscList r) ++ ")"
      where
        vshow [x]      = "vertex "   ++ show x
        vshow xs       = "vertices " ++ show xs
        eshow [(x, y)] = "edge "     ++ show x ++ " " ++ show y
        eshow xs       = "edges "    ++ show xs
        used           = referredToVertexSet r

instance Ord a => Graph (Relation a) where
    type Vertex (Relation a) = a
    empty       = Relation Set.empty Set.empty
    vertex x    = Relation (Set.singleton x) Set.empty
    overlay x y = Relation (domain x `union` domain y) (relation x `union` relation y)
    connect x y = Relation (domain x `union` domain y) (relation x `union` relation y
        `union` (domain x `setProduct` domain y))

-- | Compute the Cartesian product of two sets. /Note: this function is for internal use only/.
setProduct :: Set a -> Set b -> Set (a, b)
setProduct x y = Set.fromDistinctAscList [ (a, b) | a <- Set.toAscList x, b <- Set.toAscList y ]

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance ToGraph (Relation a) where
    type ToVertex (Relation a) = a
    toGraph (Relation d r) = vertices (Set.toList d) `overlay` edges (Set.toList r)

-- | Check if the internal representation of a relation is consistent, i.e. if all
-- pairs of elements in the 'relation' refer to existing elements in the 'domain'.
-- It should be impossible to create an inconsistent 'Relation', and we use this
-- function in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent 'Algebra.Graph.Relation.empty'                  == True
-- consistent ('Algebra.Graph.Relation.vertex' x)             == True
-- consistent ('Algebra.Graph.Relation.overlay' x y)          == True
-- consistent ('Algebra.Graph.Relation.connect' x y)          == True
-- consistent ('Algebra.Graph.Relation.edge' x y)             == True
-- consistent ('Algebra.Graph.Relation.edges' xs)             == True
-- consistent ('Algebra.Graph.Relation.graph' xs ys)          == True
-- consistent ('Algebra.Graph.Relation.fromAdjacencyList' xs) == True
-- @
consistent :: Ord a => Relation a -> Bool
consistent (Relation d r) = referredToVertexSet r `Set.isSubsetOf` d

-- | The set of elements that appear in a given set of pairs.
-- /Note: this function is for internal use only/.
referredToVertexSet :: Ord a => Set (a, a) -> Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . Set.toAscList
