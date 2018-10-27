-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.NonEmpty.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module exposes the implementation of non-empty adjacency maps. The API
-- is unstable and unsafe, and is exposed only for documentation. You should use
-- the non-internal module "Algebra.Graph.AdjacencyMap.NonEmpty" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.NonEmpty.Internal (
    -- * Adjacency map implementation
    AdjacencyMap (..), vertex, overlay, connect, consistent
    ) where

import Control.DeepSeq

import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AM

{-| The 'AdjacencyMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a 'Num' instance as a convenient notation for
working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

Note that the 'signum' method of the type class 'Num' cannot be implemented.

The 'Eq' instance satisfies the following laws of algebraic graphs:

    * 'overlay' is commutative, associative and idempotent:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z
        >       x + x == x

    * 'connect' is associative:

        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

    * 'connect' satisfies absorption and saturation:

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.

The total order on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Here are a few examples:

@'vertex' 1 < 'vertex' 2
'vertex' 3 < 'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 2
'vertex' 1 < 'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 1
'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 1 < 'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 2
'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 2 < 'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 1 + 'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 2 2
'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 2 < 'Algebra.Graph.AdjacencyMap.NonEmpty.edge' 1 3@

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'Algebra.Graph.AdjacencyMap.isSubgraphOf' x y ==> x <= y@

@x     <= x + y
x + y <= x * y@
-}
newtype AdjacencyMap a = NAM {
    -- | The /adjacency map/ of a graph: each vertex is associated with a set of
    -- its direct successors. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- adjacencyMap 'empty'      == Map.'Map.empty'
    -- adjacencyMap ('vertex' x) == Map.'Map.singleton' x Set.'Set.empty'
    -- adjacencyMap ('Algebra.Graph.AdjacencyMap.edge' 1 1) == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- adjacencyMap ('Algebra.Graph.AdjacencyMap.edge' 1 2) == Map.'Map.fromList' [(1,Set.'Set.singleton' 2), (2,Set.'Set.empty')]
    -- @
    am :: AM.AdjacencyMap a } deriving (Eq, NFData, Ord)

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = error "NonEmpty.AdjacencyMap.signum cannot be implemented."
    abs         = id
    negate      = id

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.hasVertex' x (vertex x) == True
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' (vertex x) == 1
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap a
vertex = NAM . AM.vertex

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.hasVertex' z (overlay x y) == 'Algebra.Graph.AdjacencyMap.NonEmpty.hasVertex' z x || 'Algebra.Graph.AdjacencyMap.NonEmpty.hasVertex' z y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' (overlay x y) >= 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' x
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' (overlay x y) <= 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' x + 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (overlay x y) >= 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount' x
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (overlay x y) <= 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount' x   + 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount' y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' (overlay 1 2) == 2
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay (NAM x) (NAM y) = NAM (AM.overlay x y)

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.hasVertex' z (connect x y) == 'Algebra.Graph.AdjacencyMap.NonEmpty.hasVertex' z x || 'Algebra.Graph.AdjacencyMap.NonEmpty.hasVertex' z y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' (connect x y) >= 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' x
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' (connect x y) <= 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' x + 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (connect x y) >= 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount' x
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (connect x y) >= 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount' y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (connect x y) >= 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' x * 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (connect x y) <= 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' x * 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' y + 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount' x + 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount' y
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.vertexCount' (connect 1 2) == 2
-- 'Algebra.Graph.AdjacencyMap.NonEmpty.edgeCount'   (connect 1 2) == 1
-- @
connect :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect (NAM x) (NAM y) = NAM (AM.connect x y)

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices, and the graph is non-empty. It should be
-- impossible to create an inconsistent adjacency map, and we use this function
-- in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent ('vertex' x)    == True
-- consistent ('overlay' x y) == True
-- consistent ('connect' x y) == True
-- consistent ('Algebra.Graph.AdjacencyMap.NonEmpty.edge' x y)    == True
-- consistent ('Algebra.Graph.AdjacencyMap.NonEmpty.edges' xs)    == True
-- consistent ('Algebra.Graph.AdjacencyMap.NonEmpty.stars' xs)    == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent (NAM x) = AM.consistent x && not (AM.isEmpty x)
