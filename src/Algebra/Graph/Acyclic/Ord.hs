-----------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Acyclic.Ord
-- License    : MIT (see the file LICENSE)
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines functions which guarantee Acyclic graph by
-- establishing an invariant on the edges.
-- An edge (x, y) exists if and only if x < y. This way no back
-- edges are formed and hence making a cyclic graph is impossible.
----------------------------------------------------------------
module Algebra.Graph.Acyclic.Ord (
    -- * Graph construction primitives
    edge, overlay, connect, edges, overlays, connects,

    -- * Graph transformation
    replaceVertex, mergeVertices, gmap,
    ) where

import Algebra.Graph.Acyclic.AdjacencyMap
import qualified Algebra.Graph.AdjacencyMap as AM
import Data.Function (on)

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' 1 2 (edge 1 2) == True
-- 'hasEdge' 2 1 (edge 2 1) == False
-- 'edgeCount'   (edge 1 2) == 1
-- 'edgeCount'   (edge 2 1) == 0
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- 'vertexCount' (edge 2 1) == 2
-- @
edge :: Ord a => a -> a -> AdjacencyMap a
edge x y = toAcyclicOrd (AM.edge x y)

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay x y = toAcyclicOrd $ (AM.overlay `on` fromAcyclic) x y

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'isEmpty'     (connect x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y) >= 'vertexCount' x
-- 'vertexCount' (connect x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'edgeCount' x
-- 'edgeCount'   (connect x y) >= 'edgeCount' y
-- 'edgeCount'   (connect x y) <= 'vertexCount' x * 'vertexCount' y + 'edgeCount' x + 'edgeCount' y
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- 'edgeCount'   (connect 2 1) == 0
-- @
connect :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect x y = toAcyclicOrd $ (AM.connect `on` fromAcyclic) x y

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'filter' (uncurry (<)) . 'Data.List.nub'
-- 'edgeList' . edges  == 'filter' (uncurry (<)) . 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: Ord a => [(a, a)] -> AdjacencyMap a
edges = toAcyclicOrd . AM.edges

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
overlays = toAcyclicOrd . AM.overlays . map fromAcyclic

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
connects = toAcyclicOrd . AM.connects . map fromAcyclic

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with 
-- vertex @y@ in a given 'AdjacencyMap'. All the edges are filtered
-- for the invariant to hold true.
-- If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- replaceVertex 1 2 (1 * 3)    == 2 * 3
-- replaceVertex 1 4 (1 * 3)    == 4 + 3
-- @
replaceVertex :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex
-- keeping the invariant true.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices ('const' False) x        == id
-- mergeVertices (== x) y               == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)         == 1
-- mergeVertices 'odd'  1 (3 + 4 * 5)     == 4 + 1
-- mergeVertices 'even' 1 (2 * 3 + 4 * 5) == 1 * 3 + 1 * 5
-- @
mergeVertices :: Ord a => (a -> Bool) -> a -> AdjacencyMap a -> AdjacencyMap a
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transform a graph by applying a function to each of its
-- vertices without breaking the edge invariant.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'                   == 'empty'
-- gmap f ('vertex' x)              == 'vertex' (f x)
-- 'vertexList' (gmap f ('edge' x y)) == 'vertexList' ('edge' (f x) (f y))
-- 'edgeCount'  (gmap f ('edge' x y)) <= 'edgeCount' ('edge' (f x) (f y))
-- gmap 'id'                        == 'id'
-- 'vertexList' . gmap f . gmap g   == 'vertexList' . gmap (f . g)
-- 'edgeCount' (gmap f (gmap g x))  <= 'edgeCount' (gmap (f . g) x)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap f = toAcyclicOrd . AM.gmap f . fromAcyclic
