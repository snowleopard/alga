-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Acyclic.AdjacencyMap.Ord
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides convenient functions for constructing acyclic graphs by
-- keeping only edges @(x,y)@ where @x < y@ according to the supplied 'Ord' @a@
-- instance. This module can be imported qualified to avoid confusion with the
-- modules "Algebra.Graph.AdjacencyMap" and "Algebra.Graph.Acyclic.AdjacencyMap":
--
-- @
-- import qualified Algebra.Graph.Acyclic.AdjacencyMap.Ord as Acyclic.Ord
-- @
-----------------------------------------------------------------------------
module Algebra.Graph.Acyclic.AdjacencyMap.Ord (
    -- * Basic graph construction primitives
    edge, overlay, connect, edges, overlays, connects
    ) where

import Algebra.Graph.Acyclic.AdjacencyMap

import qualified Algebra.Graph.AdjacencyMap as AM

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' 1 1 (edge 1 1) == False
-- 'hasEdge' 1 2 (edge 1 2) == True
-- 'hasEdge' 2 1 (edge 2 1) == False
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
overlay x y = toAcyclicOrd $ AM.overlay (fromAcyclic x) (fromAcyclic y)

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
connect x y = toAcyclicOrd $ AM.connect (fromAcyclic x) (fromAcyclic y)

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
