-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'AdjacencyMap' data type for edge-labelled graphs, as
-- well as associated operations and algorithms. 'AdjacencyMap' is an instance
-- of the 'C.Graph' type class, which can be used for polymorphic graph
-- construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, adjacencyMap,

    -- * Basic graph construction primitives
    empty, vertex, overlay, connect, edge, vertices, edges, overlays, (-<), (>-),

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, edgeLabel, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, postSet, preSet,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, transpose, gmap,
    emap, induce
  ) where

import Prelude ()
import Prelude.Compat

import Data.Foldable (foldMap)
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import Data.Set (Set)

import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap.Internal

import qualified Data.Map.Strict as Map

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory.
edge :: (Ord a, Eq e, Semilattice e) => e -> a -> a -> AdjacencyMap e a
edge e x y | e == zero = vertices [x, y]
           | x == y    = AM $ Map.singleton x (Map.singleton x e)
           | otherwise = AM $ Map.fromList [(x, Map.singleton y e), (y, Map.empty)]

-- | The left-hand part of a convenient ternary-ish operator @x -\<e\>- y@ for
-- connecting graphs with labelled edges. For example:
--
-- @
-- x = 'vertex' "x"
-- y = 'vertex' "y"
-- z = x -\<2\>- y
-- @
(-<) :: AdjacencyMap e a -> e -> (AdjacencyMap e a, e)
g -< e = (g, e)

-- | The right-hand part of a convenient ternary-ish operator @x -\<e\>- y@ for
-- connecting graphs with labelled edges. For example:
--
-- @
-- x = 'vertex' "x"
-- y = 'vertex' "y"
-- z = x -\<2\>- y
-- @
(>-) :: (Ord a, Dioid e) => (AdjacencyMap e a, e) -> AdjacencyMap e a -> AdjacencyMap e a
(g, e) >- h = connect e g h

infixl 5 -<
infixl 5 >-

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
vertices :: Ord a => [a] -> AdjacencyMap e a
vertices = AM . Map.fromList . map (, Map.empty)

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
edges :: (Ord a, Eq e, Semilattice e) => [(e, a, a)] -> AdjacencyMap e a
edges = fromAdjacencyMaps . concatMap fromEdge
  where
    fromEdge (e, x, y) | e == zero = [(x, Map.empty), (y, Map.empty)]
                       | otherwise = [(x, Map.singleton y e)]

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
overlays :: (Ord a, Semilattice e) => [AdjacencyMap e a] -> AdjacencyMap e a
overlays = AM . Map.unionsWith (Map.unionWith (\/)) . map adjacencyMap

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second. Complexity: /O((n + m) * log(n))/
-- time.
isSubgraphOf :: (Ord a, Eq e) => AdjacencyMap e a -> AdjacencyMap e a -> Bool
isSubgraphOf (AM x) (AM y) = Map.isSubmapOfBy Map.isSubmapOf x y

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
isEmpty :: AdjacencyMap e a -> Bool
isEmpty = Map.null . adjacencyMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
hasVertex :: Ord a => a -> AdjacencyMap e a -> Bool
hasVertex x = Map.member x . adjacencyMap

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
hasEdge :: Ord a => a -> a -> AdjacencyMap e a -> Bool
hasEdge x y (AM m) = fromMaybe False (Map.member y <$> Map.lookup x m)

-- | Extract the label of a specified edge from a graph.
edgeLabel :: (Ord a, Semilattice e) => a -> a -> AdjacencyMap e a -> e
edgeLabel x y (AM m) = fromMaybe zero (Map.lookup x m >>= Map.lookup y)

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
vertexCount :: AdjacencyMap e a -> Int
vertexCount = Map.size . adjacencyMap

-- | The number of (non-'zero') edges in a graph.
-- Complexity: /O(n)/ time.
edgeCount :: AdjacencyMap e a -> Int
edgeCount = getSum . foldMap (Sum . Map.size) . adjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
vertexList :: AdjacencyMap e a -> [a]
vertexList = Map.keys . adjacencyMap

-- | The list of edges of a graph, sorted lexicographically with respect to
-- pairs of connected vertices (i.e. edge-labels are ignored when sorting).
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
edgeList :: AdjacencyMap e a -> [(e, a, a)]
edgeList (AM m) =
    [ (e, x, y) | (x, ys) <- Map.toAscList m, (y, e) <- Map.toAscList ys ]

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
vertexSet :: AdjacencyMap e a -> Set a
vertexSet = Map.keysSet . adjacencyMap

-- | The /preset/ of an element @x@ is the set of its /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
preSet :: Ord a => a -> AdjacencyMap e a -> Map a e
preSet x (AM m) = Map.fromAscList
    [ (a, e) | (a, es) <- Map.toAscList m, Just e <- [Map.lookup x es] ]

-- | The /postset/ of a vertex is the set of its /direct successors/.
-- Complexity: /O(log(n))/ time and /O(1)/ memory.
postSet :: Ord a => a -> AdjacencyMap e a -> Map a e
postSet x = Map.findWithDefault Map.empty x . adjacencyMap

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
removeVertex :: Ord a => a -> AdjacencyMap e a -> AdjacencyMap e a
removeVertex x = AM . Map.map (Map.delete x) . Map.delete x . adjacencyMap

-- | Remove an edge from a given graph.
-- Complexity: /O(log(n))/ time.
removeEdge :: Ord a => a -> a -> AdjacencyMap e a -> AdjacencyMap e a
removeEdge x y = AM . Map.adjust (Map.delete y) x . adjacencyMap

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
replaceVertex :: (Ord a, Semilattice e) => a -> a -> AdjacencyMap e a -> AdjacencyMap e a
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
mergeVertices :: (Ord a, Semilattice e) => (a -> Bool) -> a -> AdjacencyMap e a -> AdjacencyMap e a
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
transpose :: (Ord a, Semilattice e) => AdjacencyMap e a -> AdjacencyMap e a
transpose (AM m) = AM $ Map.foldrWithKey combine vs m
  where
    combine v es = Map.unionWith (Map.unionWith (\/)) $
        Map.fromAscList [ (u, Map.singleton v e) | (u, e) <- Map.toAscList es ]
    vs = Map.fromSet (const Map.empty) (Map.keysSet m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
gmap :: (Ord a, Ord b, Semilattice e) => (a -> b) -> AdjacencyMap e a -> AdjacencyMap e b
gmap f = AM . Map.map (Map.mapKeysWith (\/) f) .
    Map.mapKeysWith (Map.unionWith (\/)) f . adjacencyMap

-- | Transform a graph by applying a function to each of its edge labels.
-- Complexity: /O((n + m) * log(n))/ time.
emap :: (e -> f) -> AdjacencyMap e a -> AdjacencyMap f a
emap f = AM . Map.map (Map.map f) . adjacencyMap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
induce :: (a -> Bool) -> AdjacencyMap e a -> AdjacencyMap e a
induce p = AM . Map.map (Map.filterWithKey (\k _ -> p k)) .
    Map.filterWithKey (\k _ -> p k) . adjacencyMap
