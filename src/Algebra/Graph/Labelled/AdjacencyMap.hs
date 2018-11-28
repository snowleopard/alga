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
    empty, vertex, edge, (-<), (>-), overlay, connect, vertices, edges,
    overlays, fromAdjacencyMaps,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, edgeLabel, vertexCount, edgeCount, vertexList,
    edgeList, vertexSet, postSet, preSet, skeleton,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, replaceEdge, mergeVertices, transpose, gmap,
    emap, induce,

    -- * Relational operations
    closure, reflexiveClosure, symmetricClosure, transitiveClosure
  ) where

import Prelude ()
import Prelude.Compat

import Data.Foldable (foldMap)
import Data.Maybe
import Data.Map (Map)
import Data.Monoid (Any, Monoid, Sum (..))
import Data.Semigroup (Semigroup)
import Data.Set (Set)

import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap.Internal

import qualified Algebra.Graph.AdjacencyMap.Internal as AM
import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: AdjacencyMap e a
empty = AM Map.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap e a
vertex x = AM $ Map.singleton x Map.empty

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory.
--
-- @
-- edge e    x y              == 'connect' e ('vertex' x) ('vertex' y)
-- edge 'zero' x y              == 'vertices' [x,y]
-- 'hasEdge'   x y (edge e x y) == (e /= 'zero')
-- 'edgeLabel' x y (edge e x y) == e
-- 'edgeCount'     (edge e x y) == if e == 'zero' then 0 else 1
-- 'vertexCount'   (edge e 1 1) == 1
-- 'vertexCount'   (edge e 1 2) == 2
-- @
edge :: (Eq e, Monoid e, Ord a) => e -> a -> a -> AdjacencyMap e a
edge e x y | e == zero = vertices [x, y]
           | x == y    = AM $ Map.singleton x (Map.singleton x e)
           | otherwise = AM $ Map.fromList [(x, Map.singleton y e), (y, Map.empty)]

-- | The left-hand part of a convenient ternary-ish operator @x-\<e\>-y@ for
-- creating labelled edges.
--
-- @
-- x -\<e\>- y == 'edge' e x y
-- @
(-<) :: a -> e -> (a, e)
g -< e = (g, e)

-- | The right-hand part of a convenient ternary-ish operator @x-\<e\>-y@ for
-- creating labelled edges.
--
-- @
-- x -\<e\>- y == 'edge' e x y
-- @
(>-) :: (Eq e, Monoid e, Ord a) => (a, e) -> a -> AdjacencyMap e a
(x, e) >- y = edge e x y

infixl 5 -<
infixl 5 >-

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
--
-- Note: 'overlay' composes parallel edges using the operator '<+>':
--
-- @
-- overlay ('edge' e x y) ('edge' f x y) == 'edge' (e '<+>' f) x y
-- @
--
-- Furthermore, when applied to transitive graphs, 'overlay' composes edges in
-- sequence using the operator '<.>':
--
-- @
-- 'transitiveClosure' (overlay ('edge' e 1 2) ('edge' f 2 3)) == 'overlays' ['edge' e 1 2, 'edge' f 2 3, 'edge' (e '<.>' f) 1 3]
-- @
overlay :: (Eq e, Monoid e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
overlay (AM x) (AM y) = AM $ Map.unionWith nonZeroUnion x y

-- Union maps, removing zero elements from the result.
nonZeroUnion :: (Eq e, Monoid e, Ord a) => Map a e -> Map a e -> Map a e
nonZeroUnion x y = Map.filter (/= zero) $ Map.unionWith mappend x y

-- | /Connect/ two graphs with edges labelled by a given label. When applied to
-- the same labels, this is an associative operation with the identity 'empty',
-- which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'isEmpty'     (connect e x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect e x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect e x y) >= 'vertexCount' x
-- 'vertexCount' (connect e x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect e x y) <= 'vertexCount' x * 'vertexCount' y + 'edgeCount' x + 'edgeCount' y
-- 'vertexCount' (connect e 1 2) == 2
-- 'edgeCount'   (connect e 1 2) == if e == 'zero' then 0 else 1
-- @
connect :: (Eq e, Monoid e, Ord a) => e -> AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
connect e (AM x) (AM y)
    | e == mempty = overlay (AM x) (AM y)
    | otherwise   = AM $ Map.unionsWith nonZeroUnion
        [ x, y, Map.fromSet (const targets) (Map.keysSet x) ]
  where
    targets = Map.fromSet (const e) (Map.keysSet y)

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Ord a => [a] -> AdjacencyMap e a
vertices = AM . Map.fromList . map (, Map.empty)

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []        == 'empty'
-- edges [(e,x,y)] == 'edge' e x y
-- edges           == 'overlays' . 'map' (\\(e, x, y) -> 'edge' e x y)
-- @
edges :: (Eq e, Monoid e, Ord a) => [(e, a, a)] -> AdjacencyMap e a
edges = fromAdjacencyMaps . concatMap fromEdge
  where
    fromEdge (e, x, y) | e == zero = [(x, Map.empty), (y, Map.empty)]
                       | otherwise = [(x, Map.singleton y e)]

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
overlays :: (Ord a, Semigroup e) => [AdjacencyMap e a] -> AdjacencyMap e a
overlays = AM . Map.unionsWith (Map.unionWith (<+>)) . map adjacencyMap

-- | Construct a graph from a list of adjacency sets.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
fromAdjacencyMaps :: (Ord a, Eq e, Monoid e) => [(a, Map a e)] -> AdjacencyMap e a
fromAdjacencyMaps ss = AM $ Map.unionWith (Map.unionWith mappend) vs es
  where
    vs = Map.fromSet (const Map.empty) . Set.unions $ map (Map.keysSet . snd) ss
    es = Map.fromListWith (Map.unionWith mappend) $ map (fmap $ Map.filter (/= zero)) ss

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O(s + m * log(m))/ time. Note that the number of edges /m/ of a
-- graph can be quadratic with respect to the expression size /s/.
--
-- @
-- isSubgraphOf 'empty'      x     ==  True
-- isSubgraphOf ('vertex' x) 'empty' ==  False
-- isSubgraphOf x y              ==> x <= y
-- @
isSubgraphOf :: (Eq e, Monoid e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a -> Bool
isSubgraphOf (AM x) (AM y) = Map.isSubmapOfBy (Map.isSubmapOfBy le) x y
  where
    le x y = mappend x y == y

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                         == True
-- isEmpty ('overlay' 'empty' 'empty')         == True
-- isEmpty ('vertex' x)                    == False
-- isEmpty ('removeVertex' x $ 'vertex' x)   == True
-- isEmpty ('removeEdge' x y $ 'edge' e x y) == False
-- @
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
edgeLabel :: (Monoid e, Ord a) => a -> a -> AdjacencyMap e a -> e
edgeLabel x y (AM m) = fromMaybe zero (Map.lookup x m >>= Map.lookup y)

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
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

-- | Convert to unlabelled adjacency map.
skeleton :: AdjacencyMap Any a -> AM.AdjacencyMap a
skeleton (AM m) = AM.AM (Map.map Map.keysSet m)

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
replaceVertex :: (Ord a, Semigroup e) => a -> a -> AdjacencyMap e a -> AdjacencyMap e a
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Replace an edge from a given graph. If it doesn't exist, it will be created.
-- Complexity: /O(log(n))/ time.
--
-- @
-- replaceEdge e x y m                 == 'overlay' (removeEdge x y m) ('edge' e x y)
-- replaceEdge e x y ('edge' f x y)      == 'edge' e x y
-- 'edgeLabel' x y (replaceEdge e x y m) == e
-- @
replaceEdge :: (Eq e, Monoid e, Ord a) => e -> a -> a -> AdjacencyMap e a -> AdjacencyMap e a
replaceEdge e x y
    | e == zero  = AM . addY . Map.alter (Just . maybe Map.empty (Map.delete y)) x . adjacencyMap
    | otherwise  = AM . addY . Map.alter replace x . adjacencyMap
  where
    addY             = Map.alter (Just . fromMaybe Map.empty) y
    replace (Just m) = Just $ Map.insert y e m
    replace Nothing  = Just $ Map.singleton y e

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
mergeVertices :: (Ord a, Semigroup e) => (a -> Bool) -> a -> AdjacencyMap e a -> AdjacencyMap e a
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
transpose :: (Ord a, Semigroup e) => AdjacencyMap e a -> AdjacencyMap e a
transpose (AM m) = AM $ Map.foldrWithKey combine vs m
  where
    combine v es = Map.unionWith (Map.unionWith (<+>)) $
        Map.fromAscList [ (u, Map.singleton v e) | (u, e) <- Map.toAscList es ]
    vs = Map.fromSet (const Map.empty) (Map.keysSet m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
gmap :: (Ord a, Ord b, Semigroup e) => (a -> b) -> AdjacencyMap e a -> AdjacencyMap e b
gmap f = AM . Map.map (Map.mapKeysWith (<+>) f) .
    Map.mapKeysWith (Map.unionWith (<+>)) f . adjacencyMap

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

-- | Compute the /reflexive and transitive closure/ of a graph over the
-- underlying star semiring using the Warshall-Floyd-Kleene algorithm.
closure :: (Eq e, Ord a, StarSemiring e) => AdjacencyMap e a -> AdjacencyMap e a
closure = goWarshallFloydKleene . reflexiveClosure

-- | Compute the /reflexive closure/ of a graph over the underlying semiring by
-- adding a self-loop of weight 'one' to every vertex.
reflexiveClosure :: (Ord a, Semiring e) => AdjacencyMap e a -> AdjacencyMap e a
reflexiveClosure (AM m) = AM $ Map.mapWithKey (\k -> Map.insertWith (<+>) k one) m

-- | Compute the /symmetric closure/ of a graph by overlaying it with its own
-- transpose.
symmetricClosure :: (Eq e, Monoid e, Ord a) => AdjacencyMap e a -> AdjacencyMap e a
symmetricClosure m = overlay m (transpose m)

-- | Compute the /transitive closure/ of a graph over the underlying star
-- semiring using a modified version of the Warshall-Floyd-Kleene algorithm,
-- which omits the reflexivity step.
transitiveClosure :: (Eq e, Ord a, StarSemiring e) => AdjacencyMap e a -> AdjacencyMap e a
transitiveClosure = goWarshallFloydKleene

-- The iterative part of the Warshall-Floyd-Kleene algorithm
goWarshallFloydKleene :: (Eq e, Ord a, StarSemiring e) => AdjacencyMap e a -> AdjacencyMap e a
goWarshallFloydKleene (AM m) = AM $ foldr update m vs
  where
    vs = Set.toAscList (Map.keysSet m)
    update k cur = Map.fromAscList [ (i, go i (get i k <.> starkk)) | i <- vs ]
      where
        get i j = edgeLabel i j (AM cur)
        starkk  = star (get k k)
        go i ik = Map.fromAscList
            [ (j, e) | j <- vs, let e = get i j <+> ik <.> get k j, e /= zero ]
