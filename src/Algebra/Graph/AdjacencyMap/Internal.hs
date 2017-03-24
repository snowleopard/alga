-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe. Where possible use non-internal module "Algebra.Graph.AdjacencyMap"
-- instead.
--
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.Internal (
    -- * Adjacency map
    AdjacencyMap (..), consistent,

    -- * Basic graph construction primitives
    empty, vertex, overlay, connect, vertices, edges, fromAdjacencyList,

    -- * Graph properties
    edgeList, adjacencyList,

    -- * Graph transformation
    removeVertex, removeEdge, gmap, induce
  ) where

import Data.Map.Strict (Map, keysSet, fromSet)
import Data.Set (Set)

import qualified Algebra.Graph.Class as C
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

{-| The 'AdjacencyMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a law-abiding 'Num' instance as a convenient
notation for working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show ('empty'     :: AdjacencyMap Int) == "empty"
show (1         :: AdjacencyMap Int) == "vertex 1"
show (1 + 2     :: AdjacencyMap Int) == "vertices [1,2]"
show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"
show (1 * 2 * 3 :: AdjacencyMap Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: AdjacencyMap Int) == "graph [1,2,3] [(1,2)]"@

The 'Eq' instance satisfies all axioms of algebraic graphs:

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is associative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

When specifying the time and memory complexity of graph algorithms, /n/ and /m/
will denote the number of vertices and edges in the graph, respectively.
-}
newtype AdjacencyMap a = AdjacencyMap {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: Map a (Set a)
  } deriving Eq

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    show a@(AdjacencyMap m)
        | m == Map.empty = "empty"
        | es == []       = if Set.size vs > 1 then "vertices " ++ show (Set.toAscList vs)
                                              else "vertex "   ++ show v
        | vs == related  = if length es > 1 then "edges " ++ show es
                                            else "edge "  ++ show e ++ " " ++ show f
        | otherwise      = "graph " ++ show (Set.toAscList vs) ++ " " ++ show es
      where
        vs      = keysSet m
        es      = edgeList a
        v       = head $ Set.toList vs
        (e,f)   = head es
        related = Set.fromList . uncurry (++) $ unzip es

instance Ord a => C.Graph (AdjacencyMap a) where
    type Vertex (AdjacencyMap a) = a
    empty   = empty
    vertex  = vertex
    overlay = overlay
    connect = connect

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
--
-- @
-- consistent 'empty'                  == True
-- consistent ('vertex' x)             == True
-- consistent ('overlay' x y)          == True
-- consistent ('connect' x y)          == True
-- consistent ('Algebra.Graph.AdjacencyMap.edge' x y)             == True
-- consistent ('edges' xs)             == True
-- consistent ('Algebra.Graph.AdjacencyMap.graph' xs ys)          == True
-- consistent ('fromAdjacencyList' xs) == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent m = Set.fromList (uncurry (++) $ unzip $ edgeList m)
    `Set.isSubsetOf` keysSet (adjacencyMap m)

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.AdjacencyMap.isEmpty'     empty == True
-- 'Algebra.Graph.AdjacencyMap.hasVertex' x empty == False
-- 'Algebra.Graph.AdjacencyMap.vertexCount' empty == 0
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   empty == 0
-- @
empty :: AdjacencyMap a
empty = AdjacencyMap $ Map.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'Algebra.Graph.AdjacencyMap.isEmpty'     (vertex x) == False
-- 'Algebra.Graph.AdjacencyMap.hasVertex' x (vertex x) == True
-- 'Algebra.Graph.AdjacencyMap.hasVertex' 1 (vertex 2) == False
-- 'Algebra.Graph.AdjacencyMap.vertexCount' (vertex x) == 1
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (vertex x) == 0
-- @
vertex :: a -> AdjacencyMap a
vertex x = AdjacencyMap $ Map.singleton x Set.empty

-- | /Overlay/ two graphs. This is an idempotent, commutative and associative
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'Algebra.Graph.AdjacencyMap.isEmpty'     (overlay x y) == 'Algebra.Graph.AdjacencyMap.isEmpty'   x   && 'Algebra.Graph.AdjacencyMap.isEmpty'   y
-- 'Algebra.Graph.AdjacencyMap.hasVertex' z (overlay x y) == 'Algebra.Graph.AdjacencyMap.hasVertex' z x || 'Algebra.Graph.AdjacencyMap.hasVertex' z y
-- 'Algebra.Graph.AdjacencyMap.vertexCount' (overlay x y) >= 'Algebra.Graph.AdjacencyMap.vertexCount' x
-- 'Algebra.Graph.AdjacencyMap.vertexCount' (overlay x y) <= 'Algebra.Graph.AdjacencyMap.vertexCount' x + 'Algebra.Graph.AdjacencyMap.vertexCount' y
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (overlay x y) >= 'Algebra.Graph.AdjacencyMap.edgeCount' x
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (overlay x y) <= 'Algebra.Graph.AdjacencyMap.edgeCount' x   + 'Algebra.Graph.AdjacencyMap.edgeCount' y
-- 'Algebra.Graph.AdjacencyMap.vertexCount' (overlay 1 2) == 2
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay x y = AdjacencyMap $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over the overlay and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'Algebra.Graph.AdjacencyMap.isEmpty'     (connect x y) == 'Algebra.Graph.AdjacencyMap.isEmpty'   x   && 'Algebra.Graph.AdjacencyMap.isEmpty'   y
-- 'Algebra.Graph.AdjacencyMap.hasVertex' z (connect x y) == 'Algebra.Graph.AdjacencyMap.hasVertex' z x || 'Algebra.Graph.AdjacencyMap.hasVertex' z y
-- 'Algebra.Graph.AdjacencyMap.vertexCount' (connect x y) >= 'Algebra.Graph.AdjacencyMap.vertexCount' x
-- 'Algebra.Graph.AdjacencyMap.vertexCount' (connect x y) <= 'Algebra.Graph.AdjacencyMap.vertexCount' x + 'Algebra.Graph.AdjacencyMap.vertexCount' y
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (connect x y) >= 'Algebra.Graph.AdjacencyMap.edgeCount' x
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (connect x y) >= 'Algebra.Graph.AdjacencyMap.edgeCount' y
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (connect x y) >= 'Algebra.Graph.AdjacencyMap.vertexCount' x * 'Algebra.Graph.AdjacencyMap.vertexCount' y
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (connect x y) <= 'Algebra.Graph.AdjacencyMap.vertexCount' x * 'Algebra.Graph.AdjacencyMap.vertexCount' y + 'Algebra.Graph.AdjacencyMap.edgeCount' x + 'Algebra.Graph.AdjacencyMap.edgeCount' y
-- 'Algebra.Graph.AdjacencyMap.vertexCount' (connect 1 2) == 2
-- 'Algebra.Graph.AdjacencyMap.edgeCount'   (connect 1 2) == 1
-- @
connect :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect x y = AdjacencyMap $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
    fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'Algebra.Graph.AdjacencyMap.hasVertex' x . vertices == 'elem' x
-- 'Algebra.Graph.AdjacencyMap.vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'Algebra.Graph.AdjacencyMap.vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Ord a => [a] -> AdjacencyMap a
vertices = AdjacencyMap . Map.fromList . map (\x -> (x, Set.empty))

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'Algebra.Graph.AdjacencyMap.edge' x y
-- 'Algebra.Graph.AdjacencyMap.edgeCount' . edges == 'length' . 'Data.List.nub'
-- 'edgeList' . edges  == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: Ord a => [(a, a)] -> AdjacencyMap a
edges = fromAdjacencyList . map (fmap return)

-- | Construct a graph from an adjacency list.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyList []                                  == 'empty'
-- fromAdjacencyList [(x, [])]                           == 'vertex' x
-- fromAdjacencyList [(x, [y])]                          == 'Algebra.Graph.AdjacencyMap.edge' x y
-- fromAdjacencyList . 'adjacencyList'                     == id
-- 'overlay' (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)
-- @
fromAdjacencyList :: Ord a => [(a, [a])] -> AdjacencyMap a
fromAdjacencyList as = AdjacencyMap $ Map.unionWith Set.union vs es
  where
    ss = map (fmap Set.fromList) as
    vs = fromSet (const Set.empty) . Set.unions $ map snd ss
    es = Map.fromListWith Set.union ss

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('Algebra.Graph.AdjacencyMap.edge' x y)     == [(x,y)]
-- edgeList ('Algebra.Graph.AdjacencyMap.star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'               == []
-- adjacencyList ('vertex' x)          == [(x, [])]
-- adjacencyList ('Algebra.Graph.AdjacencyMap.edge' 1 2)          == [(1, [2]), (2, [])]
-- adjacencyList ('Algebra.Graph.AdjacencyMap.star' 2 [3,1])      == [(1, []), (2, [1,3]), (3, [])]
-- 'fromAdjacencyList' . adjacencyList == id
-- @
adjacencyList :: AdjacencyMap a -> [(a, [a])]
adjacencyList = map (fmap Set.toAscList) . Map.toAscList . adjacencyMap

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
removeVertex x = AdjacencyMap . Map.map (Set.delete x) . Map.delete x . adjacencyMap

-- | Remove an edge from a given graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge x y ('Algebra.Graph.AdjacencyMap.edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
removeEdge x y = AdjacencyMap . Map.adjust (Set.delete y) x . adjacencyMap

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('Algebra.Graph.AdjacencyMap.edge' x y) == 'Algebra.Graph.AdjacencyMap.edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap f = AdjacencyMap . Map.map (Set.map f) . Map.mapKeysWith Set.union f . adjacencyMap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce (const True)  x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'Algebra.Graph.AdjacencyMap.isSubgraphOf' (induce p x) x == True
-- @
induce :: Ord a => (a -> Bool) -> AdjacencyMap a -> AdjacencyMap a
induce p = AdjacencyMap . Map.map (Set.filter p) . Map.filterWithKey (\k _ -> p k) . adjacencyMap

