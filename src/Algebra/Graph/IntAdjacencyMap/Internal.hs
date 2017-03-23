-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.IntAdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe. Where possible use non-internal module "Algebra.Graph.IntAdjacencyMap"
-- instead.
--
-----------------------------------------------------------------------------
module Algebra.Graph.IntAdjacencyMap.Internal (
    -- * Adjacency map
    IntAdjacencyMap (..), consistent,

    -- * Basic graph construction primitives
    empty, vertex, overlay, connect, vertices, edges, fromAdjacencyList,

    -- * Graph properties
    edgeList, adjacencyList,

    -- * Graph transformation
    removeVertex, removeEdge, gmap, induce
  ) where

import Data.IntMap.Strict (IntMap, keysSet, fromSet)
import Data.IntSet (IntSet)

import qualified Algebra.Graph.Class as C
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.IntSet         as IntSet

{-| The 'IntAdjacencyMap' data type represents a graph by a map of vertices to
their adjacency sets. We define a law-abiding 'Num' instance as a convenient
notation for working with graphs:

    > 0           == vertex 0
    > 1 + 2       == overlay (vertex 1) (vertex 2)
    > 1 * 2       == connect (vertex 1) (vertex 2)
    > 1 + 2 * 3   == overlay (vertex 1) (connect (vertex 2) (vertex 3))
    > 1 * (2 + 3) == connect (vertex 1) (overlay (vertex 2) (vertex 3))

The 'Show' instance is defined using basic graph construction primitives:

@show ('empty'     :: IntAdjacencyMap Int) == "empty"
show (1         :: IntAdjacencyMap Int) == "vertex 1"
show (1 + 2     :: IntAdjacencyMap Int) == "vertices [1,2]"
show (1 * 2     :: IntAdjacencyMap Int) == "edge 1 2"
show (1 * 2 * 3 :: IntAdjacencyMap Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: IntAdjacencyMap Int) == "graph [1,2,3] [(1,2)]"@

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
newtype IntAdjacencyMap = IntAdjacencyMap {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: IntMap IntSet
  } deriving Eq

instance Show IntAdjacencyMap where
    show a@(IntAdjacencyMap m)
        | m == IntMap.empty = "empty"
        | es == []       = if IntSet.size vs > 1 then "vertices " ++ show (IntSet.toAscList vs)
                                              else "vertex "   ++ show v
        | vs == related  = if length es > 1 then "edges " ++ show es
                                            else "edge "  ++ show e ++ " " ++ show f
        | otherwise      = "graph " ++ show (IntSet.toAscList vs) ++ " " ++ show es
      where
        vs      = keysSet m
        es      = edgeList a
        v       = head $ IntSet.toList vs
        (e,f)   = head es
        related = IntSet.fromList . uncurry (++) $ unzip es

instance C.Graph IntAdjacencyMap where
    type Vertex IntAdjacencyMap = Int
    empty   = empty
    vertex  = vertex
    overlay = overlay
    connect = connect

instance Num IntAdjacencyMap where
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
-- consistent ('IntAdjacencyMap.edge' x y)             == True
-- consistent ('edges' xs)             == True
-- consistent ('IntAdjacencyMap.graph' xs ys)          == True
-- consistent ('fromAdjacencyList' xs) == True
-- @
consistent :: IntAdjacencyMap -> Bool
consistent m = IntSet.fromList (uncurry (++) $ unzip $ edgeList m)
    `IntSet.isSubsetOf` keysSet (adjacencyMap m)

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'IntAdjacencyMap.isEmpty'     empty == True
-- 'IntAdjacencyMap.hasVertex' x empty == False
-- 'IntAdjacencyMap.vertexCount' empty == 0
-- 'IntAdjacencyMap.edgeCount'   empty == 0
-- @
empty :: IntAdjacencyMap
empty = IntAdjacencyMap $ IntMap.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'IntAdjacencyMap.isEmpty'     (vertex x) == False
-- 'IntAdjacencyMap.hasVertex' x (vertex x) == True
-- 'IntAdjacencyMap.hasVertex' 1 (vertex 2) == False
-- 'IntAdjacencyMap.vertexCount' (vertex x) == 1
-- 'IntAdjacencyMap.edgeCount'   (vertex x) == 0
-- @
vertex :: Int -> IntAdjacencyMap
vertex x = IntAdjacencyMap $ IntMap.singleton x IntSet.empty

-- | /Overlay/ two graphs. This is an idempotent, commutative and associative
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'IntAdjacencyMap.isEmpty'     (overlay x y) == 'IntAdjacencyMap.isEmpty'   x   && 'IntAdjacencyMap.isEmpty'   y
-- 'IntAdjacencyMap.hasVertex' z (overlay x y) == 'IntAdjacencyMap.hasVertex' z x || 'IntAdjacencyMap.hasVertex' z y
-- 'IntAdjacencyMap.vertexCount' (overlay x y) >= 'IntAdjacencyMap.vertexCount' x
-- 'IntAdjacencyMap.vertexCount' (overlay x y) <= 'IntAdjacencyMap.vertexCount' x + 'IntAdjacencyMap.vertexCount' y
-- 'IntAdjacencyMap.edgeCount'   (overlay x y) >= 'IntAdjacencyMap.edgeCount' x
-- 'IntAdjacencyMap.edgeCount'   (overlay x y) <= 'IntAdjacencyMap.edgeCount' x   + 'IntAdjacencyMap.edgeCount' y
-- 'IntAdjacencyMap.vertexCount' (overlay 1 2) == 2
-- 'IntAdjacencyMap.edgeCount'   (overlay 1 2) == 0
-- @
overlay :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
overlay x y = IntAdjacencyMap $ IntMap.unionWith IntSet.union (adjacencyMap x) (adjacencyMap y)

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over the overlay and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- 'IntAdjacencyMap.isEmpty'     (connect x y) == 'IntAdjacencyMap.isEmpty'   x   && 'IntAdjacencyMap.isEmpty'   y
-- 'IntAdjacencyMap.hasVertex' z (connect x y) == 'IntAdjacencyMap.hasVertex' z x || 'IntAdjacencyMap.hasVertex' z y
-- 'IntAdjacencyMap.vertexCount' (connect x y) >= 'IntAdjacencyMap.vertexCount' x
-- 'IntAdjacencyMap.vertexCount' (connect x y) <= 'IntAdjacencyMap.vertexCount' x + 'IntAdjacencyMap.vertexCount' y
-- 'IntAdjacencyMap.edgeCount'   (connect x y) >= 'IntAdjacencyMap.edgeCount' x
-- 'IntAdjacencyMap.edgeCount'   (connect x y) >= 'IntAdjacencyMap.edgeCount' y
-- 'IntAdjacencyMap.edgeCount'   (connect x y) >= 'IntAdjacencyMap.vertexCount' x * 'IntAdjacencyMap.vertexCount' y
-- 'IntAdjacencyMap.edgeCount'   (connect x y) <= 'IntAdjacencyMap.vertexCount' x * 'IntAdjacencyMap.vertexCount' y + 'IntAdjacencyMap.edgeCount' x + 'IntAdjacencyMap.edgeCount' y
-- 'IntAdjacencyMap.vertexCount' (connect 1 2) == 2
-- 'IntAdjacencyMap.edgeCount'   (connect 1 2) == 1
-- @
connect :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
connect x y = IntAdjacencyMap $ IntMap.unionsWith IntSet.union [ adjacencyMap x, adjacencyMap y,
    fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'IntAdjacencyMap.hasVertex' x . vertices == 'elem' x
-- 'IntAdjacencyMap.vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'IntAdjacencyMap.vertexIntSet'   . vertices == IntSet.'IntSet.fromList'
-- @
vertices :: [Int] -> IntAdjacencyMap
vertices = IntAdjacencyMap . IntMap.fromList . map (\x -> (x, IntSet.empty))

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'IntAdjacencyMap.edge' x y
-- 'IntAdjacencyMap.edgeCount' . edges == 'length' . 'Data.List.nub'
-- 'edgeList' . edges  == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: [(Int, Int)] -> IntAdjacencyMap
edges = fromAdjacencyList . map (fmap return)

-- | Construct a graph from an adjacency list.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyList []                                  == 'empty'
-- fromAdjacencyList [(x, [])]                           == 'vertex' x
-- fromAdjacencyList [(x, [y])]                          == 'IntAdjacencyMap.edge' x y
-- fromAdjacencyList . 'adjacencyList'                     == id
-- 'overlay' (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)
-- @
fromAdjacencyList :: [(Int, [Int])] -> IntAdjacencyMap
fromAdjacencyList as = IntAdjacencyMap $ IntMap.unionWith IntSet.union vs es
  where
    ss = map (fmap IntSet.fromList) as
    vs = fromSet (const IntSet.empty) . IntSet.unions $ map snd ss
    es = IntMap.fromListWith IntSet.union ss

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('IntAdjacencyMap.edge' x y)     == [(x,y)]
-- edgeList ('IntAdjacencyMap.star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: IntAdjacencyMap -> [(Int, Int)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'               == []
-- adjacencyList ('vertex' x)          == [(x, [])]
-- adjacencyList ('IntAdjacencyMap.edge' 1 2)          == [(1, [2]), (2, [])]
-- adjacencyList ('IntAdjacencyMap.star' 2 [1,3])      == [(1, []), (2, [1,3]), (3, [])]
-- 'fromAdjacencyList' . adjacencyList == id
-- @
adjacencyList :: IntAdjacencyMap -> [(Int, [Int])]
adjacencyList = map (fmap IntSet.toAscList) . IntMap.toAscList . adjacencyMap

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Int -> IntAdjacencyMap -> IntAdjacencyMap
removeVertex x = IntAdjacencyMap . IntMap.map (IntSet.delete x) . IntMap.delete x . adjacencyMap

-- | Remove an edge from a given graph.
-- Complexity: /O(log(n))/ time.
--
-- @
-- removeEdge x y ('IntAdjacencyMap.edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Int -> Int -> IntAdjacencyMap -> IntAdjacencyMap
removeEdge x y = IntAdjacencyMap . IntMap.adjust (IntSet.delete y) x . adjacencyMap

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'IntAdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('IntAdjacencyMap.edge' x y) == 'IntAdjacencyMap.edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Int -> Int) -> IntAdjacencyMap -> IntAdjacencyMap
gmap f = IntAdjacencyMap . IntMap.map (IntSet.map f) . IntMap.mapKeysWith IntSet.union f . adjacencyMap

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
-- 'IntAdjacencyMap.isSubgraphOf' (induce p x) x == True
-- @
induce :: (Int -> Bool) -> IntAdjacencyMap -> IntAdjacencyMap
induce p = IntAdjacencyMap . IntMap.map (IntSet.filter p) . IntMap.filterWithKey (\k _ -> p k) . adjacencyMap

