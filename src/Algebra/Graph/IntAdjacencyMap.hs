-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.IntAdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'IntAdjacencyMap' data type, as well as associated
-- operations and algorithms. 'IntAdjacencyMap' is an instance of the 'C.Graph'
-- type class, which can be used for polymorphic graph construction
-- and manipulation. See "Algebra.Graph.AdjacencyMap" for graphs with
-- non-@Int@ vertices.
-----------------------------------------------------------------------------
module Algebra.Graph.IntAdjacencyMap (
    -- * Data structure
    IntAdjacencyMap, adjacencyMap,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,
    graph, fromAdjacencyList,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexIntSet, edgeSet, postIntSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, gmap, induce,

    -- * Algorithms
    dfsForest, topSort, isTopSort,

    -- * Interoperability with King-Launchbury graphs
    GraphKL, getGraph, getVertex, graphKL, fromGraphKL
  ) where

import Data.Array
import Data.IntSet (IntSet)
import Data.Set (Set)
import Data.Tree

import Algebra.Graph.IntAdjacencyMap.Internal

import qualified Algebra.Graph.Class as C
import qualified Data.Graph          as KL
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.IntSet         as IntSet
import qualified Data.Set            as Set

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: IntAdjacencyMap
empty = C.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex x) == True
-- 'hasVertex' 1 (vertex 2) == False
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: Int -> IntAdjacencyMap
vertex = C.vertex

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Int -> Int -> IntAdjacencyMap
edge = C.edge

-- | /Overlay/ two graphs. This is an idempotent, commutative and associative
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
overlay :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
overlay = C.overlay

-- | /Connect/ two graphs. This is an associative operation with the identity
-- 'empty', which distributes over the overlay and obeys the decomposition axiom.
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
-- 'edgeCount'   (connect x y) >= 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (connect x y) <= 'vertexCount' x * 'vertexCount' y + 'edgeCount' x + 'edgeCount' y
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- @
connect :: IntAdjacencyMap -> IntAdjacencyMap -> IntAdjacencyMap
connect = C.connect

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []             == 'empty'
-- vertices [x]            == 'vertex' x
-- 'hasVertex' x  . vertices == 'elem' x
-- 'vertexCount'  . vertices == 'length' . 'Data.List.nub'
-- 'vertexIntSet' . vertices == IntSet.'IntSet.fromList'
-- @
vertices :: [Int] -> IntAdjacencyMap
vertices = IntAdjacencyMap . IntMap.fromList . map (\x -> (x, IntSet.empty))

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- 'edgeList' . edges  == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: [(Int, Int)] -> IntAdjacencyMap
edges = fromAdjacencyList . map (fmap return)

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: [IntAdjacencyMap] -> IntAdjacencyMap
overlays = C.overlays

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: [IntAdjacencyMap] -> IntAdjacencyMap
connects = C.connects

-- | Construct the graph from given lists of vertices /V/ and edges /E/.
-- The resulting graph contains the vertices /V/ as well as all the vertices
-- referred to by the edges /E/.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- graph []  []      == 'empty'
-- graph [x] []      == 'vertex' x
-- graph []  [(x,y)] == 'edge' x y
-- graph vs  es      == 'overlay' ('vertices' vs) ('edges' es)
-- @
graph :: [Int] -> [(Int, Int)] -> IntAdjacencyMap
graph vs es = overlay (vertices vs) (edges es)

-- | Construct a graph from an adjacency list.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyList []                                  == 'empty'
-- fromAdjacencyList [(x, [])]                           == 'vertex' x
-- fromAdjacencyList [(x, [y])]                          == 'edge' x y
-- fromAdjacencyList . 'adjacencyList'                     == id
-- 'overlay' (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)
-- @
fromAdjacencyList :: [(Int, [Int])] -> IntAdjacencyMap
fromAdjacencyList as = IntAdjacencyMap $ IntMap.unionWith IntSet.union vs es
  where
    ss = map (fmap IntSet.fromList) as
    vs = IntMap.fromSet (const IntSet.empty) . IntSet.unions $ map snd ss
    es = IntMap.fromListWith IntSet.union ss

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf :: IntAdjacencyMap -> IntAdjacencyMap -> Bool
isSubgraphOf x y = IntMap.isSubmapOfBy IntSet.isSubsetOf (adjacencyMap x) (adjacencyMap y)

-- | Check if a graph is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: IntAdjacencyMap -> Bool
isEmpty = IntMap.null . adjacencyMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Int -> IntAdjacencyMap -> Bool
hasVertex x = IntMap.member x . adjacencyMap

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- @
hasEdge :: Int -> Int -> IntAdjacencyMap -> Bool
hasEdge u v a = case IntMap.lookup u (adjacencyMap a) of
    Nothing -> False
    Just vs -> IntSet.member v vs

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: IntAdjacencyMap -> Int
vertexCount = IntMap.size . adjacencyMap

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: IntAdjacencyMap -> Int
edgeCount = IntMap.foldr (\es r -> (IntSet.size es + r)) 0 . adjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: IntAdjacencyMap -> [Int]
vertexList = IntMap.keys . adjacencyMap

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: IntAdjacencyMap -> [(Int, Int)]
edgeList (IntAdjacencyMap m) = [ (x, y) | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'               == []
-- adjacencyList ('vertex' x)          == [(x, [])]
-- adjacencyList ('edge' 1 2)          == [(1, [2]), (2, [])]
-- adjacencyList ('star' 2 [3,1])      == [(1, []), (2, [1,3]), (3, [])]
-- 'fromAdjacencyList' . adjacencyList == id
-- @
adjacencyList :: IntAdjacencyMap -> [(Int, [Int])]
adjacencyList = map (fmap IntSet.toAscList) . IntMap.toAscList . adjacencyMap

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexIntSet 'empty'      == IntSet.'IntSet.empty'
-- vertexIntSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexIntSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexIntSet :: IntAdjacencyMap -> IntSet
vertexIntSet = IntMap.keysSet . adjacencyMap

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: IntAdjacencyMap -> Set (Int, Int)
edgeSet = IntMap.foldrWithKey combine Set.empty . adjacencyMap
  where
    combine u es = Set.union (Set.fromAscList [ (u, v) | v <- IntSet.toAscList es ])

-- | The /postset/ of a vertex is the set of its /direct successors/.
--
-- @
-- postIntSet x 'empty'      == IntSet.'IntSet.empty'
-- postIntSet x ('vertex' x) == IntSet.'IntSet.empty'
-- postIntSet x ('edge' x y) == IntSet.'IntSet.fromList' [y]
-- postIntSet 2 ('edge' 1 2) == IntSet.'IntSet.empty'
-- @
postIntSet :: Int -> IntAdjacencyMap -> IntSet
postIntSet x = IntMap.findWithDefault IntSet.empty x . adjacencyMap

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'vertex' x
-- path [x,y] == 'edge' x y
-- @
path :: [Int] -> IntAdjacencyMap
path = C.path

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'edge' x x
-- circuit [x,y] == 'edges' [(x,y), (y,x)]
-- @
circuit :: [Int] -> IntAdjacencyMap
circuit = C.circuit

-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- clique []      == 'empty'
-- clique [x]     == 'vertex' x
-- clique [x,y]   == 'edge' x y
-- clique [x,y,z] == 'edges' [(x,y), (x,z), (y,z)]
-- @
clique :: [Int] -> IntAdjacencyMap
clique = C.clique

-- | The /biclique/ on a list of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: [Int] -> [Int] -> IntAdjacencyMap
biclique xs ys = IntAdjacencyMap $ IntMap.fromSet adjacent (x `IntSet.union` y)
  where
    x = IntSet.fromList xs
    y = IntSet.fromList ys
    adjacent v
        | v `IntSet.member` x = y
        | otherwise        = IntSet.empty

-- | The /star/ formed by a centre vertex and a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- @
star :: Int -> [Int] -> IntAdjacencyMap
star = C.star

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Tree Int -> IntAdjacencyMap
tree = C.tree

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . map 'tree'
-- @
forest :: Forest Int -> IntAdjacencyMap
forest = C.forest

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
-- removeEdge x y ('edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Int -> Int -> IntAdjacencyMap -> IntAdjacencyMap
removeEdge x y = IntAdjacencyMap . IntMap.adjust (IntSet.delete y) x . adjacencyMap

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'IntAdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Int -> Int -> IntAdjacencyMap -> IntAdjacencyMap
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate with a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (Int -> Bool) -> Int -> IntAdjacencyMap -> IntAdjacencyMap
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'IntAdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
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
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (Int -> Bool) -> IntAdjacencyMap -> IntAdjacencyMap
induce p = IntAdjacencyMap . IntMap.map (IntSet.filter p) . IntMap.filterWithKey (\k _ -> p k) . adjacencyMap

-- | Compute the /depth-first search/ forest of a graph.
--
-- @
-- 'forest' (dfsForest $ 'edge' 1 1)         == 'vertex' 1
-- 'forest' (dfsForest $ 'edge' 1 2)         == 'edge' 1 2
-- 'forest' (dfsForest $ 'edge' 2 1)         == 'vertices' [1, 2]
-- 'isSubgraphOf' ('forest' $ dfsForest x) x == True
-- dfsForest . 'forest' . dfsForest        == dfsForest
-- dfsForest $ 3 * (1 + 4) * (1 + 5)     == [ Node { rootLabel = 1
--                                                 , subForest = [ Node { rootLabel = 5
--                                                                      , subForest = [] }]}
--                                          , Node { rootLabel = 3
--                                                 , subForest = [ Node { rootLabel = 4
--                                                                      , subForest = [] }]}]
-- @
dfsForest :: IntAdjacencyMap -> Forest Int
dfsForest m = let GraphKL g r = graphKL m in fmap (fmap r) (KL.dff g)

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)             == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)             == Nothing
-- fmap (flip 'isTopSort' x) (topSort x) /= Just False
-- @
topSort :: IntAdjacencyMap -> Maybe [Int]
topSort m = if isTopSort result m then Just result else Nothing
  where
    GraphKL g r = graphKL m
    result      = map r (KL.topSort g)

-- | Check if a given list of vertices is a valid /topological sort/ of a graph.
--
-- @
-- isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True
-- isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False
-- isTopSort []        (1 * 2 + 3 * 1) == False
-- isTopSort []        'empty'           == True
-- isTopSort [x]       ('vertex' x)      == True
-- isTopSort [x]       ('edge' x x)      == False
-- @
isTopSort :: [Int] -> IntAdjacencyMap -> Bool
isTopSort xs m = go IntSet.empty xs
  where
    go seen []     = seen == IntMap.keysSet (adjacencyMap m)
    go seen (v:vs) = let newSeen = seen `seq` IntSet.insert v seen
        in postIntSet v m `IntSet.intersection` newSeen == IntSet.empty && go newSeen vs

-- | 'GraphKL' encapsulates King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library. If @graphKL g == h@ then
-- the following holds:
--
-- @
-- map ('getVertex' h) ('Data.Graph.vertices' $ 'getGraph' h)                            == IntSet.'IntSet.toAscList' ('vertexIntSet' g)
-- map (\\(x, y) -> ('getVertex' h x, 'getVertex' h y)) ('Data.Graph.edges' $ 'getGraph' h) == 'edgeList' g
-- @
data GraphKL = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    getGraph :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @a@.
    getVertex :: KL.Vertex -> Int }

-- | Build 'GraphKL' from the adjacency map of a graph.
--
-- @
-- 'fromGraphKL' . graphKL == id
-- @
graphKL :: IntAdjacencyMap -> GraphKL
graphKL m = GraphKL g $ \u -> case r u of (_, v, _) -> v
  where
    (g, r) = KL.graphFromEdges' [ ((), v, us) | (v, us) <- adjacencyList m ]

-- | Extract the adjacency map of a King-Launchbury graph.
--
-- @
-- fromGraphKL . 'graphKL' == id
-- @
fromGraphKL :: GraphKL -> IntAdjacencyMap
fromGraphKL (GraphKL g r) = fromAdjacencyList $ map (\(x, ys) -> (r x, map r ys)) (assocs g)
