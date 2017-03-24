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
-- operations and algorithms. 'AdjaceIntAdjacencyMapncyMap' is an instance of
-- the 'C.Graph' type class, which can be used for polymorphic graph construction
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
    adjacencyList, vertexSet, edgeSet, postset,

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
import Data.Tree

import Algebra.Graph.IntAdjacencyMap.Internal

import qualified Algebra.Graph.Class as C
import qualified Data.Graph          as KL
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.IntSet         as IntSet
import qualified Data.Set            as Set

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

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'      == IntSet.'IntSet.empty'
-- vertexSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexSet :: IntAdjacencyMap -> IntSet
vertexSet = IntMap.keysSet . adjacencyMap

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: IntAdjacencyMap -> Set.Set (Int, Int)
edgeSet = IntMap.foldrWithKey combine Set.empty . adjacencyMap
  where
    combine u es = Set.union (Set.fromAscList [ (u, v) | v <- IntSet.toAscList es ])

-- | The /postset/ of a vertex is the set of its /direct successors/.
--
-- @
-- postset x 'empty'      == IntSet.'IntSet.empty'
-- postset x ('vertex' x) == IntSet.'IntSet.empty'
-- postset x ('edge' x y) == IntSet.'IntSet.fromList' [y]
-- postset 2 ('edge' 1 2) == IntSet.'IntSet.empty'
-- @
postset :: Int -> IntAdjacencyMap -> IntSet
postset x = IntMap.findWithDefault IntSet.empty x . adjacencyMap

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
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- @
biclique :: [Int] -> [Int] -> IntAdjacencyMap
biclique = C.biclique

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
tree :: Tree Int -> IntAdjacencyMap
tree = C.tree

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
forest :: Forest Int -> IntAdjacencyMap
forest = C.forest

-- | The function @replaceVertex x y@ replaces vertex @x@ with vertex @y@ in a
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

-- | 'GraphKL' encapsulates King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library. If @graphKL g == h@ then
-- the following holds:
--
-- @
-- map ('getVertex' h) ('Data.Graph.vertices' $ 'getGraph' h)                            == IntSet.toAscList ('vertexSet' g)
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
        in postset v m `IntSet.intersection` newSeen == IntSet.empty && go newSeen vs

