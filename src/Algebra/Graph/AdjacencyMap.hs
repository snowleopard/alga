-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'AdjacencyMap' data type, which is an abstract
-- implementation of graph adjacency maps, as well as associated operations and
-- algorithms. 'AdjacencyMap' is an instance of "Algebra.Graph.Class" type class,
-- which can be used for polymorphic graph construction and manipulation.
-- See "Algebra.Graph.IntAdjacencyMap" for adjacency maps specialised to graphs
-- with @Int@ vertices.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, adjacencyMap, empty, vertex, overlay, connect,

    -- * Basic graph construction primitives
    edge, vertices, edges, overlays, connects, graph, fromAdjacencyList,

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
    dfsForest, topSort, isTopSort, scc,

    -- * Interoperability with King-Launchbury graphs
    GraphKL, getGraph, getVertex, graphKL, fromGraphKL
  ) where

import Data.Array
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Tree

import Algebra.Graph.AdjacencyMap.Internal

import qualified Algebra.Graph.Class as C
import qualified Data.Graph          as KL
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

-- | Construct the graph comprising a single edge.
-- Complexity: /O(1)/ time, memory.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Ord a => a -> a -> AdjacencyMap a
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
overlays :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
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
connects :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
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
graph :: Ord a => [a] -> [(a, a)] -> AdjacencyMap a
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
isSubgraphOf :: Ord a => AdjacencyMap a -> AdjacencyMap a -> Bool
isSubgraphOf x y = Map.isSubmapOfBy Set.isSubsetOf (adjacencyMap x) (adjacencyMap y)

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
isEmpty :: AdjacencyMap a -> Bool
isEmpty = Map.null . adjacencyMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Ord a => a -> AdjacencyMap a -> Bool
hasVertex x = Map.member x . adjacencyMap

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap a -> Bool
hasEdge u v a = case Map.lookup u (adjacencyMap a) of
    Nothing -> False
    Just vs -> Set.member v vs

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: Ord a => AdjacencyMap a -> Int
vertexCount = Map.size . adjacencyMap

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => AdjacencyMap a -> Int
edgeCount = Map.foldr (\es r -> (Set.size es + r)) 0 . adjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Ord a => AdjacencyMap a -> [a]
vertexList = Map.keys . adjacencyMap

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- vertexSet . 'clique'   == Set.'Set.fromList'
-- @
vertexSet :: Ord a => AdjacencyMap a -> Set a
vertexSet = Map.keysSet . adjacencyMap

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Ord a => AdjacencyMap a -> Set (a, a)
edgeSet = Map.foldrWithKey (\v es -> Set.union (Set.mapMonotonic (v,) es)) Set.empty . adjacencyMap

-- | The /postset/ of a vertex is the set of its /direct successors/.
--
-- @
-- postset x 'empty'      == Set.'Set.empty'
-- postset x ('vertex' x) == Set.'Set.empty'
-- postset x ('edge' x y) == Set.'Set.fromList' [y]
-- postset 2 ('edge' 1 2) == Set.'Set.empty'
-- @
postset :: Ord a => a -> AdjacencyMap a -> Set a
postset x = Map.findWithDefault Set.empty x . adjacencyMap

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'vertex' x
-- path [x,y] == 'edge' x y
-- @
path :: Ord a => [a] -> AdjacencyMap a
path = C.path

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'edge' x x
-- circuit [x,y] == 'edges' [(x,y), (y,x)]
-- @
circuit :: Ord a => [a] -> AdjacencyMap a
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
clique :: Ord a => [a] -> AdjacencyMap a
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
biclique :: Ord a => [a] -> [a] -> AdjacencyMap a
biclique = C.biclique

-- | The /star/ formed by a centre vertex and a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- @
star :: Ord a => a -> [a] -> AdjacencyMap a
star = C.star

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
tree :: Ord a => Tree a -> AdjacencyMap a
tree = C.tree

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
forest :: Ord a => Forest a -> AdjacencyMap a
forest = C.forest

-- | The function @replaceVertex x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
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
mergeVertices :: Ord a => (a -> Bool) -> a -> AdjacencyMap a -> AdjacencyMap a
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | 'GraphKL' encapsulates King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library. If @graphKL g == h@ then
-- the following holds:
--
-- @
-- map ('getVertex' h) ('Data.Graph.vertices' $ 'getGraph' h)                            == Set.toAscList ('vertexSet' g)
-- map (\\(x, y) -> ('getVertex' h x, 'getVertex' h y)) ('Data.Graph.edges' $ 'getGraph' h) == 'edgeList' g
-- @
data GraphKL a = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    getGraph :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @a@.
    getVertex :: KL.Vertex -> a }

-- | Build 'GraphKL' from the adjacency map of a graph.
--
-- @
-- 'fromGraphKL' . graphKL == id
-- @
graphKL :: Ord a => AdjacencyMap a -> GraphKL a
graphKL m = GraphKL g $ \u -> case r u of (_, v, _) -> v
  where
    (g, r) = KL.graphFromEdges' [ ((), v, us) | (v, us) <- adjacencyList m ]

-- | Extract the adjacency map of a King-Launchbury graph.
--
-- @
-- fromGraphKL . 'graphKL' == id
-- @
fromGraphKL :: Ord a => GraphKL a -> AdjacencyMap a
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
dfsForest :: Ord a => AdjacencyMap a -> Forest a
dfsForest m = let GraphKL g r = graphKL m in fmap (fmap r) (KL.dff g)

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)             == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)             == Nothing
-- fmap (flip 'isTopSort' x) (topSort x) /= Just False
-- @
topSort :: Ord a => AdjacencyMap a -> Maybe [a]
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
isTopSort :: Ord a => [a] -> AdjacencyMap a -> Bool
isTopSort xs m = go Set.empty xs
  where
    go seen []     = seen == Map.keysSet (adjacencyMap m)
    go seen (v:vs) = let newSeen = seen `seq` Set.insert v seen
        in postset v m `Set.intersection` newSeen == Set.empty && go newSeen vs

-- | Compute the /condensation/ of a graph, where each vertex corresponds to a
-- /strongly-connected component/ of the original graph.
--
-- @
-- scc 'empty'               == 'empty'
-- scc ('vertex' x)          == 'vertex' (Set.'Set.singleton' x)
-- scc ('edge' x y)          == 'edge' (Set.'Set.singleton' x) (Set.'Set.singleton' y)
-- scc ('circuit' (1:xs))    == 'edge' (Set.'Set.fromList' (1:xs)) (Set.'Set.fromList' (1:xs))
-- scc (3 * 1 * 4 * 1 * 5) == 'edges' [ (Set.'Set.fromList' [1,4], Set.'Set.fromList' [1,4])
--                                  , (Set.'Set.fromList' [1,4], Set.'Set.fromList' [5]  )
--                                  , (Set.'Set.fromList' [3]  , Set.'Set.fromList' [1,4])
--                                  , (Set.'Set.fromList' [3]  , Set.'Set.fromList' [5]  )]
-- @
scc :: Ord a => AdjacencyMap a -> AdjacencyMap (Set a)
scc m = gmap (\v -> Map.findWithDefault Set.empty v components) m
  where
    GraphKL g r = graphKL m
    components  = Map.fromList $ concatMap (expand . fmap r . toList) (KL.scc g)
    expand xs   = let s = Set.fromList xs in map (\x -> (x, s)) xs
