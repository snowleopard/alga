-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyIntMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'AdjacencyIntMap' data type, as well as associated
-- operations and algorithms. 'AdjacencyIntMap' is an instance of the 'C.Graph'
-- type class, which can be used for polymorphic graph construction
-- and manipulation. See "Algebra.Graph.adjacencyIntMap" for graphs with
-- non-@Int@ vertices.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyIntMap (
    -- * Data structure
    AdjacencyIntMap, adjacencyIntMap,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,
    fromAdjacencyList,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, hasSelfLoop, vertexCount, edgeCount, vertexList,
    edgeList, adjacencyList, vertexIntSet, edgeSet, preIntSet, postIntSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, starTranspose, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, transpose, gmap, induce,

    -- * Algorithms
    dfsForest, dfsForestFrom, dfs, topSort, isTopSort
  ) where

import Data.IntSet (IntSet)
import Data.Set (Set)
import Data.Tree

import Algebra.Graph.AdjacencyIntMap.Internal

import qualified Data.Graph.Typed    as Typed
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
edge :: Int -> Int -> AdjacencyIntMap
edge x y | x == y    = AM $ IntMap.singleton x (IntSet.singleton y)
         | otherwise = AM $ IntMap.fromList [(x, IntSet.singleton y), (y, IntSet.empty)]

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
vertices :: [Int] -> AdjacencyIntMap
vertices = AM . IntMap.fromList . map (\x -> (x, IntSet.empty))

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- 'edgeList' . edges  == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: [(Int, Int)] -> AdjacencyIntMap
edges = fromAdjacencyIntSets . map (fmap IntSet.singleton)

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
overlays :: [AdjacencyIntMap] -> AdjacencyIntMap
overlays = AM . IntMap.unionsWith IntSet.union . map adjacencyIntMap

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
connects :: [AdjacencyIntMap] -> AdjacencyIntMap
connects  = foldr connect empty

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
fromAdjacencyList :: [(Int, [Int])] -> AdjacencyIntMap
fromAdjacencyList = fromAdjacencyIntSets . map (fmap IntSet.fromList)

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
isSubgraphOf :: AdjacencyIntMap -> AdjacencyIntMap -> Bool
isSubgraphOf x y = IntMap.isSubmapOfBy IntSet.isSubsetOf (adjacencyIntMap x) (adjacencyIntMap y)

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
isEmpty :: AdjacencyIntMap -> Bool
isEmpty = IntMap.null . adjacencyIntMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Int -> AdjacencyIntMap -> Bool
hasVertex x = IntMap.member x . adjacencyIntMap

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Int -> Int -> AdjacencyIntMap -> Bool
hasEdge u v a = case IntMap.lookup u (adjacencyIntMap a) of
    Nothing -> False
    Just vs -> IntSet.member v vs

-- | Check if a graph contains a given loop.
-- Complexity: /O(s)/ time.
--
-- @
-- hasSelfLoop x 'empty'            == False
-- hasSelfLoop x ('vertex' z)       == False
-- hasSelfLoop x ('edge' x x)       == True
-- hasSelfLoop x                  == 'hasEdge' x x
-- hasSelfLoop x . 'removeEdge' x x == const False
-- hasSelfLoop x                  == 'elem' (x,x) . 'edgeList'
-- @
hasSelfLoop :: Int -> AdjacencyIntMap -> Bool
hasSelfLoop x = hasEdge x x

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: AdjacencyIntMap -> Int
vertexCount = IntMap.size . adjacencyIntMap

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: AdjacencyIntMap -> Int
edgeCount = IntMap.foldr (\es r -> (IntSet.size es + r)) 0 . adjacencyIntMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: AdjacencyIntMap -> [Int]
vertexList = IntMap.keys . adjacencyIntMap

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- edgeList . 'transpose'    == 'Data.List.sort' . map 'Data.Tuple.swap' . edgeList
-- @
edgeList :: AdjacencyIntMap -> [(Int, Int)]
edgeList (AM m) = [ (x, y) | (x, ys) <- IntMap.toAscList m, y <- IntSet.toAscList ys ]

-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexIntSet 'empty'      == IntSet.'IntSet.empty'
-- vertexIntSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexIntSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexIntSet :: AdjacencyIntMap -> IntSet
vertexIntSet = IntMap.keysSet . adjacencyIntMap

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: AdjacencyIntMap -> Set (Int, Int)
edgeSet = IntMap.foldrWithKey combine Set.empty . adjacencyIntMap
  where
    combine u es = Set.union (Set.fromAscList [ (u, v) | v <- IntSet.toAscList es ])

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
adjacencyList :: AdjacencyIntMap -> [(Int, [Int])]
adjacencyList = map (fmap IntSet.toAscList) . IntMap.toAscList . adjacencyIntMap

-- | The /preset/ (here 'preIntSet') of an element @x@ is the set of its
-- /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preIntSet x 'empty'      == Set.'Set.empty'
-- preIntSet x ('vertex' x) == Set.'Set.empty'
-- preIntSet 1 ('edge' 1 2) == Set.'Set.empty'
-- preIntSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preIntSet :: Int -> AdjacencyIntMap -> IntSet.IntSet
preIntSet x = IntSet.fromAscList . map fst . filter p  . IntMap.toAscList . adjacencyIntMap
  where
    p (_, set) = x `IntSet.member` set

-- | The /postset/ (here 'postIntSet') of a vertex is the set of its
-- /direct successors/.
--
-- @
-- postIntSet x 'empty'      == IntSet.'IntSet.empty'
-- postIntSet x ('vertex' x) == IntSet.'IntSet.empty'
-- postIntSet x ('edge' x y) == IntSet.'IntSet.fromList' [y]
-- postIntSet 2 ('edge' 1 2) == IntSet.'IntSet.empty'
-- @
postIntSet :: Int -> AdjacencyIntMap -> IntSet
postIntSet x = IntMap.findWithDefault IntSet.empty x . adjacencyIntMap

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path :: [Int] -> AdjacencyIntMap
path xs = case xs of []     -> empty
                     [x]    -> vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edges' [(x,y), (y,x)]
-- circuit . 'reverse' == 'transpose' . circuit
-- @
circuit :: [Int] -> AdjacencyIntMap
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique . 'reverse'  == 'transpose' . clique
-- @
clique :: [Int] -> AdjacencyIntMap
clique = fromAdjacencyIntSets . fst . go
  where
    go []     = ([], IntSet.empty)
    go (x:xs) = let (res, set) = go xs in ((x, set) : res, IntSet.insert x set)

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: [Int] -> [Int] -> AdjacencyIntMap
biclique xs ys = AM $ IntMap.fromSet adjacent (x `IntSet.union` y)
  where
    x = IntSet.fromList xs
    y = IntSet.fromList ys
    adjacent v = if v `IntSet.member` x then y else IntSet.empty

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: Int -> [Int] -> AdjacencyIntMap
star x [] = vertex x
star x ys = connect (vertex x) (vertices ys)

-- | The /star transpose/ formed by a list of leaves connected to a centre vertex.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- starTranspose x []    == 'vertex' x
-- starTranspose x [y]   == 'edge' y x
-- starTranspose x [y,z] == 'edges' [(y,x), (z,x)]
-- starTranspose x ys    == 'connect' ('vertices' ys) ('vertex' x)
-- starTranspose x ys    == 'transpose' ('star' x ys)
-- @
starTranspose :: Int -> [Int] -> AdjacencyIntMap
starTranspose x [] = vertex x
starTranspose x ys = connect (vertices ys) (vertex x)

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Tree Int -> AdjacencyIntMap
tree (Node x []) = vertex x
tree (Node x f ) = star x (map rootLabel f)
    `overlay` forest (filter (not . null . subForest) f)

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . map 'tree'
-- @
forest :: Forest Int -> AdjacencyIntMap
forest = overlays . map tree

-- | Remove a vertex from a given graph.
-- Complexity: /O(n*log(n))/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Int -> AdjacencyIntMap -> AdjacencyIntMap
removeVertex x = AM . IntMap.map (IntSet.delete x) . IntMap.delete x . adjacencyIntMap

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
removeEdge :: Int -> Int -> AdjacencyIntMap -> AdjacencyIntMap
removeEdge x y = AM . IntMap.adjust (IntSet.delete y) x . adjacencyIntMap

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyIntMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Int -> Int -> AdjacencyIntMap -> AdjacencyIntMap
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: (Int -> Bool) -> Int -> AdjacencyIntMap -> AdjacencyIntMap
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transpose a given graph.
-- Complexity: /O(m * log(n))/ time, /O(n + m)/ memory.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- 'edgeList' . transpose  == 'Data.List.sort' . map 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: AdjacencyIntMap -> AdjacencyIntMap
transpose (AM m) = AM $ IntMap.foldrWithKey combine vs m
  where
    combine v es = IntMap.unionWith IntSet.union (IntMap.fromSet (const $ IntSet.singleton v) es)
    vs           = IntMap.fromSet (const IntSet.empty) (IntMap.keysSet m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyIntMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Int -> Int) -> AdjacencyIntMap -> AdjacencyIntMap
gmap f = AM . IntMap.map (IntSet.map f) . IntMap.mapKeysWith IntSet.union f . adjacencyIntMap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce (const True ) x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (Int -> Bool) -> AdjacencyIntMap -> AdjacencyIntMap
induce p = AM . IntMap.map (IntSet.filter p) . IntMap.filterWithKey (\k _ -> p k) . adjacencyIntMap

-- | Compute the /depth-first search/ forest of a graph.
--
-- @
-- 'forest' (dfsForest $ 'edge' 1 1)         == 'vertex' 1
-- 'forest' (dfsForest $ 'edge' 1 2)         == 'edge' 1 2
-- 'forest' (dfsForest $ 'edge' 2 1)         == 'vertices' [1, 2]
-- 'isSubgraphOf' ('forest' $ dfsForest x) x == True
-- dfsForest . 'forest' . dfsForest        == dfsForest
-- dfsForest ('vertices' vs)               == map (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
-- 'dfsForestFrom' ('vertexList' x) x        == dfsForest x
-- dfsForest $ 3 * (1 + 4) * (1 + 5)     == [ Node { rootLabel = 1
--                                                 , subForest = [ Node { rootLabel = 5
--                                                                      , subForest = [] }]}
--                                          , Node { rootLabel = 3
--                                                 , subForest = [ Node { rootLabel = 4
--                                                                      , subForest = [] }]}]
-- @
dfsForest :: AdjacencyIntMap -> Forest Int
dfsForest = Typed.dfsForest . Typed.fromAdjacencyIntMap

-- | Compute the /depth-first search/ forest of a graph, searching from each of
-- the given vertices in order. Note that the resulting forest does not
-- necessarily span the whole graph, as some vertices may be unreachable.
--
-- @
-- 'forest' (dfsForestFrom [1]    $ 'edge' 1 1)     == 'vertex' 1
-- 'forest' (dfsForestFrom [1]    $ 'edge' 1 2)     == 'edge' 1 2
-- 'forest' (dfsForestFrom [2]    $ 'edge' 1 2)     == 'vertex' 2
-- 'forest' (dfsForestFrom [3]    $ 'edge' 1 2)     == 'empty'
-- 'forest' (dfsForestFrom [2, 1] $ 'edge' 1 2)     == 'vertices' [1, 2]
-- 'isSubgraphOf' ('forest' $ dfsForestFrom vs x) x == True
-- dfsForestFrom ('vertexList' x) x               == 'dfsForest' x
-- dfsForestFrom vs             ('vertices' vs)   == map (\\v -> Node v []) ('Data.List.nub' vs)
-- dfsForestFrom []             x               == []
-- dfsForestFrom [1, 4] $ 3 * (1 + 4) * (1 + 5) == [ Node { rootLabel = 1
--                                                        , subForest = [ Node { rootLabel = 5
--                                                                             , subForest = [] }
--                                                 , Node { rootLabel = 4
--                                                        , subForest = [] }]
-- @
dfsForestFrom :: [Int] -> AdjacencyIntMap -> Forest Int
dfsForestFrom vs = Typed.dfsForestFrom vs . Typed.fromAdjacencyIntMap

-- | Compute the list of vertices visited by the /depth-first search/ in a graph,
-- when searching from each of the given vertices in order.
--
-- @
-- dfs [1]    $ 'edge' 1 1                == [1]
-- dfs [1]    $ 'edge' 1 2                == [1, 2]
-- dfs [2]    $ 'edge' 1 2                == [2]
-- dfs [3]    $ 'edge' 1 2                == []
-- dfs [1, 2] $ 'edge' 1 2                == [1, 2]
-- dfs [2, 1] $ 'edge' 1 2                == [2, 1]
-- dfs []     $ x                       == []
-- dfs [1, 4] $ 3 * (1 + 4) * (1 + 5)   == [1, 5, 4]
-- 'isSubgraphOf' ('vertices' $ dfs vs x) x == True
-- @
dfs :: [Int] -> AdjacencyIntMap -> [Int]
dfs vs = concatMap flatten . dfsForestFrom vs

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)             == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)             == Nothing
-- fmap (flip 'isTopSort' x) (topSort x) /= Just False
-- @
topSort :: AdjacencyIntMap -> Maybe [Int]
topSort m = if isTopSort result m then Just result else Nothing
  where
    result = Typed.topSort (Typed.fromAdjacencyIntMap m)

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
isTopSort :: [Int] -> AdjacencyIntMap -> Bool
isTopSort xs m = go IntSet.empty xs
  where
    go seen []     = seen == IntMap.keysSet (adjacencyIntMap m)
    go seen (v:vs) = let newSeen = seen `seq` IntSet.insert v seen
        in postIntSet v m `IntSet.intersection` newSeen == IntSet.empty && go newSeen vs
