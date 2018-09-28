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
    empty, vertex, overlay, connect, edge, vertices, edges, overlays, connects,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexSet, edgeSet, postSet, preSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, starTranspose, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, replaceLabel, mergeVertices,
    transpose, gmap, emap, induce
  ) where

import Data.Set (Set)
import Data.Tree

import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap.Internal

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

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
edge :: (Ord a, Dioid e) => a -> a -> AdjacencyMap a e
edge x y | x == y    = LAM $ Map.singleton x (Map.singleton y one)
         | otherwise = LAM $ Map.fromList [(x, Map.singleton y one), (y, Map.empty)]

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
vertices :: Ord a => [a] -> AdjacencyMap a e
vertices = LAM . Map.fromList . map (, Map.empty)

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- 'edgeList' . edges  == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: (Ord a, Dioid e) => [(a, a)] -> AdjacencyMap a e
edges = fromAdjacencySets . map (fmap Set.singleton)

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
overlays
        :: (Ord a, Semilattice e)
        => [AdjacencyMap a e]
        -> AdjacencyMap a e
overlays = LAM . Map.unionsWith (Map.unionWith (\/)) . map adjacencyMap


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
connects
        :: (Ord a, Dioid e)
        => [AdjacencyMap a e]
        -> AdjacencyMap a e
connects = foldr connect empty

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second. Complexity: /O((n + m) * log(n))/
-- time.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf
        :: (Ord a, Eq e)
        => AdjacencyMap a e
        -> AdjacencyMap a e
        -> Bool
isSubgraphOf x y = Map.isSubmapOfBy Map.isSubmapOf
                                    (adjacencyMap x)
                                    (adjacencyMap y)

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
isEmpty :: AdjacencyMap a e -> Bool
isEmpty = Map.null . adjacencyMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Ord a => a -> AdjacencyMap a e -> Bool
hasVertex x = Map.member x . adjacencyMap

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
hasEdge :: Ord a => a -> a -> AdjacencyMap a e -> Bool
hasEdge u v a = case Map.lookup u (adjacencyMap a) of
        Nothing -> False
        Just vs -> Map.member v vs


-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: AdjacencyMap a e -> Int
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
edgeCount :: AdjacencyMap a e -> Int
edgeCount = Map.foldr (\es r -> (Map.size es + r)) 0 . adjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: AdjacencyMap a e -> [a]
vertexList = Map.keys . adjacencyMap

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
edgeList :: AdjacencyMap a e -> [(a, a)]
edgeList (LAM m) = do
        (x, ys) <- Map.toAscList m
        (y, _ ) <- Map.toAscList ys
        pure (x, y)

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
adjacencyList :: AdjacencyMap a e -> [(a, [a])]
adjacencyList =
        map (fmap (map fst . Map.toAscList))
                . Map.toAscList
                . adjacencyMap

-- | The /preset/ of an element @x@ is the set of its /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preSet x 'empty'      == Set.'Set.empty'
-- preSet x ('vertex' x) == Set.'Set.empty'
-- preSet 1 ('edge' 1 2) == Set.'Set.empty'
-- preSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preSet :: Ord a => a -> AdjacencyMap a e -> Set.Set a
preSet x = Set.fromAscList . map fst . filter p . Map.toAscList . adjacencyMap
  where
    p (_, set) = x `Map.member` set




-- | The set of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- vertexSet . 'clique'   == Set.'Set.fromList'
-- @
vertexSet :: AdjacencyMap a e -> Set a
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
edgeSet :: Ord a => AdjacencyMap a e -> Set (a, a)
edgeSet = Set.fromAscList . edgeList


-- | The /postset/ (here 'postSet') of a vertex is the set of its /direct successors/.
--
-- @
-- postSet x 'empty'      == Set.'Set.empty'
-- postSet x ('vertex' x) == Set.'Set.empty'
-- postSet x ('edge' x y) == Set.'Set.fromList' [y]
-- postSet 2 ('edge' 1 2) == Set.'Set.empty'
-- @
postSet :: Ord a => a -> AdjacencyMap a e -> Set a
postSet x =
        Map.keysSet . Map.findWithDefault Map.empty x . adjacencyMap

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path :: (Ord a, Dioid e) => [a] -> AdjacencyMap a e
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
circuit :: (Ord a, Dioid e) => [a] -> AdjacencyMap a e
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
clique :: (Ord a, Dioid e) => [a] -> AdjacencyMap a e
clique = fromAdjacencySets . fst . go
  where
    go []     = ([], Set.empty)
    go (x:xs) = let (res, set) = go xs in ((x, set) : res, Set.insert x set)

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
biclique :: (Ord a, Dioid e) => [a] -> [a] -> AdjacencyMap a e
biclique xs ys = LAM $ Map.fromSet adjacent (x `Set.union` y)
    where
        x = Set.fromList xs
        y = Set.fromList ys
        adjacent v | v `Set.member` x = Map.fromSet (const zero) y
                   | otherwise        = Map.empty

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: (Ord a, Dioid e) => a -> [a] -> AdjacencyMap a e
star x [] = vertex x
star x ys = connect (vertex x) (vertices ys)

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L * log(n))/ time, memory and size, where /L/ is the total
-- size of the input.
--
-- @
-- stars []                      == 'empty'
-- stars [(x, [])]               == 'vertex' x
-- stars [(x, [y])]              == 'edge' x y
-- stars [(x, ys)]               == 'star' x ys
-- stars                         == 'overlays' . map (uncurry 'star')
-- stars . 'adjacencyList'         == id
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: (Ord a, Dioid e) => [(a, [a])] -> AdjacencyMap a e
stars = fromAdjacencySets . map (fmap Set.fromList)

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
starTranspose :: (Ord a, Dioid e) => a -> [a] -> AdjacencyMap a e
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
tree :: (Ord a, Dioid e) => Tree a -> AdjacencyMap a e
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
forest :: (Ord a, Dioid e) => Forest a -> AdjacencyMap a e
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
removeVertex
        :: Ord a => a -> AdjacencyMap a e -> AdjacencyMap a e
removeVertex x =
        LAM . Map.map (Map.delete x) . Map.delete x . adjacencyMap

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
removeEdge
        :: Ord a
        => a
        -> a
        -> AdjacencyMap a e
        -> AdjacencyMap a e
removeEdge x y = LAM . Map.adjust (Map.delete y) x . adjacencyMap

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex
        :: Ord a
        => a
        -> a
        -> AdjacencyMap a e
        -> AdjacencyMap a e
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | The function @'replaceLabel' x y@ replaces label @x@ with label @y@ in a
-- given 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceLabel x x            == id
-- @
replaceLabel :: Eq e => e -> e -> AdjacencyMap a e -> AdjacencyMap a e
replaceLabel u v = emap $ \w -> if w == u then v else w

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
mergeVertices
        :: Ord a
        => (a -> Bool)
        -> a
        -> AdjacencyMap a e
        -> AdjacencyMap a e
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
transpose
        :: (Ord a, Semilattice e)
        => AdjacencyMap a e
        -> AdjacencyMap a e
transpose (LAM m) = LAM $ Map.foldrWithKey combine vs m
    where
        combine
                :: (Ord a, Semilattice e)
                => a
                -> Map.Map a e
                -> Map.Map a (Map.Map a e)
                -> Map.Map a (Map.Map a e)
        combine v es = Map.unionWith
                (Map.unionWith (\/))
                (Map.fromSet (const $ Map.singleton v zero) (Map.keysSet es))
        vs = Map.fromSet (const Map.empty) (Map.keysSet m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap
        :: (Ord a, Ord b)
        => (a -> b)
        -> AdjacencyMap a e
        -> AdjacencyMap b e
gmap f =
        LAM
                . Map.map (Map.mapKeys f)
                . Map.mapKeysWith Map.union f
                . adjacencyMap

-- | Transform a graph by applying a function to each of its edge labels. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- emap f 'empty'      == 'empty'
-- emap f ('vertex' x) == 'vertex' (f x)
-- emap f ('edge' x y) == 'edge' (f x) (f y)
-- emap id           == id
-- emap f . gmap g   == gmap (f . g)
-- @
emap :: (e -> e') -> AdjacencyMap a e -> AdjacencyMap a e'
emap f = LAM . Map.map (Map.map f) . adjacencyMap

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
induce
        :: (a -> Bool)
        -> AdjacencyMap a e
        -> AdjacencyMap a e
induce p =
        LAM
                . Map.map (Map.filterWithKey (\k _ -> p k))
                . Map.filterWithKey (\k _ -> p k)
                . adjacencyMap
