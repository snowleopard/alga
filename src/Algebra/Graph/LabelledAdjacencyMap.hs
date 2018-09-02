-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'LabelledAdjacencyMap' data type, as well as associated
-- operations and algorithms. 'LabelledAdjacencyMap' is an instance of the 'C.Graph' type
-- class, which can be used for polymorphic graph construction and manipulation.
-- "Algebra.Graph.IntAdjacencyMap" defines adjacency maps specialised to graphs
-- with @Int@ vertices.
-----------------------------------------------------------------------------
module Algebra.Graph.LabelledAdjacencyMap
        (
    -- * Data structure
          LabelledAdjacencyMap
        , labelledAdjacencyMap
        ,
       -- * Basic graph construction primitives
          empty
        , vertex
        , overlay
        , connect
        , edge
        , vertices
        , edges
        , overlays
        , connects
        ,

    -- * Relations on graphs
          isSubgraphOf
        , isAcyclic
        ,

    -- * Graph properties
          isEmpty
        , hasVertex
        , hasEdge
        , vertexCount
        , edgeCount
        , vertexList
        , edgeList
        , adjacencyList
        , vertexSet
        , edgeSet
        , postSet
        , preSet
        , 

    -- * Standard families of graphs
          path
        , circuit
        , clique
        , biclique
        , star
        , stars
        , starTranspose
        , tree
        , forest
        ,

    -- * Graph transformation
          removeVertex
        , removeEdge
        , replaceVertex
        , mergeVertices
        , transpose
        , gmap
        , gemap
        , induce
        ,

    -- * Algorithms
          dfsForest
        , dfsForestFrom
        , dfs
        , topSort
        , isTopSortOf
        , scc
        )
where

import           Data.Foldable                  ( toList )
import           Data.Maybe
import           Data.Set                       ( Set )
import           Data.Tree
import           Algebra.Graph.Labelled         ( Dioid(..)
                                                , zero
                                                )

import           Algebra.Graph.LabelledAdjacencyMap.Internal

import qualified Data.Graph.Typed as Typed
import qualified Data.Graph                    as KL
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

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
edge :: (Ord a, Dioid e) => a -> a -> LabelledAdjacencyMap a e
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
vertices :: Ord a => [a] -> LabelledAdjacencyMap a e
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
edges :: (Ord a, Dioid e) => [(a, a)] -> LabelledAdjacencyMap a e
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
        :: (Ord a, Dioid e)
        => [LabelledAdjacencyMap a e]
        -> LabelledAdjacencyMap a e
overlays = LAM . Map.unionsWith (Map.unionWith (|+|)) . map labelledAdjacencyMap


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
        => [LabelledAdjacencyMap a e]
        -> LabelledAdjacencyMap a e
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
        => LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
        -> Bool
isSubgraphOf x y = Map.isSubmapOfBy Map.isSubmapOf
                                    (labelledAdjacencyMap x)
                                    (labelledAdjacencyMap y)

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
isEmpty :: LabelledAdjacencyMap a e -> Bool
isEmpty = Map.null . labelledAdjacencyMap

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Ord a => a -> LabelledAdjacencyMap a e -> Bool
hasVertex x = Map.member x . labelledAdjacencyMap

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
hasEdge :: Ord a => a -> a -> LabelledAdjacencyMap a e -> Bool
hasEdge u v a = case Map.lookup u (labelledAdjacencyMap a) of
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
vertexCount :: LabelledAdjacencyMap a e -> Int
vertexCount = Map.size . labelledAdjacencyMap

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: LabelledAdjacencyMap a e -> Int
edgeCount = Map.foldr (\es r -> (Map.size es + r)) 0 . labelledAdjacencyMap

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: LabelledAdjacencyMap a e -> [a]
vertexList = Map.keys . labelledAdjacencyMap

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
edgeList :: LabelledAdjacencyMap a e -> [(a, a)]
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
adjacencyList :: LabelledAdjacencyMap a e -> [(a, [a])]
adjacencyList =
        map (fmap (map fst . Map.toAscList))
                . Map.toAscList
                . labelledAdjacencyMap

-- | The /preset/ of an element @x@ is the set of its /direct predecessors/.
-- Complexity: /O(n * log(n))/ time and /O(n)/ memory.
--
-- @
-- preSet x 'empty'      == Set.'Set.empty'
-- preSet x ('vertex' x) == Set.'Set.empty'
-- preSet 1 ('edge' 1 2) == Set.'Set.empty'
-- preSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preSet :: Ord a => a -> LabelledAdjacencyMap a e -> Set.Set a
preSet x = Set.fromAscList . map fst . filter p . Map.toAscList . labelledAdjacencyMap
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
vertexSet :: LabelledAdjacencyMap a e -> Set a
vertexSet = Map.keysSet . labelledAdjacencyMap

-- | The set of edges of a given graph.
-- Complexity: /O((n + m) * log(m))/ time and /O(m)/ memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Ord a => LabelledAdjacencyMap a e -> Set (a, a)
edgeSet =
  Map.foldrWithKey
    (\v es -> Set.union (Set.mapMonotonic (v, ) (Map.keysSet es)))
    Set.empty .
  labelledAdjacencyMap


-- | The /postset/ (here 'postSet') of a vertex is the set of its /direct successors/.
--
-- @
-- postSet x 'empty'      == Set.'Set.empty'
-- postSet x ('vertex' x) == Set.'Set.empty'
-- postSet x ('edge' x y) == Set.'Set.fromList' [y]
-- postSet 2 ('edge' 1 2) == Set.'Set.empty'
-- @
postSet :: Ord a => a -> LabelledAdjacencyMap a e -> Set a
postSet x =
        Map.keysSet . Map.findWithDefault Map.empty x . labelledAdjacencyMap

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path :: (Ord a, Dioid e) => [a] -> LabelledAdjacencyMap a e
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
circuit :: (Ord a, Dioid e) => [a] -> LabelledAdjacencyMap a e
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
clique :: (Ord a, Dioid e) => [a] -> LabelledAdjacencyMap a e
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
biclique :: (Ord a, Dioid e) => [a] -> [a] -> LabelledAdjacencyMap a e
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
star :: (Ord a, Dioid e) => a -> [a] -> LabelledAdjacencyMap a e
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
stars :: (Ord a, Dioid e) => [(a, [a])] -> LabelledAdjacencyMap a e
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
starTranspose :: (Ord a, Dioid e) => a -> [a] -> LabelledAdjacencyMap a e
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
tree :: (Ord a, Dioid e) => Tree a -> LabelledAdjacencyMap a e
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
forest :: (Ord a, Dioid e) => Forest a -> LabelledAdjacencyMap a e
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
        :: Ord a => a -> LabelledAdjacencyMap a e -> LabelledAdjacencyMap a e
removeVertex x =
        LAM . Map.map (Map.delete x) . Map.delete x . labelledAdjacencyMap

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
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
removeEdge x y = LAM . Map.adjust (Map.delete y) x . labelledAdjacencyMap

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
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
replaceVertex u v = gmap $ \w -> if w == u then v else w
-- TODO: replaceVertexWithLabel?
-- TODO: replaceLabel

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
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
mergeVertices p v = gmap $ \u -> if p u then v else u
-- TODO: mergeVerticesAndLabel p v l =
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
        :: (Ord a, Dioid e)
        => LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
transpose (LAM m) = LAM $ Map.foldrWithKey combine vs m
    where
        combine
                :: (Ord a, Dioid e)
                => a
                -> Map.Map a e
                -> Map.Map a (Map.Map a e)
                -> Map.Map a (Map.Map a e)
        combine v es = Map.unionWith
                (Map.unionWith (|+|))
                (Map.fromSet (const $ Map.singleton v zero) (Map.keysSet es))
        vs = Map.fromSet (const Map.empty) (Map.keysSet m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'LabelledAdjacencyMap'.
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
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap b e
gmap f =
        LAM
                . Map.map (Map.mapKeys f)
                . Map.mapKeysWith Map.union f
                . labelledAdjacencyMap
                
-- | Transform a graph by applying a function to each of its edge labels. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'LabelledAdjacencyMap'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gemap f 'empty'      == 'empty'
-- gemap f ('vertex' x) == 'vertex' (f x)
-- gemap f ('edge' x y) == 'edge' (f x) (f y)
-- gemap id           == id
-- gemap f . gmap g   == gmap (f . g)
-- @
gemap :: (e -> e') -> LabelledAdjacencyMap a e -> LabelledAdjacencyMap a e'
gemap f = LAM . Map.map (Map.map f) . labelledAdjacencyMap

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
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
induce p =
        LAM
                . Map.map (Map.filterWithKey (\k _ -> p k))
                . Map.filterWithKey (\k _ -> p k)
                . labelledAdjacencyMap



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
dfsForest :: (Ord a) => LabelledAdjacencyMap a e -> Forest a
dfsForest g = dfsForestFrom (vertexList g) g

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
dfsForestFrom :: (Ord a) => [a] -> LabelledAdjacencyMap a e -> Forest a
dfsForestFrom vs = Typed.dfsForestFrom vs . Typed.fromLabelledAdjacencyMap

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
dfs ::  (Ord a) => [a] -> LabelledAdjacencyMap a e -> [a]
dfs vs = concatMap flatten . dfsForestFrom vs

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)             == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)             == Nothing
-- fmap (flip 'isTopSort' x) (topSort x) /= Just False
-- @
topSort :: Ord a => LabelledAdjacencyMap a e -> Maybe [a]
topSort m = if isTopSortOf result m then Just result else Nothing
  where
    result = Typed.topSort (Typed.fromLabelledAdjacencyMap m)

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
isTopSortOf :: Ord a => [a] -> LabelledAdjacencyMap a e -> Bool
isTopSortOf xs m = go Set.empty xs
    where
        go seen [] = seen == Map.keysSet (labelledAdjacencyMap m)
        go seen (v : vs) =
                let newSeen = seen `seq` Set.insert v seen
                in  postSet v m
                    `Set.intersection` newSeen
                    ==                 Set.empty
                    &&                 go newSeen vs

-- | Check if a given graph is /acyclic/.
--
-- @
-- isAcyclic (1 * 2 + 3 * 1) == True
-- isAcyclic (1 * 2 + 2 * 1) == False
-- isAcyclic . 'circuit'       == 'null'
-- isAcyclic                 == 'isJust' . 'topSort'
-- @
isAcyclic :: Ord a => LabelledAdjacencyMap a e -> Bool
isAcyclic = isJust . topSort

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
scc :: Ord a => LabelledAdjacencyMap a e -> LabelledAdjacencyMap (Set a) e
scc m = gmap (\v -> Map.findWithDefault Set.empty v components) m
  where
    (Typed.GraphKL g r _) = Typed.fromLabelledAdjacencyMap m
    components = Map.fromList $ concatMap (expand . fmap r . toList) (KL.scc g)
    expand xs  = let s = Set.fromList xs in map (, s) xs
