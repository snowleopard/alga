{-# LANGUAGE DeriveGeneric #-}

----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Bipartite.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for
-- the motivation behind the library, the underlying theory, and
-- implementation details.
--
-- This module defines the 'Bipartite.AdjacencyMap' for bipartite graphs data
-- type and basic associated functions.
----------------------------------------------------------------------------
module Algebra.Graph.Bipartite.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, leftToRight, rightToLeft,

    -- * Basic graph construction primitives
    empty, leftVertex, rightVertex, vertex, edge, overlay, connect, vertices, edges,
    overlays, connects,

    -- * Conversion functions
    fromAdjacencyMap, toAdjacencyMap, fromGraph,

    -- * Graph properties
    isEmpty, hasEdge, hasLeftVertex, hasRightVertex, hasVertex, leftVertexCount,
    rightVertexCount, vertexCount, edgeCount,

    -- * Miscellaneous
    consistent,
    ) where

import Data.Either (lefts, rights)
import Data.List   (sort)
import Data.Tuple  (swap)
import GHC.Generics

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

{-| The 'Bipartite.AdjacencyMap' data type represents an __undirected__
bipartite graph by two maps of vertices into its adjacency sets. The two type
parameteters define the types of identifiers of the vertices of each part.

__Note:__ even if the identifiers and their types for two vertices of
different parts are equal, these vertices are considered to be different.
See examples for more details.
-}
data AdjacencyMap a b = BAM {
    -- | The /adjacency map/ of the left part of the graph: each vertex is
    -- associated with a set of its neighbours. Complexity: /O(1)/ time and
    -- memory
    --
    -- @
    -- leftToRight 'empty'                    == Map.'Map.empty'
    -- leftToRight ('leftVertex' 1)           == Map.'Map.singleton' 1 Set.'Set.empty'
    -- leftToRight ('rightVertex' 1)          == Map.'Map.empty'
    -- leftToRight ('edge' 1 2)               == Map.'Map.singleton' 1 (Set.'Set.singleton' 2)
    -- leftToRight ('edge' 1 1)               == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- leftToRight ('edges' [(1, 1), (1, 2)]) == Map.'Map.singleton' 1 (Set.'Set.fromAscList' [1, 2])
    -- @
    leftToRight :: Map.Map a (Set.Set b),

    -- | The inverse map for 'leftToRight'. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- rightToLeft 'empty'                    == Map.'Map.empty'
    -- rightToLeft ('leftVertex' 1)           == Map.'Map.empty'
    -- rightToLeft ('rightVertex' 1)          == Map.'Map.singleton' 2 Set.'Set.empty'
    -- rightToLeft ('edge' 1 2)               == Map.'Map.singleton' 2 (Set.'Set.singleton' 1)
    -- rightToLeft ('edge' 1 1)               == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- rightToLeft ('edges' [(1, 1), (1, 2)]) == Map.'Map.fromAscList' [(1, Set.'Set.singleton' 1), (2, Set.'Set.singleton' 1)]
    -- @
    rightToLeft :: Map.Map b (Set.Set a)
} deriving (Eq, Show, Generic)

-- | Check that the internal graph representation is consistent, i.e. that all
-- edges that are present in the 'leftToRight' map are present in the
-- 'rightToLeft' map.
-- Complexity: /O(m log m)/
--
-- @
-- consistent 'empty'                 == True
-- consistent ('vertex' x)            == True
-- consistent ('edge' x y)            == True
-- consistent ('edges' xs)            == True
-- consistent ('fromGraph' g)         == True
-- consistent ('fromAdjacencyMap' am) == True
-- @
consistent :: (Ord a, Ord b) => AdjacencyMap a b -> Bool
consistent (BAM lr rl) = internalEdgeList lr == sort (map swap $ internalEdgeList rl)

-- | The list of edges of a bipartite adjacency map
--
-- /Note: this function is for internal use only./
internalEdgeList :: Map.Map a (Set.Set b) -> [(a, b)]
internalEdgeList lr = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'                                == False
-- hasEdge x y ('edge' x y)                           == True
-- hasEdge 2 3 ('edge' 1 2)                           == False
-- hasEdge x y ('overlay' g ('edge' x y))               == True
-- hasEdge 1 2 ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Left 2))) == False
-- @
hasEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> Bool
hasEdge u v (BAM m _) = ((Set.member v) <$> (u `Map.lookup` m)) == Just True

-- | Check if a graph contains a given vertex in the left part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasLeftVertex x 'empty'           == False
-- hasLeftVertex x ('leftVertex' x)  == True
-- hasLeftVertex x ('rightVertex' x) == False
-- hasLeftVertex 1 ('leftVertex' 2)  == False
-- @
hasLeftVertex :: (Ord a, Ord b) => a -> AdjacencyMap a b -> Bool
hasLeftVertex = undefined

-- | Check if a graph contains a given vertex in the right part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasRightVertex x 'empty'           == False
-- hasRightVertex x ('rightVertex' x) == True
-- hasRightVertex x ('leftVertex' x)  == False
-- hasRightVertex 1 ('rightVertex' 2) == False
-- @
hasRightVertex :: (Ord a, Ord b) => b -> AdjacencyMap a b -> Bool
hasRightVertex = undefined

-- | Check if a graph contains a given vertex in the given part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'                   == False
-- hasVertex (Right x) ('rightVertex' x) == True
-- hasVertex (Right x) ('leftVertex' x)  == False
-- hasVertex (Left 1) ('leftVertex' 2)   == False
-- hasVertex . Left                    == 'hasLeftVertex'
-- hasVertex . Right                   == 'hasRightVertex'
-- @
hasVertex :: (Ord a, Ord b) => Either a b -> AdjacencyMap a b -> Bool
hasVertex = undefined

-- | Check if a graph is empty.
-- Complecity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                 == True
-- isEmpty ('overlay' 'empty' 'empty') == True
-- isEmpty ('vertex' x)            == False
-- isEmpty                       == (==) 'empty'
-- @
isEmpty :: AdjacencyMap a b -> Bool
isEmpty = undefined

-- | The number of vertices in the left part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- leftVertexCount 'empty'         == 0
-- leftVertexCount 'leftVertex' x  == 1
-- leftVertexCount 'rightVertex' x == 0
-- leftVertexCount . 'edges'       == 'length' . 'Data.List.nub' . 'map' 'fst'
-- @
leftVertexCount :: Ord a => AdjacencyMap a b -> Int
leftVertexCount = undefined

-- | The number of vertices in the right part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- rightVertexCount 'empty'         == 0
-- rightVertexCount 'rightVertex' x == 1
-- rightVertexCount 'leftVertex' x  == 0
-- rightVertexCount . 'edges'       == 'length' . 'Data.List.nub' . 'map' 'snd'
-- @
rightVertexCount :: Ord b => AdjacencyMap a b -> Int
rightVertexCount = undefined

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'         == 0
-- vertexCount 'leftVertex' x  == 1
-- vertexCount 'rightVertex' x == 1
-- vertexCount g             == 'leftVertexCount' g + 'rightVertexCount' g
-- @
vertexCount :: (Ord a, Ord b) => AdjacencyMap a b -> Int
vertexCount = undefined

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount . 'edges'    == 'length' . 'Data.List.nub'
-- @
edgeCount :: (Ord a, Ord b) => AdjacencyMap a b -> Int
edgeCount = undefined

-- | Adds all edges needed to make the graph undirected.
-- Complexity: /O(m log n)/
--
-- /Note: this function is for internal use only./
addReverseEdges :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AM.AdjacencyMap (Either a b)
addReverseEdges m = AM.overlay m $ AM.edges [ (v, u) | (u, v) <- AM.edgeList m ]

-- | Constructs a bipartite 'Bipartite.AdjacencyMap' from
-- 'Algebra.Graph.AdjacencyMap' with given part identifiers, adding all needed
-- edges to make the graph undirected and removing all edges inside one part.
-- Complexity: /O(m log n)/
--
-- @
-- 'fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.empty')                                                     == 'empty'
-- 'leftToRight' (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right 2)))                      == Map.'Map.singleton' 1 (Set.'Set.singleton' 2)
-- fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edge' (Left 1) (Left 2))                                     == 'empty'
-- 'rightToLeft' (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edges' [(Left 1, Right 1), (Left 1, Right 2)])) == Map.'Map.fromAscList' [(1, Set.'Set.singleton' 1), (2, Set.'Set.singleton' 1)]
-- @
fromAdjacencyMap :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AdjacencyMap a b
fromAdjacencyMap m = BAM (Map.fromAscList [ (u, setRights vs) | (Left  u, vs) <- Map.toAscList (AM.adjacencyMap $ addReverseEdges m)])
                         (Map.fromAscList [ (u, setLefts  vs) | (Right u, vs) <- Map.toAscList (AM.adjacencyMap $ addReverseEdges m)])
    where
        setRights = Set.fromAscList . rights . Set.toAscList
        setLefts  = Set.fromAscList . lefts  . Set.toAscList

-- | Constructs an 'Algrebra.Graph.AdjacencyMap' from a bipartite
-- 'AdjacencyMap'.
-- Complexity: /O(m log n)/
--
-- @
-- toAdjacencyMap 'empty'          == 'Algebra.Graph.AdjacencyMap.empty'
-- toAdjacencyMap ('leftVertex' 1) == 'Algebra.Graph.AdjacencyMap.vertex' (Left 1)
-- toAdjacencyMap ('edge' 1 2)     == 'Algebra.Graph.AdjacencyMap.edges' [(Left 1, Right 2), (Right 2, Left 1)]
-- @
toAdjacencyMap :: (Ord a, Ord b) => AdjacencyMap a b -> AM.AdjacencyMap (Either a b)
toAdjacencyMap (BAM lr rl) = AM.overlays $
       [ AM.edges [ (Left u, Right v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]
       , AM.edges [ (Right u, Left v) | (u, vs) <- Map.toAscList rl, v <- Set.toAscList vs ]
       , AM.vertices $ Left  <$> Map.keys lr
       , AM.vertices $ Right <$> Map.keys rl ]

-- | Constructs a bipartite 'AdjacencyMap' from a 'Algebra.Graph.Graph' with
-- given part identifiers, adding all needed edges to make the graph undirected
-- and removing all edges inside one part.
-- Complexity: /O(m log n)/
--
-- @
-- 'leftToRight' (fromGraph ('Algebra.Graph.empty'))                                        == Map.'Map.empty'
-- 'leftToRight' (fromGraph ('Algebra.Graph.edge' (Left 1) (Right 2)))                      == Map.'Map.singleton' 1 (Set.'Set.singleton' 2)
-- 'leftToRight' (fromGraph ('Algebra.Graph.edge' (Left 1) (Left 2)))                       == Map.'Map.empty'
-- 'rightToLeft' (fromGraph ('Algebra.Graph.edges' [(Left 1, Right 1), (Left 1, Right 2)])) == Map.'Map.fromAscList' [(1, Set.'Set.singleton' 1), (2, Set.'Set.singleton' 1)]
-- @
fromGraph :: (Ord a, Ord b) => G.Graph (Either a b) -> AdjacencyMap a b
fromGraph = fromAdjacencyMap . (G.foldg AM.empty AM.vertex AM.overlay AM.connect)

-- | Constructs the /empty graph/.
-- Complexity: /O(1)/ time and memory
--
-- @
-- 'leftToRight' empty == Map.'Map.empty'
-- 'rightToLeft' empty == Map.'Map.empty'
-- 'hasVertex' x empty == False
-- @
empty :: AdjacencyMap a b
empty = undefined

-- | Constructs the bipartite graph comprising /a single isolated vertex/ in
-- the left part.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftToRight' (leftVertex x)      == Map.'Map.singleton' x Set.'Set.empty'
-- 'rightToLeft' (leftVertex x)      == Map.'Map.empty'
-- 'hasEdge' x y (leftVertex x)      == False
-- 'hasLeftVertex' x (leftVertex x)  == True
-- 'hasRightVertex' x (leftVertex x) == False
-- @
leftVertex :: a -> AdjacencyMap a b
leftVertex = undefined

-- | Constructs the bipartite graph comprising /a single isolated vertex/ in
-- the right part.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftToRight' (rightVertex x)      == Map.'Map.empty'
-- 'rightToLeft' (rightVertex x)      == Map.'Map.singleton' x Set.'Set.empty'
-- 'hasEdge' x y (rightVertex y)      == False
-- 'hasLeftVertex' x (rightVertex x)  == False
-- 'hasRightVertex' x (rightVertex x) == True
-- @
rightVertex :: b -> AdjacencyMap a b
rightVertex = undefined

-- | Constructs the bipartite graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- vertex (Left x)                == 'leftVertex' x
-- vertex (Right x)               == 'rightVertex' x
-- 'hasEdge' x y (vertex (Left x))  == False
-- 'hasEdge' x y (vertex (Right y)) == False
-- @
vertex :: Either a b -> AdjacencyMap a b
vertex = undefined

-- | Constructs the bipartite graph comprising /a single edge/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftToRight' (edge x y) == Map.'Map.singleton' x (Set.'Set.singleton' y)
-- 'rightToLeft' (edge x y) == Map.'Map.singleton' y (Set.'Set.singleton' x)
-- 'hasEdge' x y (edge x y) == True
-- 'hasEdge' y x (edge x y) == (x == y)
-- @
edge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b
edge = undefined

-- | /Overlay/ two bipartite graphs. This is a commutative, associative and
-- idempotent operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- @
overlay :: (Ord a, Ord b) => AdjacencyMap a b -> AdjacencyMap a b -> AdjacencyMap a b
overlay = undefined

-- | /Connect/ two bipartite graphs, not adding the inappropriate edges. This
-- is an associative operation with the identity 'empty', which distributes
-- over 'overlay' and obeys the decomposition axion.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices in the arguments: /m = O(m1 + m2 + l1 * r2 + l2 * r1)/.
--
-- @
-- 'isEmpty'     (connect x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y) >= 'vertexCount' x
-- 'vertexCount' (connect x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'edgeCount' x
-- 'edgeCount'   (connect x y) >= 'edgeCount' y
-- 'edgeCount'   (connect x y) >= 'leftVertexCount' x * 'rightVertexCount' y + 'rightVertexCount' x * 'leftVertexCount' y
-- 'edgeCount'   (connect x y) <= 'leftVertexCount' x * 'rightVertexCount' y + 'rightVertexCount' x * 'leftVertexCount' y + 'edgeCount' x + 'edgeCount' y
-- @
connect :: (Ord a, Ord b) => AdjacencyMap a b -> AdjacencyMap a b -> AdjacencyMap a b
connect = undefined

-- | Constructs the graph comprising two given lists of isolated vertices for
-- each part.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the total
-- length of two lists.
--
-- @
-- vertices [] []                               == 'empty'
-- vertices [x] []                              == 'leftVertex' x
-- vertices [] [x]                              == 'rightVertex' x
-- 'hasLeftVertex' x . ('flip' ('const' [])) vertices == 'elem' x
-- 'hasRightVertex' x . 'const' [] vertices         == 'elem' x
-- @
vertices :: (Ord a, Ord b) => [a] -> [b] -> AdjacencyMap a b
vertices = undefined

-- | Constructs the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges :: (Ord a, Ord b) => [(a, b)] -> AdjacencyMap a b
edges = undefined

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
overlays :: (Ord a, Ord b) => [AdjacencyMap a b] -> AdjacencyMap a b
overlays = undefined

-- | Connects a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == connect x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: (Ord a, Ord b) => [AdjacencyMap a b] -> AdjacencyMap a b
connects = undefined
