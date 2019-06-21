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
-- This module defines the 'AdjacencyMap' for bipartite graphs data type and
-- basic associated functions.
----------------------------------------------------------------------------
module Algebra.Graph.Bipartite.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, leftAdjacencyMap, rightAdjacencyMap,

    -- * Basic graph construction primitives
    empty, leftVertex, rightVertex, vertex, edge, overlay, connect,
    vertices, edges, overlays, connects,

    -- * Conversion functions
    toBipartite, fromBipartite, fromGraph,

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
bipartite graph by two maps of vertices into their adjacency sets. The two type
parameteters define the types of identifiers of the vertices of each part.

__Note:__ even if the identifiers and their types for two vertices of
different parts are equal, these vertices are considered to be different.
See examples for more details.
-}
data AdjacencyMap a b = BAM {
    -- | The /adjacency map/ of the left part of the graph: each vertex is
    -- associated with a set of its neighbours. Complexity: /O(1)/ time and
    -- memory.
    --
    -- @
    -- leftAdjacencyMap 'empty'                    == Map.'Map.empty'
    -- leftAdjacencyMap ('leftVertex' 1)           == Map.'Map.singleton' 1 Set.'Set.empty'
    -- leftAdjacencyMap ('rightVertex' 1)          == Map.'Map.empty'
    -- leftAdjacencyMap ('edge' 1 1)               == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- leftAdjacencyMap ('edge' 1 "a")             == Map.'Map.singleton' 1 (Set.'Set.singleton' "a")
    -- leftAdjacencyMap ('edges' [(1, 1), (1, 2)]) == Map.'Map.singleton' 1 (Set.'Set.fromAscList' [1, 2])
    -- @
    leftAdjacencyMap :: Map.Map a (Set.Set b),

    -- | The inverse map for 'leftAdjacencyMap'. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- rightAdjacencyMap 'empty'                    == Map.'Map.empty'
    -- rightAdjacencyMap ('leftVertex' 1)           == Map.'Map.empty'
    -- rightAdjacencyMap ('rightVertex' 1)          == Map.'Map.singleton' 2 Set.'Set.empty'
    -- rightAdjacencyMap ('edge' 1 1)               == Map.'Map.singleton' 1 (Set.'Set.singleton' "a")
    -- rightAdjacencyMap ('edge' 1 "a")             == Map.'Map.singleton' "a" (Set.'Set.singleton' 1)
    -- rightAdjacencyMap ('edges' [(1, 1), (1, 2)]) == Map.'Map.fromAscList' [(1, Set.'Set.singleton' 1), (2, Set.'Set.singleton' 1)]
    -- @
    rightAdjacencyMap :: Map.Map b (Set.Set a)
} deriving (Eq, Show, Generic)

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftAdjacencyMap' empty  == Map.'Map.empty'
-- 'rightAdjacencyMap' empty == Map.'Map.empty'
-- 'hasVertex' x empty       == False
-- @
empty :: AdjacencyMap a b
empty = BAM Map.empty Map.empty

-- | Construct the bipartite graph comprising /a single isolated vertex/ in
-- the left part.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftAdjacencyMap' (leftVertex x)  == Map.'Map.singleton' x Set.'Set.empty'
-- 'rightAdjacencyMap' (leftVertex x) == Map.'Map.empty'
-- 'hasEdge' x y (leftVertex z)       == False
-- 'hasLeftVertex' x (leftVertex x)   == True
-- 'hasRightVertex' x (leftVertex x)  == False
-- @
leftVertex :: a -> AdjacencyMap a b
leftVertex x = BAM (Map.singleton x Set.empty) Map.empty

-- | Construct the bipartite graph comprising /a single isolated vertex/ in
-- the right part.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftAdjacencyMap' (rightVertex x)  == Map.'Map.empty'
-- 'rightAdjacencyMap' (rightVertex x) == Map.'Map.singleton' x Set.'Set.empty'
-- 'hasEdge' x y (rightVertex y)       == False
-- 'hasLeftVertex' x (rightVertex x)   == False
-- 'hasRightVertex' x (rightVertex x)  == True
-- @
rightVertex :: b -> AdjacencyMap a b
rightVertex y = BAM Map.empty (Map.singleton y Set.empty)

-- | Construct the bipartite graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- vertex (Left x)                == 'leftVertex' x
-- vertex (Right x)               == 'rightVertex' x
-- 'hasEdge' x y (vertex (Left x))  == False
-- 'hasEdge' x y (vertex (Right y)) == False
-- @
vertex :: Either a b -> AdjacencyMap a b
vertex (Left x)  = leftVertex x
vertex (Right y) = rightVertex y

-- | Construct the bipartite graph comprising /a single edge/.
-- Complexity: /O(1)/ time and memory.
--
-- @
-- 'leftAdjacencyMap' (edge x y)  == Map.'Map.singleton' x (Set.'Set.singleton' y)
-- 'rightAdjacencyMap' (edge x y) == Map.'Map.singleton' y (Set.'Set.singleton' x)
-- 'hasEdge' x y (edge x y)       == True
-- 'hasEdge' 1 1 (edge 1 1)       == True
-- 'hasEdge' 2 1 (edge 1 2)       == False
-- @
edge :: a -> b -> AdjacencyMap a b
edge x y = BAM (Map.singleton x (Set.singleton y)) (Map.singleton y (Set.singleton x))

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
overlay (BAM lr1 rl1) (BAM lr2 rl2) = BAM (Map.unionWith Set.union lr1 lr2) (Map.unionWith Set.union rl1 rl2)

-- | /Connect/ two bipartite graphs, not adding the edges between vertices in
-- the same part. This is a commutative and associative operation with the
-- identity 'empty', which distributes over 'overlay' and obeys the
-- decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices in the arguments: /m = O(m1 + m2 + l1 * r2 + l2 * r1)/.
--
-- @
-- connect ('leftVertex' 1) ('rightVertex' "a")      == 'edge' 1 "a"
-- connect ('leftVertex' 1) ('rightVertex' 1)        == 'edge' 1 1
-- connect ('leftVertex' 1) ('leftVertex' 2)         == 'vertices' [1, 2] []
-- connect ('vertices' [1] [4]) ('vertices' [2] [3]) == 'edges' [(1, 3), (2, 4)]
-- 'isEmpty'     (connect x y)                     == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y)                     == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y)                     >= 'vertexCount' x
-- 'vertexCount' (connect x y)                     <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y)                     >= 'edgeCount' x
-- 'edgeCount'   (connect x y)                     >= 'edgeCount' y
-- 'edgeCount'   (connect x y)                     >= 'leftVertexCount' x * 'rightVertexCount' y
-- 'edgeCount'   (connect x y)                     <= 'leftVertexCount' x * 'rightVertexCount' y + 'rightVertexCount' x * 'leftVertexCount' y + 'edgeCount' x + 'edgeCount' y
-- @
connect :: (Ord a, Ord b) => AdjacencyMap a b -> AdjacencyMap a b -> AdjacencyMap a b
connect (BAM lr1 rl1) (BAM lr2 rl2) = BAM lr rl
    where
        lr = Map.unionsWith Set.union $
            [ lr1, lr2
            , Map.fromSet (const $ Map.keysSet rl2) (Map.keysSet lr1)
            , Map.fromSet (const $ Map.keysSet rl1) (Map.keysSet lr2)
            ]
        rl = Map.unionsWith Set.union $
            [ rl1, rl2
            , Map.fromSet (const $ Map.keysSet lr2) (Map.keysSet rl1)
            , Map.fromSet (const $ Map.keysSet lr1) (Map.keysSet rl2)
            ]

-- | Construct the graph comprising two given lists of isolated vertices for
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
vertices ls rs = BAM (Map.fromList $ map ((flip (,)) Set.empty) ls) (Map.fromList $ map ((flip (,)) Set.empty) rs)

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x, y)]    == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges :: (Ord a, Ord b) => [(a, b)] -> AdjacencyMap a b
edges es = BAM (Map.fromListWith Set.union (map (onRight Set.singleton) es)) $
                Map.fromListWith Set.union (map (onRight Set.singleton) (map swap es))
    where
        onRight f (x, y) = (x, f y)

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
overlays ams = BAM (Map.unionsWith Set.union (map leftAdjacencyMap ams)) $
                    Map.unionsWith Set.union (map rightAdjacencyMap ams)

-- | Connect a given list of graphs.
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
connects = foldr connect empty

-- | Construct a bipartite 'AdjacencyMap' from "Algebra.Graph.AdjacencyMap"
-- with given part identifiers, adding all needed edges to make the graph
-- undirected and removing all edges inside one part.
-- Complexity: /O(m log(n))/.
--
-- @
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.empty')                     == 'empty'
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right 1))   == 'edge' 1 1
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.edge' (Left 1) (Left 1))    == 'empty'
-- toBipartite (Algebra.Graph.AdjacencyMap.'Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right "a")) == 'edge' 1 "a"
-- @
toBipartite :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AdjacencyMap a b
toBipartite m = BAM (Map.fromAscList [ (u, setRights vs) | (Left  u, vs) <- Map.toAscList (AM.adjacencyMap $ AM.symmetricClosure m)])
                         (Map.fromAscList [ (u, setLefts  vs) | (Right u, vs) <- Map.toAscList (AM.adjacencyMap $ AM.symmetricClosure m)])
    where
        setRights = Set.fromAscList . rights . Set.toAscList
        setLefts  = Set.fromAscList . lefts  . Set.toAscList

-- | Construct an 'Algrebra.Graph.AdjacencyMap' from a bipartite
-- 'AdjacencyMap'.
-- Complexity: /O(m log(n))/.
--
-- @
-- fromBipartite 'empty'          == 'Algebra.Graph.AdjacencyMap.empty'
-- fromBipartite ('leftVertex' 1) == 'Algebra.Graph.AdjacencyMap.vertex' (Left 1)
-- fromBipartite ('edge' 1 2)     == 'Algebra.Graph.AdjacencyMap.edges' [(Left 1, Right 2), (Right 2, Left 1)]
-- @
fromBipartite :: (Ord a, Ord b) => AdjacencyMap a b -> AM.AdjacencyMap (Either a b)
fromBipartite (BAM lr rl) = AM.overlays $
       [ AM.edges [ (Left u, Right v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]
       , AM.edges [ (Right u, Left v) | (u, vs) <- Map.toAscList rl, v <- Set.toAscList vs ]
       , AM.vertices $ Left  <$> Map.keys lr
       , AM.vertices $ Right <$> Map.keys rl ]

-- | Construct a bipartite 'AdjacencyMap' from a 'Algebra.Graph.Graph' with
-- given part identifiers, adding all needed edges to make the graph undirected
-- and removing all edges inside one part.
-- Complexity: /O(m log n)/.
--
-- @
-- fromGraph (Algebra.Graph.'Algebra.Graph.empty')                     == 'empty'
-- fromGraph (Algebra.Graph.'Algebra.Graph.edge' (Left 1) (Right 1))   == 'edge' 1 1
-- fromGraph (Algebra.Graph.'Algebra.Graph.edge' (Left 1) (Right "a")) == 'edge' 1 "a"
-- fromGraph (Algebra.Graph.'Algebra.Graph.edge' (Left 1) (Left 2))    == 'empty'
-- @
fromGraph :: (Ord a, Ord b) => G.Graph (Either a b) -> AdjacencyMap a b
fromGraph = toBipartite . (G.foldg AM.empty AM.vertex AM.overlay AM.connect)

internalEdgeList :: Map.Map a (Set.Set b) -> [(a, b)]
internalEdgeList lr = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

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
isEmpty (BAM lr rl) = Map.null lr && Map.null rl

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'                                == False
-- hasEdge x y ('edge' x y)                           == True
-- hasEdge 2 3 ('edge' 1 2)                           == False
-- hasEdge x y ('overlay' z ('edge' x y))               == True
-- hasEdge 1 2 ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Left 2))) == False
-- @
hasEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> Bool
hasEdge u v (BAM m _) = (Set.member v <$> (u `Map.lookup` m)) == Just True

-- | Check if a graph contains a given vertex in the left part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasLeftVertex x 'empty'           == False
-- hasLeftVertex x ('leftVertex' x)  == True
-- hasLeftVertex x ('rightVertex' x) == False
-- hasLeftVertex 1 ('leftVertex' 2)  == False
-- @
hasLeftVertex :: Ord a => a -> AdjacencyMap a b -> Bool
hasLeftVertex x (BAM lr _) = x `Map.member` lr

-- | Check if a graph contains a given vertex in the right part.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasRightVertex x 'empty'           == False
-- hasRightVertex x ('rightVertex' x) == True
-- hasRightVertex x ('leftVertex' x)  == False
-- hasRightVertex 1 ('rightVertex' 2) == False
-- @
hasRightVertex :: Ord b => b -> AdjacencyMap a b -> Bool
hasRightVertex y (BAM _ rl) = y `Map.member` rl

-- | Check if a graph contains a given vertex.
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
hasVertex (Left x)  = hasLeftVertex x
hasVertex (Right y) = hasRightVertex y

-- | The number of vertices in the left part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- leftVertexCount 'empty'           == 0
-- leftVertexCount ('leftVertex' x)  == 1
-- leftVertexCount ('rightVertex' x) == 0
-- leftVertexCount . 'edges'         == 'length' . 'Data.List.nub' . 'map' 'fst'
-- @
leftVertexCount :: AdjacencyMap a b -> Int
leftVertexCount = Map.size . leftAdjacencyMap

-- | The number of vertices in the right part in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- rightVertexCount 'empty'           == 0
-- rightVertexCount ('rightVertex' x) == 1
-- rightVertexCount ('leftVertex' x)  == 0
-- rightVertexCount . 'edges'         == 'length' . 'Data.List.nub' . 'map' 'snd'
-- @
rightVertexCount :: AdjacencyMap a b -> Int
rightVertexCount = Map.size . rightAdjacencyMap

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'           == 0
-- vertexCount ('leftVertex' x)  == 1
-- vertexCount ('rightVertex' x) == 1
-- vertexCount x               == 'leftVertexCount' x + 'rightVertexCount' x
-- @
vertexCount :: AdjacencyMap a b -> Int
vertexCount g = leftVertexCount g + rightVertexCount g

-- | The number of edges in a graph.
-- Complexity: /O(n)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount . 'edges'    == 'length' . 'Data.List.nub'
-- @
edgeCount :: AdjacencyMap a b -> Int
edgeCount = Map.foldr ((+) . Set.size) 0 . leftAdjacencyMap

-- | Check that the internal graph representation is consistent, i.e. that all
-- edges that are present in the 'leftAdjacencyMap' are present in the
-- 'rightAdjacencyMap' map.
--
-- @
-- consistent 'empty'            == True
-- consistent ('vertex' x)       == True
-- consistent ('edge' x y)       == True
-- consistent ('edges' xs)       == True
-- consistent ('fromGraph' g)    == True
-- consistent ('toBipartite' am) == True
-- @
consistent :: (Ord a, Ord b) => AdjacencyMap a b -> Bool
consistent (BAM lr rl) = internalEdgeList lr == sort (map swap $ internalEdgeList rl)
