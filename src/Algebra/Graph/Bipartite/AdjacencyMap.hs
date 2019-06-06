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

    -- * Conversion functions
    fromAdjacencyMap, toAdjacencyMap, fromGraph,

    -- * Graph properties
    hasEdge,

    -- * Miscellaneous
    consistent,
    ) where

import Data.Either (lefts, rights)
import Data.List   (sort)
import Data.Tuple  (swap)

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- | The 'Bipartite.AdjacencyMap' data type represents an __undirected__
-- bipartite graph by two maps of vertices into its adjacency sets. The two
-- type arguments for this datatype stand for the identifiers of the vertices
-- of each part.
--
--  __Note:__ even if the identifiers and their types for two vertices of
-- different parts are equal, these vertices are considered to be different.
-- See the examples for more details.
data AdjacencyMap a b = BAM {
    -- | The /adjacency map/ of the left part of the graph: each vertex is
    -- associated with a set of its neighbours. Complexity: /O(1)/ time and
    -- memory
    --
    -- @
    -- leftToRight ('fromGraph' 'Algebra.Graph.empty') == Map.'Map.empty'
    -- leftToRight ('fromGraph' ('Algebra.Graph.vertex' (Left 1))) == Map.'Map.singleton' 1 Set.'Set.empty'
    -- leftToRight ('fromGraph' ('Algebra.Graph.vertex' (Right 1))) == Map.'Map.empty'
    -- leftToRight ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Right 2))) == Map.'Map.singleton' 1 (Set.'Set.singleton' 2)
    -- leftToRight ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Right 1))) == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- leftToRight ('fromGraph' ('Algebra.Graph.edges' [(Left 1, Right 1), (Left 1, Right 2)])) == Map.'Map.singleton' 1 (Set.'Set.fromAscList' [1, 2])
    -- @
    leftToRight :: Map.Map a (Set.Set b),

    -- | The inverse map for 'leftToRight'. Complexity: /O(1)/ time and memory.
    --
    -- @
    -- rightToLeft ('fromGraph' 'Algebra.Graph.empty') == Map.'Map.empty'
    -- rightToLeft ('fromGraph' ('Algebra.Graph.vertex' (Left 1))) == Map.'Map.empty'
    -- rightToLeft ('fromGraph' ('Algebra.Graph.vertex' (Right 1))) == Map.'Map.singleton' 2 Set.'Set.empty'
    -- rightToLeft ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Right 2))) == Map.'Map.singleton' 2 (Set.'Set.singleton' 1)
    -- rightToLeft ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Right 1))) == Map.'Map.singleton' 1 (Set.'Set.singleton' 1)
    -- rightToLeft ('fromGraph' ('Algebra.Graph.edges' [(Left 1, Right 1), (Left 1, Right 2)])) == Map.'Map.fromAscList' [(1, Set.'Set.singleton' 1), (2, Set.'Set.singleton' 1)]
    -- @
    rightToLeft :: Map.Map b (Set.Set a)
} deriving (Eq, Show)

-- | Check that the internal graph representation is consistent, i.e. that all
-- edges that are present in the 'leftToRight' map are present in the
-- 'rightToLeft' map.
-- Complexity: /O(m log m)/
--
-- @
-- consistent ('fromGraph' 'Algebra.Graph.empty')         == True
-- consistent ('fromGraph' ('Algebra.Graph.vertex' x))    == True
-- consistent ('fromGraph' ('Algebra.Graph.overlay' x y)) == True
-- consistent ('fromGraph' ('Algebra.Graph.connect' x y)) == True
-- consistent ('fromGraph' ('Algebra.Graph.edge' x y))    == True
-- consistent ('fromGraph' ('Algebra.Graph.edges' xs))    == True
-- consistent ('fromGraph' ('Algebra.Graph.stars' xs))    == True
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
-- hasEdge x y ('fromGraph' 'Algebra.Graph.empty')                     == False
-- hasEdge (Left x) (Right y) ('fromGraph' ('Algebra.Graph.edge' x y)) == True
-- hasEdge 1 1 ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Right 1))) == True
-- hasEdge 1 2 ('fromGraph' ('Algebra.Graph.edge' (Left 1) (Left 2)))  == False
-- @
hasEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> Bool
hasEdge u v (BAM m _) = ((Set.member v) <$> (u `Map.lookup` m)) == Just True

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
-- 'leftToRight' (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.empty'))                   == Map.'Map.empty'
-- 'leftToRight' (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right 2))) == Map.'Map.singleton' 1 (Set.singleton 2)
-- 'leftToRight' (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edge' (Left 1) (Left 2)))  == Map.'Map.empty'
-- 'hasEdge' x y (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.empty'))                   == False
-- 'hasEdge' 1 2 (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edge' (Left 1) (Left 2)    == False
-- 'hasEdge' 1 2 (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right 2)   == True
-- 'hasEdge' 2 1 (fromAdjacencyMap ('Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right 2)   == False
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
-- toAdjacencyMap (fromGraph 'Algebra.Graph.empty')                     == 'Algebra.Graph.AdjacencyMap.empty'
-- toAdjacencyMap (fromGraph ('Algebra.Graph.edge' (Left 1) (Right 2))) == 'Algebra.Graph.AdjacencyMap.edge' (Left 1) (Right 2)
-- toAdjacencyMap (fromGraph ('Algebra.Graph.edge' (Left 1) (Left 2)))  == 'Algebra.Graph.AdjacencyMap.empty'
-- @
toAdjacencyMap :: (Ord a, Ord b) => AdjacencyMap a b -> AM.AdjacencyMap (Either a b)
toAdjacencyMap (BAM lr rl) = AM.overlays $
       [ AM.edges [ (Left u, Right v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ] ]
    ++ [ AM.edges [ (Right u, Left v) | (u, vs) <- Map.toAscList rl, v <- Set.toAscList vs ] ]
    ++ map (AM.vertex . Left)  (Map.keys lr)
    ++ map (AM.vertex . Right) (Map.keys rl)

-- | Constructs a bipartite 'AdjacencyMap' from a 'Algebra.Graph.Graph' with
-- given part identifiers, adding all needed edges to make the graph undirected
-- and removing all edges inside one part.
-- Complexity: /O(m log n)/
--
-- @
-- 'leftToRight' (fromGraph ('Algebra.Graph.Graph.empty'))                   == Map.'Map.empty'
-- 'leftToRight' (fromGraph ('Algebra.Graph.Graph.edge' (Left 1) (Right 2))) == Map.'Map.singleton' 1 (Set.singleton 2)
-- 'leftToRight' (fromGraph ('Algebra.Graph.Graph.edge' (Left 1) (Left 2)))  == Map.'Map.empty'
-- 'hasEdge' x y (fromGraph ('Algebra.Graph.Graph.empty'))                   == False
-- 'hasEdge' 1 2 (fromGraph ('Algebra.Graph.Graph.edge' (Left 1) (Left 2)    == False
-- 'hasEdge' 1 2 (fromGraph ('Algebra.Graph.Graph.edge' (Left 1) (Right 2)   == True
-- 'hasEdge' 2 1 (fromGraph ('Algebra.Graph.Graph.edge' (Left 1) (Right 2)   == False
-- @
fromGraph :: (Ord a, Ord b) => G.Graph (Either a b) -> AdjacencyMap a b
fromGraph = fromAdjacencyMap . (G.foldg AM.empty AM.vertex AM.overlay AM.connect)
