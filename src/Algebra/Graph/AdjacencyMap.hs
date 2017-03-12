-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of adjacency maps.
--
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, adjacencyMap,

    -- * Properties of adjacency maps
    isEmpty, hasVertex, hasEdge, toSet,

    -- * Operations on adjacency maps
    gmap, edgeList, edges, adjacencyList, fromAdjacencyList,
    postset, toKL, toKLvia, fromKL
  ) where

import Data.Array
import qualified Data.Graph as KL -- KL stands for King-Launchbury graphs
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Graph.AdjacencyMap.Internal

-- | Check if a graph is empty.
--
-- @
-- isEmpty 'Algebra.Graph.empty'      == True
-- isEmpty ('Algebra.Graph.vertex' x) == False
-- @
isEmpty :: AdjacencyMap a -> Bool
isEmpty = Map.null . adjacencyMap

-- | Check if a graph contains a given vertex.
--
-- @
-- hasVertex x 'Algebra.Graph.empty'      == False
-- hasVertex x ('Algebra.Graph.vertex' x) == True
-- @
hasVertex :: Ord a => a -> AdjacencyMap a -> Bool
hasVertex v = Map.member v . adjacencyMap

-- | Check if a graph contains a given edge.
--
-- @
-- hasEdge x y 'Algebra.Graph.empty'      == False
-- hasEdge x y ('Algebra.Graph.vertex' z) == False
-- hasEdge x y ('Algebra.Graph.edge' x y) == True
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap a -> Bool
hasEdge u v a = case Map.lookup u (adjacencyMap a) of
    Nothing -> False
    Just vs -> Set.member v vs

-- | The set of vertices of a given graph.
--
-- @
-- toSet 'Algebra.Graph.empty'         == Set.empty
-- toSet ('Algebra.Graph.vertex' x)    == Set.singleton x
-- toSet ('Algebra.Graph.vertices' xs) == Set.fromList xs
-- toSet ('Algebra.Graph.clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => AdjacencyMap a -> Set.Set a
toSet = Map.keysSet . adjacencyMap

-- | The /postset/ of a vertex @x@ is the set of its /direct successors/.
--
-- @
-- postset x 'Algebra.Graph.empty'      == Set.empty
-- postset x ('Algebra.Graph.vertex' x) == Set.empty
-- postset x ('Algebra.Graph.edge' x y) == Set.fromList [y]
-- postset y ('Algebra.Graph.edge' x y) == Set.empty
-- @
postset :: Ord a => a -> AdjacencyMap a -> Set a
postset x = Map.findWithDefault Set.empty x . adjacencyMap

toKLvia :: Ord b => (a -> b) -> (b -> a) -> AdjacencyMap a -> (KL.Graph, KL.Vertex -> a)
toKLvia a2b b2a x = (g, \v -> case r v of (_, u, _) -> b2a u)
  where
    (g, r) = KL.graphFromEdges' [ ((), a2b v, map a2b us) | (v, us) <- adjacencyList x ]

toKL :: Ord a => AdjacencyMap a -> (KL.Graph, KL.Vertex -> a)
toKL = toKLvia id id

fromKL :: Ord a => KL.Graph -> (KL.Vertex -> a) -> AdjacencyMap a
fromKL g r = fromAdjacencyList . map (\(x, ys) -> (r x, map r ys)) $ assocs g
