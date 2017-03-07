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

    -- * Operations on adjacency maps
    gmap, adjacencyList, edgeList,
    fromAdjacencyList, edges, postset, toKL, toKLvia, fromKL
  ) where

import Data.Array
import qualified Data.Graph as KL -- KL stands for King-Launchbury graphs
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Graph.AdjacencyMap.Internal

adjacencyList :: AdjacencyMap a -> [(a, [a])]
adjacencyList = map (fmap Set.toAscList) . Map.toAscList . adjacencyMap

edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList

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
