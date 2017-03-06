-----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Graph.AdjacencyMap.Internal
-- Copyright   :  (c) Andrey Mokhov 2016-2017
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe. Where possible use non-internal module "Algebra.Graph.AdjacencyMap"
-- instead.
--
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.Internal (
    -- * Adjacency maps
    AdjacencyMap (..), gmap, fromAdjacencyList, edges
  ) where

import           Data.Map.Strict (Map, keysSet, fromSet)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import Algebra.Graph.Classes

-- | The 'AdjacencyMap' data type represents a graph by a map of vertices
-- to their adjacency sets.
newtype AdjacencyMap a = AdjacencyMap { adjacencyMap :: Map a (Set a) }
    deriving (Eq, Show)

instance Ord a => Graph (AdjacencyMap a) where
    type Vertex (AdjacencyMap a) = a
    empty       = AdjacencyMap $ Map.empty
    vertex  x   = AdjacencyMap $ Map.singleton x Set.empty
    overlay x y = AdjacencyMap $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)
    connect x y = AdjacencyMap $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

gmap :: (Ord a, Ord b) => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap f = AdjacencyMap . Map.map (Set.map f) . Map.mapKeysWith Set.union f . adjacencyMap

fromAdjacencyList :: Ord a => [(a, [a])] -> AdjacencyMap a
fromAdjacencyList = AdjacencyMap . Map.fromAscList . map (\(x, ys) -> (x, Set.fromList ys))

edges :: Ord a => [(a, a)] -> AdjacencyMap a
edges = AdjacencyMap . Map.fromListWith Set.union . map (\(x, y) -> (x, Set.singleton y))
