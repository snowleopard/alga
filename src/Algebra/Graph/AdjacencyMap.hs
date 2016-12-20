{-# LANGUAGE TypeFamilies #-}
module Algebra.Graph.AdjacencyMap (AdjacencyMap, adjacencyMap) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map, keysSet, fromSet)
import qualified Data.Map.Strict as Map

import Algebra.Graph

newtype AdjacencyMap a = AM { adjacencyMap :: Map a (Set a) } deriving (Eq, Show)

instance Ord a => Graph (AdjacencyMap a) where
    type Vertex (AdjacencyMap a) = a
    empty       = AM $ Map.empty
    vertex  x   = AM $ Map.singleton x Set.empty
    overlay x y = AM $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)
    connect x y = AM $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id
