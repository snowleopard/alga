{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.AdjacencyMap (
    AdjacencyMap, adjacencyMap, mapVertices, vertexSet
    ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map, keysSet, fromSet)
import qualified Data.Map.Strict as Map
import Test.QuickCheck

import Algebra.Graph

newtype AdjacencyMap a = AM { adjacencyMap :: Map a (Set a) }
    deriving (Arbitrary, Eq, Show)

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

mapVertices :: (Ord a, Ord b) => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
mapVertices f (AM x) = AM . Map.map (Set.map f) $ Map.mapKeysWith Set.union f x

vertexSet :: AdjacencyMap a -> Set a
vertexSet (AM x) = Map.keysSet x
