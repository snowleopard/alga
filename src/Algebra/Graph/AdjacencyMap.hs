{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.AdjacencyMap (
    AdjacencyMap, adjacencyMap, mapVertices, vertexSet, adjacencyList, edgeList,
    postset, fromEdgeList, transpose
    ) where

import           Data.Map.Strict (Map, keysSet, fromSet)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import Test.QuickCheck

import Algebra.Graph hiding (fromEdgeList)

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

adjacencyList :: AdjacencyMap a -> [(a, [a])]
adjacencyList = map (fmap Set.toAscList) . Map.toAscList . adjacencyMap

edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList

fromEdgeList :: Ord a => [(a, a)] -> AdjacencyMap a
fromEdgeList = AM . Map.fromListWith Set.union . map (\(x, y) -> (x, Set.singleton y))

postset :: Ord a => a -> AdjacencyMap a -> Set a
postset x = Map.findWithDefault Set.empty x . adjacencyMap

transpose :: Ord a => AdjacencyMap a -> AdjacencyMap a
transpose = fromEdgeList . map swap . edgeList
