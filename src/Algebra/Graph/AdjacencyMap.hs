{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.AdjacencyMap (
    AdjacencyMap, adjacencyMap, mapVertices, vertexSet, adjacencyList, edgeList,
    fromAdjacencyList, fromEdgeList, postset, transpose, toKL, toKLvia, fromKL
    ) where

import Data.Array
import qualified Data.Graph as KL -- KL stands for King-Launchbury graphs
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

fromAdjacencyList :: Ord a => [(a, [a])] -> AdjacencyMap a
fromAdjacencyList = AM . Map.fromAscList . map (\(x, ys) -> (x, Set.fromList ys))

edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList

fromEdgeList :: Ord a => [(a, a)] -> AdjacencyMap a
fromEdgeList = AM . Map.fromListWith Set.union . map (\(x, y) -> (x, Set.singleton y))

postset :: Ord a => a -> AdjacencyMap a -> Set a
postset x = Map.findWithDefault Set.empty x . adjacencyMap

transpose :: Ord a => AdjacencyMap a -> AdjacencyMap a
transpose = fromEdgeList . map swap . edgeList

toKLvia :: Ord b => (a -> b) -> (b -> a) -> AdjacencyMap a -> (KL.Graph, KL.Vertex -> a)
toKLvia a2b b2a x = (g, \v -> case r v of (_, u, _) -> b2a u)
  where
    (g, r) = KL.graphFromEdges' [ ((), a2b v, map a2b us) | (v, us) <- adjacencyList x ]

toKL :: Ord a => AdjacencyMap a -> (KL.Graph, KL.Vertex -> a)
toKL = toKLvia id id

fromKL :: Ord a => KL.Graph -> (KL.Vertex -> a) -> AdjacencyMap a
fromKL g r = fromAdjacencyList . map (\(x, ys) -> (r x, map r ys)) $ assocs g
