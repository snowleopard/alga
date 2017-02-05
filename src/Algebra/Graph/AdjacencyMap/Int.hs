module Algebra.Graph.AdjacencyMap.Int (
    AdjacencyMap, adjacencyMap, mapVertices, adjacencyList, edgeList,
    fromAdjacencyList, fromEdgeList, postset, transpose, toKL, toKLvia, fromKL
    ) where

import Data.Array
import qualified Data.Graph as KL -- KL stands for King-Launchbury graphs
import           Data.IntMap.Strict (IntMap, keysSet, fromSet)
import qualified Data.IntMap.Strict as Map
import           Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Tuple
import Test.QuickCheck

import Algebra.Graph hiding (fromEdgeList)

newtype AdjacencyMap = AM { adjacencyMap :: IntMap IntSet }
    deriving (Arbitrary, Eq, Show)

instance Graph AdjacencyMap where
    type Vertex AdjacencyMap = Int
    empty       = AM $ Map.empty
    vertex  x   = AM $ Map.singleton x Set.empty
    overlay x y = AM $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)
    connect x y = AM $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance Num AdjacencyMap where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

mapVertices :: (Int -> Int) -> AdjacencyMap -> AdjacencyMap
mapVertices f (AM x) = AM . Map.map (Set.map f) $ Map.mapKeysWith Set.union f x

adjacencyList :: AdjacencyMap -> [(Int, [Int])]
adjacencyList = map (fmap Set.toAscList) . Map.toAscList . adjacencyMap

fromAdjacencyList :: [(Int, [Int])] -> AdjacencyMap
fromAdjacencyList = AM . Map.fromAscList . map (\(x, ys) -> (x, Set.fromList ys))

edgeList :: AdjacencyMap -> [(Int, Int)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList

fromEdgeList :: [(Int, Int)] -> AdjacencyMap
fromEdgeList = AM . Map.fromListWith Set.union . map (\(x, y) -> (x, Set.singleton y))

postset :: Int -> AdjacencyMap -> IntSet
postset x = Map.findWithDefault Set.empty x . adjacencyMap

transpose :: AdjacencyMap -> AdjacencyMap
transpose = fromEdgeList . map swap . edgeList

toKLvia :: Ord a => (Int -> a) -> (a -> Int) -> AdjacencyMap -> (KL.Graph, KL.Vertex -> Int)
toKLvia i2a a2i x = (g, \v -> case r v of (_, u, _) -> a2i u)
  where
    (g, r) = KL.graphFromEdges' [ ((), i2a v, map i2a us) | (v, us) <- adjacencyList x ]

toKL :: AdjacencyMap -> (KL.Graph, KL.Vertex -> Int)
toKL = toKLvia id id

fromKL :: KL.Graph -> (KL.Vertex -> Int) -> AdjacencyMap
fromKL g r = fromAdjacencyList . map (\(x, ys) -> (r x, map r ys)) $ assocs g
