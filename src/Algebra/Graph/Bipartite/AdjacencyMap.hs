module Algebra.Graph.Bipartite.AdjacencyMap (
    AdjacencyMap, leftToRight, rightToLeft,
    consistent,
    fromAdjacencyMap, fromGraph,
    hasEdge,
) where

import Data.Tuple

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

data AdjacencyMap a b = BAM {
    leftToRight :: Map.Map a (Set.Set b),
    rightToLeft :: Map.Map b (Set.Set a)
} deriving Eq

consistent :: (Ord a, Ord b) => AdjacencyMap a b -> Bool
consistent (BAM lr rl) = internalEdgeList lr == (map swap $ internalEdgeList rl)

internalEdgeList :: Map.Map a (Set.Set b) -> [(a, b)]
internalEdgeList lr = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

hasEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> Bool
hasEdge u v (BAM m _) = ((Set.member v) <$> (u `Map.lookup` m)) == Just True

fromAdjacencyMap :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AdjacencyMap a b
fromAdjacencyMap m = BAM (Map.fromAscList [ (u, filterRight vs) | (Left  u, vs) <- Map.toAscList (AM.adjacencyMap m)])
                         (Map.fromAscList [ (u, filterLeft  vs) | (Right u, vs) <- Map.toAscList (AM.adjacencyMap m)])
    where
        filterRight s = Set.fromAscList [ v | Right v <- Set.toAscList s ]
        filterLeft  s = Set.fromAscList [ v | Left  v <- Set.toAscList s ]

fromGraph :: (Ord a, Ord b) => G.Graph (Either a b) -> AdjacencyMap a b
fromGraph = fromAdjacencyMap . (G.foldg AM.empty AM.vertex AM.overlay AM.connect)
