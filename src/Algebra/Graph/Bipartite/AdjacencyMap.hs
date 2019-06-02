module Algebra.Graph.Bipartite.AdjacencyMap (
    AdjacencyMap, leftToRight, rightToLeft,
    consistent,
    hasEdge,
) where

import Data.Tuple

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
