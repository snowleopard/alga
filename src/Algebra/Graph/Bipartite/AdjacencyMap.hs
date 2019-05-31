module Algebra.Graph.Bipartite.AdjacencyMap (
    AdjacencyMap, leftToRight, rightToLeft,
    consistent,
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

data AdjacencyMap a b = BAM {
    leftToRight :: Map.Map a (Set.Set b),
    rightToLeft :: Map.Map b (Set.Set a)
} deriving Eq

consistent :: (Ord a, Ord b) => AdjacencyMap a b -> Bool
consistent (BAM lr rl) = oneWayConstistent lr rl && oneWayConstistent rl lr

internalEdgeList :: Map.Map a (Set.Set b) -> [(a, b)]
internalEdgeList lr = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

hasEdge :: (Ord a, Ord b) => Map.Map a (Set.Set b) -> a -> b -> Bool
hasEdge m u v = ((Set.member v) <$> (m Map.!? u)) == Just True

oneWayConstistent :: (Ord a, Ord b) => Map.Map a (Set.Set b) -> Map.Map b (Set.Set a) -> Bool
oneWayConstistent straight inverse = foldr (&&) True $ (uncurry . flip) (hasEdge inverse) <$> internalEdgeList straight
