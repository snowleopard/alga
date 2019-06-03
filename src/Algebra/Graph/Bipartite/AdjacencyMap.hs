module Algebra.Graph.Bipartite.AdjacencyMap (
    AdjacencyMap, leftToRight, rightToLeft,
    consistent,
    fromAdjacencyMap, toAdjacencyMap, fromGraph,
    hasEdge,
    ) where

import Data.Either (lefts, rights)
import Data.List   (sort)
import Data.Tuple  (swap)

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

data AdjacencyMap a b = BAM {
    leftToRight :: Map.Map a (Set.Set b),
    rightToLeft :: Map.Map b (Set.Set a)
} deriving (Eq, Show)

consistent :: (Ord a, Ord b) => AdjacencyMap a b -> Bool
consistent (BAM lr rl) = internalEdgeList lr == sort (map swap $ internalEdgeList rl)

internalEdgeList :: Map.Map a (Set.Set b) -> [(a, b)]
internalEdgeList lr = [ (u, v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

hasEdge :: (Ord a, Ord b) => a -> b -> AdjacencyMap a b -> Bool
hasEdge u v (BAM m _) = ((Set.member v) <$> (u `Map.lookup` m)) == Just True

addReverseEdges :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AM.AdjacencyMap (Either a b)
addReverseEdges m = AM.edges $ (AM.edgeList m) ++ [ (v, u) | (u, v) <- AM.edgeList m ]

fromAdjacencyMap :: (Ord a, Ord b) => AM.AdjacencyMap (Either a b) -> AdjacencyMap a b
fromAdjacencyMap m = BAM (Map.fromAscList [ (u, setRights vs) | (Left  u, vs) <- Map.toAscList (AM.adjacencyMap $ addReverseEdges m)])
                         (Map.fromAscList [ (u, setLefts  vs) | (Right u, vs) <- Map.toAscList (AM.adjacencyMap $ addReverseEdges m)])
    where
        setRights = Set.fromAscList . rights . Set.toAscList
        setLefts  = Set.fromAscList . lefts  . Set.toAscList

toAdjacencyMap :: (Ord a, Ord b) => AdjacencyMap a b -> AM.AdjacencyMap (Either a b)
toAdjacencyMap (BAM lr _) = AM.edges [ (Left u, Right v) | (u, vs) <- Map.toAscList lr, v <- Set.toAscList vs ]

fromGraph :: (Ord a, Ord b) => G.Graph (Either a b) -> AdjacencyMap a b
fromGraph = fromAdjacencyMap . (G.foldg AM.empty AM.vertex AM.overlay AM.connect)
