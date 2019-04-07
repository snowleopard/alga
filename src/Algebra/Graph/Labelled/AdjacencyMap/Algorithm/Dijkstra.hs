module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Algebra.Graph.Label (Semiring(..), zero)
import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))
import Data.List (minimumBy)
import Data.Map.Strict (Map, (!), (\\))
import qualified Data.Map.Strict as Map

dijkstra ::
     (Ord a, Ord b, Semiring b) => a -> AdjacencyMap b a -> Maybe (Map a b)
dijkstra k m =
  if Map.member k $ adjacencyMap m
    then Just $ recurseDijkstraStep (Map.singleton k one, Map.empty)
    else Nothing
  where
    recurseDijkstraStep (w, a)
      | w == Map.empty = a
      | otherwise = recurseDijkstraStep $ dijkstraStep (w, a) m

dijkstraStep ::
     (Ord a, Ord b, Semiring b)
  => (Map a b, Map a b)
  -> AdjacencyMap b a
  -> (Map a b, Map a b)
dijkstraStep wA@(w, a) m =
  case minValuedElemKey w of
    Nothing -> (w, addUnreachableVertices a m)
    Just kV -> dijkstraStepNonEmpty wA m kV

mapUnionMin :: (Ord a, Ord b) => Map a b -> Map a b -> Map a b
mapUnionMin = Map.unionWith min2
  where
    min2 a b
      | a < b = a
      | otherwise = b

addUnreachableVertices ::
     (Ord a, Ord b, Monoid b) => Map a b -> AdjacencyMap b a -> Map a b
addUnreachableVertices a m = mapUnionMin a allVertices
  where
    allVertices = Map.map (const zero) $ adjacencyMap m

dijkstraStepNonEmpty ::
     (Ord a, Ord b, Semiring b)
  => (Map a b, Map a b)
  -> AdjacencyMap b a
  -> (a, b)
  -> (Map a b, Map a b)
dijkstraStepNonEmpty (w, a) m (k, v) = (nW, nA)
  where
    nA = Map.insert k v a
    nW = mapUnionMin (Map.delete k w) (addWeight $ adjacencyMap m ! k) \\ nA
    addWeight = Map.map (<.> v)

minValuedElemKey :: (Ord v) => Map k v -> Maybe (k, v)
minValuedElemKey m =
  case Map.toList m of
    [] -> Nothing
    l -> Just $ minimumBy compareSnd l
  where
    compareSnd a b = compare (snd a) (snd b)
