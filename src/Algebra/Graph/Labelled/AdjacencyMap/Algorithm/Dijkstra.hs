module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))
import Data.List (minimumBy)
import Data.Map.Strict (Map, (!), (\\))
import qualified Data.Map.Strict as Map

infinity :: Int
infinity = maxBound

dijkstra :: (Ord a) => a -> AdjacencyMap Int a -> Maybe (Map a Int)
dijkstra k m =
  if Map.member k $ adjacencyMap m
    then Just $ recurseDijkstraStep (Map.singleton k 0, Map.empty)
    else Nothing
  where
    recurseDijkstraStep (w, a)
      | w == Map.empty = a
      | otherwise = recurseDijkstraStep $ dijkstraStep (w, a) m

dijkstraStep ::
     (Ord a)
  => (Map a Int, Map a Int)
  -> AdjacencyMap Int a
  -> (Map a Int, Map a Int)
dijkstraStep wA@(w, a) m =
  case minValuedElemKey w of
    Nothing -> (w, addUnreachableVertices a m)
    Just kV -> dijkstraStepNonEmpty wA m kV

mapUnionMin :: (Ord a) => Map a Int -> Map a Int -> Map a Int
mapUnionMin = Map.unionWith min'
  where
    min' a b
      | a < b = a
      | otherwise = b

addUnreachableVertices ::
     (Ord a) => Map a Int -> AdjacencyMap Int a -> Map a Int
addUnreachableVertices a m = mapUnionMin a allVertices
  where
    allVertices = Map.map (const infinity) $ adjacencyMap m

dijkstraStepNonEmpty ::
     (Ord a)
  => (Map a Int, Map a Int)
  -> AdjacencyMap Int a
  -> (a, Int)
  -> (Map a Int, Map a Int)
dijkstraStepNonEmpty (w, a) m (k, v) = (nW, nA)
  where
    nA = Map.insert k v a
    nW = mapUnionMin (Map.delete k w) (addWeight $ adjacencyMap m ! k) \\ nA
    addWeight = Map.map (+ v)

minValuedElemKey :: (Ord v) => Map k v -> Maybe (k, v)
minValuedElemKey m =
  case Map.toList m of
    [] -> Nothing
    l -> Just $ minimumBy compare' l
  where
    compare' a b = compare (snd a) (snd b)
