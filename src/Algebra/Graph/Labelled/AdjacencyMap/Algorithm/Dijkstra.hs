module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))
import Data.List (minimumBy)
import Data.Map.Strict (Map, (!), (\\))
import qualified Data.Map.Strict as Map
import Algebra.Graph.Label (Semiring(..), Distance, infinite, distance)

dijkstra :: (Ord a, Ord b, Num b) => a -> AdjacencyMap (Distance b) a -> Maybe (Map a (Distance b))
dijkstra k m =
  if Map.member k $ adjacencyMap m
    then Just $ recurseDijkstraStep (Map.singleton k one, Map.empty)
    else Nothing
  where
    recurseDijkstraStep (w, a)
      | w == Map.empty = a
      | otherwise = recurseDijkstraStep $ dijkstraStep (w, a) m

dijkstraStep ::
     (Ord a, Ord b, Num b)
  => (Map a (Distance b), Map a (Distance b))
  -> AdjacencyMap (Distance b) a
  -> (Map a (Distance b), Map a (Distance b))
dijkstraStep wA@(w, a) m =
  case minValuedElemKey w of
    Nothing -> (w, addUnreachableVertices a m)
    Just kV -> dijkstraStepNonEmpty wA m kV

mapUnionMin :: (Ord a, Ord b) => Map a (Distance b) -> Map a (Distance b) -> Map a(Distance b)
mapUnionMin = Map.unionWith min2
  where
    min2 a b
      | a < b = a
      | otherwise = b

addUnreachableVertices ::
     (Ord a, Ord b) => Map a (Distance b) -> AdjacencyMap (Distance b) a -> Map a(Distance b)
addUnreachableVertices a m = mapUnionMin a allVertices
  where
    allVertices = Map.map (const $ distance infinite) $ adjacencyMap m

dijkstraStepNonEmpty ::
     (Ord a, Num b, Ord b)
  => (Map a (Distance b), Map a (Distance b))
  -> AdjacencyMap (Distance b) a
  -> (a, Distance b)
  -> (Map a (Distance b), Map a (Distance b))
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
