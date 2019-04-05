module Algebra.Graph.Labelled.AdjacencyMap.Algorithm where 
import qualified Algebra.Graph.Labelled.AdjacencyMap.Internal as LAMI
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List (minimumBy)

infinity :: Int
infinity = maxBound

dijkstraStep :: (Ord a) => (Map a Int, Map a Int) -> LAMI.AdjacencyMap Int a -> (Map a Int, Map a Int)
dijkstraStep wA@(w, a) m =
  case minValuedElemKey w of
    Nothing -> (w, addUnreachableVertices a m)
    Just kV -> dijkstraStepNonEmpty wA m kV

addUnreachableVertices :: (Ord a) => Map a Int -> LAMI.AdjacencyMap Int a -> Map a Int 
addUnreachableVertices a m = unionMin a allVertices
  where
    allVertices = Map.map (const infinity) $ LAMI.adjacencyMap m
    unionMin = Map.unionWith min'
    min' a b = if a < b then a else b
    
dijkstraStepNonEmpty :: (Ord a) => (Map a Int, Map a Int) -> LAMI.AdjacencyMap Int a -> (a, Int) -> (Map a Int, Map a Int)
dijkstraStepNonEmpty (w, a) m (k, v) = (nW, nA)
  where
    nW = unionMin (Map.delete k w) (addWeight $ LAMI.adjacencyMap m ! k)
    nA = Map.insert k v a
    unionMin = Map.unionWith min'
    addWeight = Map.map (+ v)
    min' a b = if a < b then a else b

minValuedElemKey :: (Ord v) => Map k v -> Maybe (k, v)
minValuedElemKey m = 
  case Map.toList m of
    [] -> Nothing
    l -> Just $ minimumBy compare' l
  where
    compare' a b = compare (snd a) (snd b)
    
