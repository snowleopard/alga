module Algebra.Graph.Labelled.AdjacencyMap.Algorithm where

import Data.Set (Set)
import Algebra.Graph.Label
import Data.Map.Strict (Map, (!))
import Algebra.Graph.Labelled.AdjacencyMap

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- TODO: Inprove documentation for 'dijkstra'.
-- TODO: Add tests and examples for 'dijkstra'.
-- | Compute shortest path.
dijkstra :: (Ord a, Ord e, Dioid e) => AdjacencyMap e a -> a -> Map a e
dijkstra = dijkstra' zero one

-- Extended dijkstra. This function should not be exported.
dijkstra' :: (Ord a, Ord e, Dioid e) => e -> e -> AdjacencyMap e a -> a -> Map a e
dijkstra' z o am src = maybe zm (snd . processG . const processI) (Map.lookup src zm)
  where
    im = adjacencyMap am
    zm = Map.map (const zero) im
    processI = (Set.singleton (one, src), Map.insert src one zm)
    processG sm@(s, _)
      | o < z = processS (Set.minView s) sm
      | otherwise = processS (Set.maxView s) sm
    processS Nothing sm = sm
    processS (Just ((_, v1), s)) (_, m) = processG $ relaxV v1 (s, m)
    relaxV v1 sm =
      let eL = map (\(v2, e) -> (e, v1, v2)) . Map.toList $ im ! v1
      in foldr relaxE sm eL
    relaxE (e, v1, v2) (s, m) =
      let n = ((m ! v1) <.> e) <+> (m ! v2)
      in (Set.insert (n, v2) s, Map.insert v2 n m)

-- REPL Testing

x = edges
  [ (4::Distance Int, 0, 1)
  , (8, 0, 7)
  , (11, 1, 7)
  , (8, 1, 2)
  , (7, 7, 8)
  , (1, 7, 16)
  , (6, 8, 16)
  , (2, 2, 8)
  , (4, 2, 25)
  , (2, 16, 25)
  , (14, 3, 25)
  , (9, 3, 40)
  , (10, 25, 40)
  , (7, 2, 3)]
