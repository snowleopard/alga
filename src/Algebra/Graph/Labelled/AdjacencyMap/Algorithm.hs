module Algebra.Graph.Labelled.AdjacencyMap.Algorithm where

import Algebra.Graph.Label
import Data.Map.Strict (Map, (!))
import Algebra.Graph.Labelled.AdjacencyMap

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- TODO: Improve documentation for 'dijkstra'.
-- TODO: Change heap operations
-- | A generic Dijkstra algorithm that relaxes the list of edges
-- based on the 'Dioid'.
--
-- If the 'Dioid' is 'Distance' (negative 'Dioid') the relaxation
-- is done in ascending order.
--
-- If the 'Dioid' is 'Capacity' (positive 'Dioid') the relaxation
-- is done in descending order.
--
-- The examples below assume the edge values are 'Distance'
-- @
-- dijkstra ('edges' [(2, 'b', 'c'), (1, 'a', 'b'), (4, 'a', 'c')]) 'z' == Map.'Map.fromList' [('a', distance infinite), ('b', distance infinite), ('c', distance infinite)]
-- dijkstra ('edges' [(2, 'b', 'c'), (1, 'a', 'b'), (4, 'a', 'c')]) 'a' == Map.'Map.fromList' [('a', 0), ('b', 1), ('c', 3)]
-- @
--
-- The examples below assume the edge values are 'Capacity'
-- @
-- dijkstra ('edges' [(2, 'b', 'c'), (1, 'a', 'b'), (4, 'a', 'c')]) 'z' == Map.'Map.fromList' [('a', 0), ('b', 0), ('c', 0)]
-- dijkstra ('edges' [(2, 'b', 'c'), (1, 'a', 'b'), (4, 'a', 'c')]) 'a' == Map.'Map.fromList' [('a', capacity infinite), ('b', 1), ('c', 4)]
-- @
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

-- TODO: Improve documentation for bellmanFord
-- TODO: Write tests and examples for bellmanFord
-- TODO: safely change 'vL' to 'tail vL' in processL
bellmanFord :: (Ord a, Dioid e) => a -> AdjacencyMap e a -> Map a e
bellmanFord src wam = maybe zm processL im
  where
    am = adjacencyMap wam
    zm = Map.map (const zero) am
    im = Map.insert src one zm <$ Map.lookup src zm
    vL = Map.keys am
    processL m = foldr (const processR) m vL
    processR m = foldr relaxV m vL
    relaxV v1 m =
      let eL = map (\(v2, e) -> (e, v1, v2)) . Map.toList $ am ! v1
      in foldr relaxE m eL
    relaxE (e, v1, v2) m =
      let n = ((m ! v1) <.> e) <+> (m ! v2)
      in Map.adjust (const n) v2 m




