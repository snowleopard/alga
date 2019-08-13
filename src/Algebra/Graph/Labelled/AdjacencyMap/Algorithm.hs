module Algebra.Graph.Labelled.AdjacencyMap.Algorithm where

import Algebra.Graph.Label
import Data.Map.Strict (Map, (!))
import Algebra.Graph.Labelled.AdjacencyMap

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- TODO: Improve documentation for 'dijkstra'.
-- | A generic Dijkstra algorithm that relaxes the list of edges
-- based on the 'Dioid'.
--
-- The heap (min vs max) is decided based on '<+>', 'zero' and 'one'.
--
-- We assume that the underlying semiring is selective i.e. the 
-- operation '<+>' always selects one of its arguments.
--
-- Choice of heap:
-- @
-- 'one' < 'zero':
--   'one' <+> 'zero' == 'one': Min heap
--   'one' <+> 'zero' == 'zero': Max heap
-- 'one' > 'zero':
--   'one' <+> 'zero' == 'one': Max heap
--   'one' <+> 'zero' == 'zero': Min heap
-- @
--
-- The examples below assume the edge values are 'Distance'.
--
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
    view =
      case (compare o z, o <+> z) of
        (LT, o) -> Set.minView
        (LT, z) -> Set.maxView
        (GT, o) -> Set.maxView
        (GT, z) -> Set.minView
        _       -> Set.minView
    processI = (Set.singleton (one, src), Map.insert src one zm)
    processG sm@(s, _) = processS (view s) sm
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
-- TODO: Change foldr to foldr'
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

-- TODO: Improve documentation for floydWarshall
-- TODO: Write tests and examples for floydWarshall
-- TODO: Change foldr to foldr'
floydWarshall :: (Ord a, Dioid e) => AdjacencyMap e a -> Map a (Map a e)
floydWarshall wam = relax0 im
  where
    am = adjacencyMap wam
    zm = Map.map (const $ Map.map (const zero) am) am
    em = Map.unionWith Map.union am zm
    im = Map.mapWithKey (Map.adjust (const one)) em
    vL = Map.keys am
    relax0 m = foldr relax1 m vL
    relax1 i m = foldr (relax2 i) m vL
    relax2 i j m = foldr (relax3 i j) m vL
    relax3 i j k m =
      let n = (m ! i ! j) <+> ((m ! i ! k) <.> (m ! k ! j))
      in Map.adjust (Map.adjust (const n) j) i m



