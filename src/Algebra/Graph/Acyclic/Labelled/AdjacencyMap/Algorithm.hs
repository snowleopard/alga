module Algebra.Graph.Acyclic.Labelled.AdjacencyMap.Algorithm where

import Algebra.Graph.Acyclic.Labelled.AdjacencyMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import Algebra.Graph.Label
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List (foldl')

-- TODO: Replace this function with 'skeleton' from Acyclic.Labelled to remove the use of fromMaybe
-- TODO: Make 'topSort' more efficient
-- TODO: Add examples and tests for 'topSort'
topSort :: (Ord a) => AdjacencyMap e a -> [a]
topSort = fromMaybe [] . AM.topSort . LAM.skeleton . fromAcyclic

-- TODO: Improve documentation for 'fold'
-- TODO: Add examples and tests for 'fold'
-- TODO: Make 'fold' more efficient
-- | Compute the final state by using the initial state and
-- traversing the entire graph in topological order.
--
-- @
-- fold (\e v1 v2 -> ++ (e, v1, v2)) [] (LAM.toAcyclicOrd $ LAM.edges [(5, 2, 3), (0, 1, 2), (6, 1, 3)]) == [(0, 1, 2), (5, 2, 3), (6, 1, 3)] 
-- @
fold :: (Ord a) => (e -> a -> a -> s -> s) -> s ->  AdjacencyMap e a -> s
fold f s am = foldl' f' s . unfold nm . topSort $ am
  where
    em = LAM.adjacencyMap . fromAcyclic $ am
    nm = Map.map (const []) em
    addP v1 m =
      let adjust v2 e = Map.adjust ((e, v1, v2):) v2
      in Map.foldrWithKey adjust m (em ! v1)
    unfold _ [] = []
    unfold m (v2:vs) = (m ! v2) ++ unfold (addP v2 m) vs
    f' s (e, v1, v2) = f e v1 v2 s 

-- TODO: Add time complexity
-- TODO: Add examples using 'Optimum' data type
-- | Compute the /shortest path/ to each vertex in the graph
-- from a given source vertex.
--
-- The following examples assume that the edges are distances,
-- ie. the edge 'Semiring' is 'Distance'.
-- @
-- optimumPath ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'z' == Map.'Map.fromList' [('a', distance infinite), ('b', distance infinite), ('c', distance infinite)]
-- optimumPath ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'a' == Map.'Map.fromList' [('a', 0), ('b', 1), ('c', 3)]
-- @
optimumPath :: (Dioid e, Ord a) => AdjacencyMap e a -> a -> Map a e
optimumPath am src = fromMaybe zm $ fold relax Nothing am
  where
    zm = Map.map (const zero) . LAM.adjacencyMap . fromAcyclic $ am
    relax e v1 v2 Nothing
      | v1 == src = relax e v1 v2 . Just . Map.insert src one $ zm 
      | otherwise = Nothing
    relax e v1 v2 (Just m) = Just $ Map.adjust (<+> ((m ! v1) <.> e)) v2 m
