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
-- | fold takes any function with the signature @e -> a -> a -> s -> s@.
-- This function folds over edges, modifying an original input state.
-- The edges are processed in an topological order of sink vertices,
-- ie. edge (e1, x, v1) is processed before edge (e2, y, v2) if and
-- only if v1 comes before v2 in any valid topological order of the
-- graph. Please note that (e, v1, v2) represents an edge 'e' from
-- 'v1' to 'v2'.
--
-- @
-- fold f s ('empty') == s 
-- fold f s ('vertex' x) == s 
-- fold f s ('vertices' xs) == s 
-- fold (\e v1 v2 -> flip (++) (e, v1, v2)) [] ('toAcyclicOrd' $ 'LAM.edge' 5 1 2) == [(5, 1, 2)] 
-- fold (\e v1 v2 -> flip (++) (e, v1, v2)) [] ('toAcyclicOrd' $ 'LAM.edges' [(5, 2, 3), (0, 1, 2), (6, 1, 3)]) == [(0, 1, 2), (5, 2, 3), (6, 1, 3)] 
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
-- | Compute the /shortest path/ to each vertex in the graph
-- from a given source vertex.
--
-- The following examples assume that the edges are distances,
-- ie. the edge 'Semiring' is 'Distance'.
-- @
-- optimumPath ('vertex' x) x == Map.'Map.fromList' [(x, 0)]
-- optimumPath ('vertex' 'a') 'z' == Map.'Map.fromList' [('a', 'distance' 'infinite')]
-- optimumPath ('toAcyclicOrd' $ 'LAM.edge' 2 'a' 'b') 'a' == Map.'Map.fromList' [('a', 0), ('b', 2)]
-- optimumPath ('toAcyclicOrd' $ 'LAM.edge' 2 'a' 'b') 'z' == Map.'Map.fromList' [('a', 'distance' 'infinite'), ('b', 'distance' 'infinite')]
-- optimumPath ('vertices' ['a', 'b']) 'a' == Map.'Map.fromList' [('a', 0), ('b', 'distance' 'infinite')]
-- optimumPath ('vertices' ['a', 'b']) 'z' == Map.'Map.fromList' [('a', 'distance' 'infinite'), ('b', 'distance' 'infinite')]
-- optimumPath ('toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'z' == Map.'Map.fromList' [('a', 'distance' 'infinite'), ('b', 'distance' 'infinite'), ('c', 'distance' 'infinite')]
-- optimumPath ('toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'a' == Map.'Map.fromList' [('a', 0), ('b', 1), ('c', 3)]
-- @
optimumPath :: (Dioid e, Ord a) => AdjacencyMap e a -> a -> Map a e
optimumPath am src = fromMaybe zm $ fold relax im am
  where
    zm = Map.map (const zero) . LAM.adjacencyMap . fromAcyclic $ am
    im = Map.insert src one zm <$ Map.lookup src zm
    relax _ _ _ Nothing = Nothing
    relax e v1 v2 (Just m) = Just $ Map.adjust (<+> ((m ! v1) <.> e)) v2 m
