module Algebra.Graph.Acyclic.Labelled.Algorithm where

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
-- | Compute the final state by using the initial state and
-- traversing the entire graph in topological order.
fold :: (Ord a) => (s -> a -> [(e, a)] -> s) -> s ->  AdjacencyMap e a -> s
fold f s am = snd . foldl' f' (Map.map (const []) em, s) . topSort $ am
  where
    em = LAM.adjacencyMap . fromAcyclic $ am
    f' (p, s) x = (addP x p, f s x (p ! x))
    addP x p = Map.foldrWithKey (\a e -> Map.adjust ((e, x):) a) p (em ! x)

-- TODO: Add time complexity
-- TODO: Add examples using 'Optimum' data type
-- | Compute the /shortest path/ to each vertex in the graph
-- from a given source vertex.
--
-- The following examples assume that the edges are distances,
-- ie. the edge 'Semiring' is 'Distance'.
-- @
-- optimumPath ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'z' == Nothing
-- optimumPath ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'a' == Just (Map.'Map.fromList' [('a', 0), ('b', 1), ('c', 3)])
-- @
optimumPath :: (Dioid e, Ord a) => AdjacencyMap e a -> a -> Maybe (Map a e)
optimumPath am src = fold relax Nothing am
  where
    em = LAM.adjacencyMap . fromAcyclic $ am
    relax mM v2 v1L
      | v2 == src =
          return . Map.insert src one . Map.map (const zero) $ em 
      | otherwise =
          flip (foldr (relaxOn v2)) v1L <$> mM
    relaxOn v2 (e, v1) m = Map.adjust (<+> ((m ! v1) <.> e)) v2 m
