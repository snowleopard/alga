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
-- TODO: Add examples and tests
topSort :: (Ord a) => AdjacencyMap e a -> [a]
topSort = fromMaybe [] . AM.topSort . LAM.skeleton . fromAcyclic

-- TODO: Improve documentation for 'foldAcyclic'
-- | Compute the final state by using the initial state and
-- traversing the entire graph in topological order.
foldAcyclic :: (Ord a) => (s -> a -> [a] -> s) -> s ->  AdjacencyMap e a -> s
foldAcyclic f s = snd . foldl' f' ([], s) . topSort
  where
    f' (p, s) x = (x:p, f s x p)

-- TODO: Add time complexity
-- TODO: Add examples using 'Optimum' data type
-- | Compute the /shortest path/ to each vertex in the graph
-- from a given source vertex.
--
-- The following examples assume that the edges are distances,
-- ie. the edge 'Semiring' is 'Distance'.
-- @
-- optimumPath ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'z' == Nothing
-- optimumPath ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'a' == Just $ Map.'Map.fromList' [('a', 0), ('b', 1), ('c', 3)]
-- @
optimumPath :: (Dioid e, Ord a) => AdjacencyMap e a -> a -> Maybe (Map a e)
optimumPath am src = foldAcyclic foldF Nothing am
  where
    foldF s x _ = relaxVertex (srcInit s x) x
    em = (LAM.adjacencyMap . fromAcyclic) am
    srcInit s x
      | x == src = Just . Map.insert src one . Map.map (const zero) $ em
      | otherwise = s
    relaxVertex s x = flip (Map.foldrWithKey (relaxEdge x)) (em ! x) <$> s
    relaxEdge v1 v2 e m = Map.insert v2 (((m ! v1) <.> e) <+> (m ! v2)) m
