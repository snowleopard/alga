module Algebra.Graph.Acyclic.Labelled.Algorithm where

import Algebra.Graph.Acyclic.Labelled.AdjacencyMap
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import Algebra.Graph.Label
import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- TODO: Replace this function with 'skeleton' from Acyclic.Labelled to remove the use of fromMaybe
-- TODO: Make 'topSort' more efficient
-- TODO: Add examples and tests
topSort :: (Ord a) => AdjacencyMap e a -> [a]
topSort = fromMaybe [] . AM.topSort . LAM.skeleton . fromAcyclic

-- TODO: Add time complexity
-- TODO: Add examples using 'Optimum' data type
-- | Compute the /shortest path/ to each vertex in the graph
-- from a given source vertex.
--
-- The following examples assume that the edges are distances,
-- ie. the edge 'Semiring' is 'Distance'.
-- @
-- dijkstra ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'z' == Map.'Map.fromList' [('z', 0), ('a', Infinite), ('b', Infinite), ('c', Infinite)]
-- dijkstra ('LAM.toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) 'a' == Map.'Map.fromList' [('a', 0), ('b', 1), ('c', 3)]
-- @
optimumPath :: (Semiring e, Ord a) => AdjacencyMap e a -> a -> Map a e
optimumPath am s = foldl (relaxVertex em) (initialize em) vL
  where
    vL = dropWhile (/=s) $ topSort am
    em = (LAM.adjacencyMap . fromAcyclic) am
    initialize = Map.insert s one . Map.map (const zero)
    relaxVertex em m v = Map.foldrWithKey (relaxEdge v) m (em ! v)
    relaxEdge v1 v2 e m = Map.insert v2 (((m ! v1) <.> e) <+> (m ! v2)) m
