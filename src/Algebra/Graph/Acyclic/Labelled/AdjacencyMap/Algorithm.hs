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
-- TODO: Make 'fold' more efficient
-- TODO: Add tests for fold
-- | fold takes any function with the signature @e -> a -> a -> s -> s@.
-- This function folds over an acyclic graph, modifying an original
-- input state. If one assumes the acyclic graph as a dependency
-- graph and the vertices as resources, then, the expected function
-- for fold takes all the dependents of the resource, the resource
-- and the input state in that order and pproduces an output state.
-- The resources (vertices) are processed in their topological order.
--
-- @
-- fold f s 'empty' == s 
-- fold (\e v -> (++[v])) [] == 'topSort'
-- fold (\e v -> (++[(e, v)]) [] ('vertex' x) == [([], x)] 
-- fold (\e v -> (++[(e, v)]) [] ('toAcyclicOrd' $ 'LAM.edge' 5 1 2) == [([], 1), ([(5, 1)], 2)] 
-- @
fold :: (Ord a) => ([(e, a)] -> a -> s -> s) -> s ->  AdjacencyMap e a -> s
fold f s wam = snd . foldl' process (nm, s) . topSort $ wam
  where
    am = LAM.adjacencyMap (fromAcyclic wam)
    nm = Map.map (const []) am
    addP v1 m =
      let adjust v2 e = Map.adjust ((e, v1):) v2
      in Map.foldrWithKey adjust m (am ! v1)
    process (m, s) v2 = (addP v2 m, f (m ! v2) v2 s)

-- TODO: Add time complexity
-- | Compute the /shortest path/ to each vertex in the graph
-- from a given source vertex.
--
-- The following examples assume that the edges are distances,
-- ie. the edge 'Semiring' is 'Distance'.
-- @
-- optimumPath x ('vertex' x) == Map.'Map.fromList' [(x, 0)]
-- optimumPath 'z' ('vertex' 'a') == Map.'Map.fromList' [('a', 'distance' 'infinite')]
-- optimumPath 'a' ('toAcyclicOrd' $ 'LAM.edge' 2 'a' 'b') == Map.'Map.fromList' [('a', 0), ('b', 2)]
-- optimumPath 'z' ('toAcyclicOrd' $ 'LAM.edge' 2 'a' 'b') == Map.'Map.fromList' [('a', 'distance' 'infinite'), ('b', 'distance' 'infinite')]
-- optimumPath 'a' ('vertices' ['a', 'b']) == Map.'Map.fromList' [('a', 0), ('b', 'distance' 'infinite')]
-- optimumPath 'z' ('vertices' ['a', 'b']) == Map.'Map.fromList' [('a', 'distance' 'infinite'), ('b', 'distance' 'infinite')]
-- optimumPath 'a' ('toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) == Map.'Map.fromList' [('a', 0), ('b', 1), ('c', 3)]
-- optimumPath 'z' ('toAcyclicOrd' $ 'LAM.edges' [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) == Map.'Map.fromList' [('a', 'distance' 'infinite'), ('b', 'distance' 'infinite'), ('c', 'distance' 'infinite')]
-- @
optimumPath :: (Dioid e, Ord a) => a -> AdjacencyMap e a -> Map a e
optimumPath src am = maybe zm relaxW im
  where
    zm = Map.map (const zero) . LAM.adjacencyMap . fromAcyclic $ am
    im = Map.insert src one zm <$ Map.lookup src zm
    relaxE v2 (e, v1) m = Map.adjust (<+> ((m ! v1) <.> e)) v2 m
    relaxV eL v2 m = foldr (relaxE v2) m eL 
    relaxW m = fold relaxV m am

