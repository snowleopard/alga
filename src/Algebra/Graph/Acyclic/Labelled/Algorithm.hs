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
topSort :: (Ord a) => AdjacencyMap e a -> [a]
topSort = fromMaybe [] . AM.topSort . LAM.skeleton . fromAcyclic

dijkstra :: (Semiring e, Ord a) => AdjacencyMap e a -> a -> Map a e
dijkstra am s = foldl (relaxVertex em) (initialize em) vl
  where
    vl = dropWhile (/=s) $ topSort am
    em = (LAM.adjacencyMap . fromAcyclic) am
    initialize = Map.insert s one . Map.map (const zero)
    relaxVertex em m v = Map.foldrWithKey (relaxEdge v) m (em ! v)
    relaxEdge v1 v2 e m = Map.insert v2 (((m ! v1) <.> e) <+> (m ! v2)) m

{-
-- REPL Testing
x = toAcyclicOrd $ LAM.edges
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
-}
