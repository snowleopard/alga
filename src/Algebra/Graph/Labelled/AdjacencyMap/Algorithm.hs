module Algebra.Graph.Labelled.AdjacencyMap.Algorithm where

import Data.Map.Strict ((!))
import Algebra.Graph.Labelled.AdjacencyMap
import Data.List (foldl', sort)

import qualified Data.Map.Strict as Map

-- TODO: Make Prim's more efficient
-- TODO: Improve documentation for prim
-- TODO: Write examples and tests for prim
-- TODO: Remove Monoid instance on edges in Prim's
prim :: (Ord a, Ord e, Monoid e) => AdjacencyMap e a -> AdjacencyMap e a
prim wam = edges . snd . foldl' addEdge (im, []) $ eL
  where
    am = adjacencyMap wam
    im = Map.mapWithKey const am
    eL = sort . edgeList $ wam
    makeSet x y = Map.adjust (min x) y . Map.adjust (min y) x
    findSet m x = if m ! x == x
                     then x
                     else findSet m $ m ! x
    addEdge (m, g) e@(_, x, y) = if findSet m x == findSet m y
                                    then (m, g)
                                    else (makeSet x y m, e:g)

