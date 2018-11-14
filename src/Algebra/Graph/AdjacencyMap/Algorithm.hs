-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.Algorithm
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.Algorithm (
    -- * Algorithms
    scc
  ) where

import Data.Foldable (toList)
import Data.Maybe

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Internal

import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Graph                          as KL
import qualified Data.Graph.Typed                    as Typed
import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set

-- TODO: Update docs.
-- | Compute the /condensation/ of a graph, where each vertex corresponds to a
-- /strongly-connected component/ of the original graph.
--
-- @
-- scc 'empty'               == 'empty'
-- scc ('vertex' x)          == 'vertex' (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.vertex' x)
-- scc ('edge' 1 1)          == 'vertex' (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.edge' 1 1)
-- scc ('edge' 1 2)          == 'edge'   (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.vertex' 1) (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.vertex' 2)
-- scc ('circuit' (1:xs))    == 'vertex' (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.circuit1' (1 'Data.List.NonEmpty.:|' xs))
-- scc (3 * 1 * 4 * 1 * 5) == 'edges'  [ (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.vertex'  3      , NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.vertex'  5      )
--                                   , (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.vertex'  3      , NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.clique1' [1,4,1])
--                                   , (NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.clique1' [1,4,1], NonEmpty.'Algebra.Graph.NonEmpty.AdjacencyMap.vertex'  5      ) ]
-- @
scc :: Ord a => AdjacencyMap a -> AdjacencyMap (NonEmpty.AdjacencyMap a)
scc m = fromAdjacencySets
    [ (v', Set.delete v' $ Set.map (components Map.!) us)
    | (v, us) <- Map.toList (adjacencyMap m), let v' = components Map.! v ]
  where
    Typed.GraphKL g r _ = Typed.fromAdjacencyMap m
    components = Map.fromList $ concatMap (expand . fmap r . toList) (KL.scc g)
    expand xs  = map (\x -> (x, c)) xs
      where
        s = Set.fromList xs
        c = fromJust . NonEmpty.toNonEmpty $ induce (`Set.member` s) m
