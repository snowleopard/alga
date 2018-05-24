-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.KingLaunchbury
-- Copyright  : (c) Anton Lorenzen 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides primitives for interoperability between this library and
-- the 'Data.Graph' module of the containers library. It is for internal use only
-- and MAY BE REMOVED WITHOUT NOTICE at any point!
-----------------------------------------------------------------------------
module Data.Graph.Typed (
  GraphKL(..),
  fromAdjacencyMap, toAdjacenyMap, toAdjacenyMap2,
  fromIntAdjacencyMap, toIntAdjacencyMap,
  dfsForest, dfsForestFrom, dfs, topSort
  ) where

import Algebra.Graph.AdjacencyMap.Internal as AM (AdjacencyMap(..))
import Algebra.Graph.IntAdjacencyMap.Internal as IAM (IntAdjacencyMap(..))

import Data.Tree
import Data.Maybe

import qualified Data.Graph         as KL
import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set           as Set
import qualified Data.IntSet        as IntSet
import qualified Data.Array         as Array

-- | 'GraphKL' encapsulates King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library.
data GraphKL a = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    toGraphKL :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @a@.
    fromVertexKL :: KL.Vertex -> a,
    -- | A mapping from vertices of type @a@ to "Data.Graph.Vertex".
    -- Returns 'Nothing' if the argument is not in the graph.
    toVertexKL :: a -> Maybe KL.Vertex }

-- | Build 'GraphKL' from an 'AdjacencyMap'.
-- If @mkGraphKL (adjacencyMap g) == h@ then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'Algebra.Graph.AdjacencyMap.vertexList' g
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'Algebra.Graph.AdjacencyMap.edgeList' g
-- @
fromAdjacencyMap :: Ord a => AdjacencyMap a -> GraphKL a
fromAdjacencyMap (AM.AM m) = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, Set.toAscList us) | (v, us) <- Map.toAscList m ]

fromIntAdjacencyMap :: IntAdjacencyMap -> GraphKL Int
fromIntAdjacencyMap (IAM.AM m) = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, IntSet.toAscList us) | (v, us) <- IntMap.toAscList m ]

-- | Build an 'AdjacencyMap' from a 'GraphKL'.
toAdjacenyMap :: Ord a => GraphKL a -> AdjacencyMap a
toAdjacenyMap (GraphKL g from _) = AM.AM $ Map.fromList
  $ map (\v -> (from v, Set.fromList $ map from $ g Array.! v)) $ KL.vertices g

-- | Build an 'AdjacencyMap' from a 'GraphKL'.
-- Alternative, possible faster implementation -> TODO: Benchmark!
toAdjacenyMap2 :: Ord a => GraphKL a -> AdjacencyMap a
toAdjacenyMap2 (GraphKL g from _) = AM.AM $ Map.mapKeys from $ Map.fromDistinctAscList
  $ map (\v -> (v, Set.fromList $ map from $ g Array.! v)) $ KL.vertices g

toIntAdjacencyMap :: GraphKL Int -> IntAdjacencyMap
toIntAdjacencyMap (GraphKL g from _) = IAM.AM $ IntMap.mapKeys from
  $ IntMap.fromDistinctAscList $ map (\v -> (v, IntSet.fromList $ map from $ g Array.! v))
  $ KL.vertices g

-- | Compute the /depth-first search/ forest of a graph.
--
-- @
-- 'forest' (dfsForest $ 'edge' 1 1)         == 'vertex' 1
-- 'forest' (dfsForest $ 'edge' 1 2)         == 'edge' 1 2
-- 'forest' (dfsForest $ 'edge' 2 1)         == 'vertices' [1, 2]
-- 'isSubgraphOf' ('forest' $ dfsForest x) x == True
-- dfsForest . 'forest' . dfsForest        == dfsForest
-- dfsForest ('vertices' vs)               == map (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
-- 'dfsForestFrom' ('vertexList' x) x        == dfsForest x
-- dfsForest $ 3 * (1 + 4) * (1 + 5)     == [ Node { rootLabel = 1
--                                                 , subForest = [ Node { rootLabel = 5
--                                                                      , subForest = [] }]}
--                                          , Node { rootLabel = 3
--                                                 , subForest = [ Node { rootLabel = 4
--                                                                      , subForest = [] }]}]
-- @
dfsForest :: GraphKL a -> Forest a
dfsForest (GraphKL g r _) = fmap (fmap r) (KL.dff g)

-- | Compute the /depth-first search/ forest of a graph, searching from each of
-- the given vertices in order. Note that the resulting forest does not
-- necessarily span the whole graph, as some vertices may be unreachable.
--
-- @
-- 'forest' (dfsForestFrom [1]    $ 'edge' 1 1)     == 'vertex' 1
-- 'forest' (dfsForestFrom [1]    $ 'edge' 1 2)     == 'edge' 1 2
-- 'forest' (dfsForestFrom [2]    $ 'edge' 1 2)     == 'vertex' 2
-- 'forest' (dfsForestFrom [3]    $ 'edge' 1 2)     == 'empty'
-- 'forest' (dfsForestFrom [2, 1] $ 'edge' 1 2)     == 'vertices' [1, 2]
-- 'isSubgraphOf' ('forest' $ dfsForestFrom vs x) x == True
-- dfsForestFrom ('vertexList' x) x               == 'dfsForest' x
-- dfsForestFrom vs             ('vertices' vs)   == map (\\v -> Node v []) ('Data.List.nub' vs)
-- dfsForestFrom []             x               == []
-- dfsForestFrom [1, 4] $ 3 * (1 + 4) * (1 + 5) == [ Node { rootLabel = 1
--                                                        , subForest = [ Node { rootLabel = 5
--                                                                             , subForest = [] }
--                                                 , Node { rootLabel = 4
--                                                        , subForest = [] }]
-- @
dfsForestFrom :: [a] -> GraphKL a -> Forest a
dfsForestFrom vs (GraphKL g r t) = fmap (fmap r) (KL.dfs g (mapMaybe t vs))

-- | Compute the list of vertices visited by the /depth-first search/ in a graph,
-- when searching from each of the given vertices in order.
--
-- @
-- dfs [1]    $ 'edge' 1 1                == [1]
-- dfs [1]    $ 'edge' 1 2                == [1, 2]
-- dfs [2]    $ 'edge' 1 2                == [2]
-- dfs [3]    $ 'edge' 1 2                == []
-- dfs [1, 2] $ 'edge' 1 2                == [1, 2]
-- dfs [2, 1] $ 'edge' 1 2                == [2, 1]
-- dfs []     $ x                       == []
-- dfs [1, 4] $ 3 * (1 + 4) * (1 + 5)   == [1, 5, 4]
-- 'isSubgraphOf' ('vertices' $ dfs vs x) x == True
-- @
dfs :: [a] -> GraphKL a -> [a]
dfs vs = concatMap flatten . dfsForestFrom vs

-- | Compute the /topological sort/ of a graph.
-- Unlike the (Int)AdjacencyMap algorithm this returns
-- a result even if the graph is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)             == [3,1,2]
-- topSort (1 * 2 + 2 * 1)             == [1,2]
-- @
topSort :: GraphKL a -> [a]
topSort (GraphKL g r _) = map r (KL.topSort g)
