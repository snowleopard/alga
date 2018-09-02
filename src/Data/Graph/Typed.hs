-----------------------------------------------------------------------------
-- |
-- Module     : Data.Graph.Typed
-- Copyright  : (c) Anton Lorenzen, Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : anfelor@posteo.de, andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides primitives for interoperability between this library and
-- the "Data.Graph" module of the containers library. It is for internal use only
-- and __MAY BE REMOVED WITHOUT NOTICE__ at any point!
-----------------------------------------------------------------------------
module Data.Graph.Typed (
  GraphKL(..), fromAdjacencyMap, fromAdjacencyIntMap, fromLabelledAdjacencyMap,
  dfsForest, dfsForestFrom, dfs, topSort
  ) where

import Algebra.Graph.AdjacencyMap.Internal    as AM  (AdjacencyMap    (..))
import Algebra.Graph.AdjacencyIntMap.Internal as AIM (AdjacencyIntMap (..))

import Algebra.Graph.LabelledAdjacencyMap.Internal    as LAM  (LabelledAdjacencyMap    (..))

import Data.Tree
import Data.Maybe

import qualified Data.Graph         as KL
import qualified Data.Map.Strict    as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set           as Set
import qualified Data.IntSet        as IntSet

-- | 'GraphKL' encapsulates King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library.
data GraphKL a = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    toGraphKL :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @a@.
    -- This is partial and may fail if the vertex is out of bounds.
    fromVertexKL :: KL.Vertex -> a,
    -- | A mapping from vertices of type @a@ to "Data.Graph.Vertex".
    -- Returns 'Nothing' if the argument is not in the graph.
    toVertexKL :: a -> Maybe KL.Vertex }

-- | Build 'GraphKL' from an 'AdjacencyMap'.
-- If @fromAdjacencyMap g == h@ then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'Algebra.Graph.AdjacencyMap.vertexList' g
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'Algebra.Graph.AdjacencyMap.edgeList' g
--
-- 'toGraphKL' (fromAdjacencyMap (1 * 2 + 3 * 1)) == 'array' (0,2) [(0,[1]),(1,[]),(2,[0])]
-- 'toGraphKL' (fromAdjacencyMap (1 * 2 + 2 * 1)) == 'array' (0,1) [(0,[1]),(1,[0])]
-- @
fromAdjacencyMap :: Ord a => AdjacencyMap a -> GraphKL a
fromAdjacencyMap (AM.AM m) = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, Set.toAscList us) | (v, us) <- Map.toAscList m ]

-- | Build 'GraphKL' from an 'AdjacencyIntMap'.
-- If @fromAdjacencyIntMap g == h@ then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'Data.IntSet.toAscList' ('Algebra.Graph.AdjacencyIntMap.vertexIntSet' g)
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'Algebra.Graph.AdjacencyIntMap.edgeList' g
--
-- 'toGraphKL' (fromAdjacencyIntMap (1 * 2 + 3 * 1)) == 'array' (0,2) [(0,[1]),(1,[]),(2,[0])]
-- 'toGraphKL' (fromAdjacencyIntMap (1 * 2 + 2 * 1)) == 'array' (0,1) [(0,[1]),(1,[0])]
-- @
fromAdjacencyIntMap :: AdjacencyIntMap -> GraphKL Int
fromAdjacencyIntMap (AIM.AM m) = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, IntSet.toAscList us) | (v, us) <- IntMap.toAscList m ]

-- | Build 'GraphKL' from a 'LabelledAdjacencyMap'.
-- If @fromLabelledAdjacencyMap g == h@ then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'Algebra.Graph.AdjacencyMap.vertexList' g
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'Algebra.Graph.fromLabelledAdjacencyMap.edgeList' g
--
-- 'toGraphKL' (fromLabelledAdjacencyMap (1 * 2 + 3 * 1)) == 'array' (0,2) [(0,[1]),(1,[]),(2,[0])]
-- 'toGraphKL' (fromLabelledAdjacencyMap (1 * 2 + 2 * 1)) == 'array' (0,1) [(0,[1]),(1,[0])]
-- @
fromLabelledAdjacencyMap :: Ord a => LAM.LabelledAdjacencyMap a e -> GraphKL a
fromLabelledAdjacencyMap (LAM.LAM m) =GraphKL
        { toGraphKL    = g
        , fromVertexKL = \u -> case r u of
                (_, v, _) -> v
        , toVertexKL   = t
        }
    where
        (g, r, t) = KL.graphFromEdges
                [ ((), v, Set.toAscList (Map.keysSet us))
                | (v, us) <- Map.toAscList m
                ]

-- | Compute the /depth-first search/ forest of a graph.
--
-- In the following we will use the helper function:
--
-- @
-- (%) :: (GraphKL Int -> a) -> AM.AdjacencyMap Int -> a
-- a % g = a $ fromAdjacencyMap g
-- @
-- for greater clarity. (One could use an AdjacencyIntMap just as well)
--
-- @
-- 'forest' (dfsForest % 'edge' 1 1)           == 'vertex' 1
-- 'forest' (dfsForest % 'edge' 1 2)           == 'edge' 1 2
-- 'forest' (dfsForest % 'edge' 2 1)           == 'vertices' [1, 2]
-- 'isSubgraphOf' ('forest' $ dfsForest % x) x == True
-- dfsForest % 'forest' (dfsForest % x)      == dfsForest % x
-- dfsForest % 'vertices' vs                 == map (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
-- 'dfsForestFrom' ('vertexList' x) % x        == dfsForest % x
-- dfsForest % (3 * (1 + 4) * (1 + 5))     == [ Node { rootLabel = 1
--                                                   , subForest = [ Node { rootLabel = 5
--                                                                        , subForest = [] }]}
--                                            , Node { rootLabel = 3
--                                                   , subForest = [ Node { rootLabel = 4
--                                                                        , subForest = [] }]}]
-- @
dfsForest :: GraphKL a -> Forest a
dfsForest (GraphKL g r _) = fmap (fmap r) (KL.dff g)

-- | Compute the /depth-first search/ forest of a graph, searching from each of
-- the given vertices in order. Note that the resulting forest does not
-- necessarily span the whole graph, as some vertices may be unreachable.
--
-- @
-- 'forest' (dfsForestFrom [1]    % 'edge' 1 1)       == 'vertex' 1
-- 'forest' (dfsForestFrom [1]    % 'edge' 1 2)       == 'edge' 1 2
-- 'forest' (dfsForestFrom [2]    % 'edge' 1 2)       == 'vertex' 2
-- 'forest' (dfsForestFrom [3]    % 'edge' 1 2)       == 'empty'
-- 'forest' (dfsForestFrom [2, 1] % 'edge' 1 2)       == 'vertices' [1, 2]
-- 'isSubgraphOf' ('forest' $ dfsForestFrom vs % x) x == True
-- dfsForestFrom ('vertexList' x) % x               == 'dfsForest' % x
-- dfsForestFrom vs               % 'vertices' vs   == map (\\v -> Node v []) ('Data.List.nub' vs)
-- dfsForestFrom []               % x             == []
-- dfsForestFrom [1, 4] % (3 * (1 + 4) * (1 + 5)) == [ Node { rootLabel = 1
--                                                          , subForest = [ Node { rootLabel = 5
--                                                                               , subForest = [] }
--                                                   , Node { rootLabel = 4
--                                                          , subForest = [] }]
-- @
dfsForestFrom :: [a] -> GraphKL a -> Forest a
dfsForestFrom vs (GraphKL g r t) = fmap (fmap r) (KL.dfs g (mapMaybe t vs))

-- | Compute the list of vertices visited by the /depth-first search/ in a graph,
-- when searching from each of the given vertices in order.
--
-- @
-- dfs [1]    % 'edge' 1 1                == [1]
-- dfs [1]    % 'edge' 1 2                == [1, 2]
-- dfs [2]    % 'edge' 1 2                == [2]
-- dfs [3]    % 'edge' 1 2                == []
-- dfs [1, 2] % 'edge' 1 2                == [1, 2]
-- dfs [2, 1] % 'edge' 1 2                == [2, 1]
-- dfs []     % x                       == []
-- dfs [1, 4] % (3 * (1 + 4) * (1 + 5)) == [1, 5, 4]
-- 'isSubgraphOf' ('vertices' $ dfs vs x) x == True
-- @
dfs :: [a] -> GraphKL a -> [a]
dfs vs = concatMap flatten . dfsForestFrom vs

-- | Compute the /topological sort/ of a graph.
-- Unlike the (Int)AdjacencyMap algorithm this returns
-- a result even if the graph is cyclic.
--
-- @
-- topSort % (1 * 2 + 3 * 1) == [3,1,2]
-- topSort % (1 * 2 + 2 * 1) == [1,2]
-- @
topSort :: GraphKL a -> [a]
topSort (GraphKL g r _) = map r (KL.topSort g)
