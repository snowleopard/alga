-----------------------------------------------------------------------------
-- |
-- Module     : Data.Graph.Typed
-- Copyright  : (c) Anton Lorenzen, Andrey Mokhov 2016-2025
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
-- and may be removed without notice at any point.
-----------------------------------------------------------------------------
module Data.Graph.Typed (
    -- * Data type and construction
    GraphKL(..), fromAdjacencyMap, fromAdjacencyIntMap,

    -- * Basic algorithms
    dfsForest, dfsForestFrom, dfs, topSort, scc
    ) where

import Data.Tree
import Data.Maybe
import Data.Foldable

import qualified Data.Graph as KL

import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Algebra.Graph.AdjacencyIntMap       as AIM

import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set

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

-- | Build 'GraphKL' from an 'AM.AdjacencyMap'. If @fromAdjacencyMap g == h@
-- then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'AM.vertexList' g
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'AM.edgeList' g
-- 'toGraphKL' (fromAdjacencyMap (1 * 2 + 3 * 1))                                == 'array' (0,2) [(0,[1]), (1,[]), (2,[0])]
-- 'toGraphKL' (fromAdjacencyMap (1 * 2 + 2 * 1))                                == 'array' (0,1) [(0,[1]), (1,[0])]
-- @
fromAdjacencyMap :: Ord a => AM.AdjacencyMap a -> GraphKL a
fromAdjacencyMap am = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), x, ys) | (x, ys) <- AM.adjacencyList am ]

-- | Build 'GraphKL' from an 'AIM.AdjacencyIntMap'. If
-- @fromAdjacencyIntMap g == h@ then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'Data.IntSet.toAscList' ('Algebra.Graph.AdjacencyIntMap.vertexIntSet' g)
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'Algebra.Graph.AdjacencyIntMap.edgeList' g
-- 'toGraphKL' (fromAdjacencyIntMap (1 * 2 + 3 * 1))                             == 'array' (0,2) [(0,[1]), (1,[]), (2,[0])]
-- 'toGraphKL' (fromAdjacencyIntMap (1 * 2 + 2 * 1))                             == 'array' (0,1) [(0,[1]), (1,[0])]
-- @
fromAdjacencyIntMap :: AIM.AdjacencyIntMap -> GraphKL Int
fromAdjacencyIntMap aim = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \x -> case r x of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), x, ys) | (x, ys) <- AIM.adjacencyList aim ]

-- | Compute the /depth-first search/ forest of a graph.
--
-- In the following examples we will use the helper function:
--
-- @
-- (%) :: Ord a => ('GraphKL' a -> b) -> 'AM.AdjacencyMap' a -> b
-- f % x = f ('fromAdjacencyMap' x)
-- @
--
-- for greater clarity.
--
-- @
-- 'AM.forest' (dfsForest % 'AM.edge' 1 1)           == 'AM.vertex' 1
-- 'AM.forest' (dfsForest % 'AM.edge' 1 2)           == 'AM.edge' 1 2
-- 'AM.forest' (dfsForest % 'AM.edge' 2 1)           == 'AM.vertices' [1,2]
-- 'AM.isSubgraphOf' ('AM.forest' $ dfsForest % x) x == True
-- dfsForest % 'AM.forest' (dfsForest % x)      == dfsForest % x
-- dfsForest % 'AM.vertices' vs                 == 'map' (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
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
-- In the following examples we will use the helper function:
--
-- @
-- (%) :: Ord a => ('GraphKL' a -> b) -> 'AM.AdjacencyMap' a -> b
-- f % x = f ('fromAdjacencyMap' x)
-- @
--
-- for greater clarity.
--
-- @
-- 'AM.forest' $ (dfsForestFrom % 'AM.edge' 1 1) [1]          == 'AM.vertex' 1
-- 'AM.forest' $ (dfsForestFrom % 'AM.edge' 1 2) [0]          == 'AM.empty'
-- 'AM.forest' $ (dfsForestFrom % 'AM.edge' 1 2) [1]          == 'AM.edge' 1 2
-- 'AM.forest' $ (dfsForestFrom % 'AM.edge' 1 2) [2]          == 'AM.vertex' 2
-- 'AM.forest' $ (dfsForestFrom % 'AM.edge' 1 2) [2,1]        == 'AM.vertices' [1,2]
-- 'AM.isSubgraphOf' ('AM.forest' $ dfsForestFrom % x $ vs) x == True
-- dfsForestFrom % x $ 'AM.vertexList' x                 == 'dfsForest' % x
-- dfsForestFrom % 'AM.vertices' vs $ vs                 == 'map' (\\v -> Node v []) ('Data.List.nub' vs)
-- dfsForestFrom % x $ []                           == []
-- dfsForestFrom % (3 * (1 + 4) * (1 + 5)) $ [1,4]  == [ Node { rootLabel = 1
--                                                            , subForest = [ Node { rootLabel = 5
--                                                                                 , subForest = [] }
--                                                     , Node { rootLabel = 4
--                                                            , subForest = [] }]
-- @
dfsForestFrom :: GraphKL a -> [a] -> Forest a
dfsForestFrom (GraphKL g r t) = fmap (fmap r) . KL.dfs g . mapMaybe t

-- | Compute the list of vertices visited by the /depth-first search/ in a
-- graph, when searching from each of the given vertices in order.
--
-- In the following examples we will use the helper function:
--
-- @
-- (%) :: Ord a => ('GraphKL' a -> b) -> 'AM.AdjacencyMap' a -> b
-- f % x = f ('fromAdjacencyMap' x)
-- @
--
-- for greater clarity.
--
-- @
-- dfs % 'AM.edge' 1 1 $ [1]   == [1]
-- dfs % 'AM.edge' 1 2 $ [0]   == []
-- dfs % 'AM.edge' 1 2 $ [1]   == [1,2]
-- dfs % 'AM.edge' 1 2 $ [2]   == [2]
-- dfs % 'AM.edge' 1 2 $ [1,2] == [1,2]
-- dfs % 'AM.edge' 1 2 $ [2,1] == [2,1]
-- dfs % x        $ []    == []
--
-- dfs % (3 * (1 + 4) * (1 + 5)) $ [1,4]     == [1,5,4]
-- 'Data.List.and' [ 'AM.hasVertex' v x | v <- dfs % x $ vs ] == True
-- @
dfs :: GraphKL a -> [a] -> [a]
dfs x = concatMap flatten . dfsForestFrom x

-- | Compute the /topological sort/ of a graph. Note that this function returns
-- a result even if the graph is cyclic.
--
-- In the following examples we will use the helper function:
--
-- @
-- (%) :: Ord a => ('GraphKL' a -> b) -> 'AM.AdjacencyMap' a -> b
-- f % x = f ('fromAdjacencyMap' x)
-- @
--
-- for greater clarity.
--
-- @
-- topSort % (1 * 2 + 3 * 1) == [3,1,2]
-- topSort % (1 * 2 + 2 * 1) == [1,2]
-- @
topSort :: GraphKL a -> [a]
topSort (GraphKL g r _) = map r (KL.topSort g)

-- TODO: Add docs and tests.
scc :: Ord a => AM.AdjacencyMap a -> AM.AdjacencyMap (NonEmpty.AdjacencyMap a)
scc m = AM.gmap (component Map.!) $ removeSelfLoops $ AM.gmap (leader Map.!) m
  where
    GraphKL g decode _ = fromAdjacencyMap m
    sccs      = map toList (KL.scc g)
    leader    = Map.fromList [ (decode y, x)      | x:xs <- sccs, y <- x:xs ]
    component = Map.fromList [ (x, expand (x:xs)) | x:xs <- sccs ]
    expand xs = fromJust $ NonEmpty.toNonEmpty $ AM.induce (`Set.member` s) m
      where
        s = Set.fromList (map decode xs)

removeSelfLoops :: Ord a => AM.AdjacencyMap a -> AM.AdjacencyMap a
removeSelfLoops m = foldr (\x -> AM.removeEdge x x) m (AM.vertexList m)
