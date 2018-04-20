-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.KingLaunchbury
-- Copyright  : (c) Anton Lorenzen 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides primitives for interoperability between this library and
-- the 'Data.Graph' module of the containers library.
-----------------------------------------------------------------------------
module Algebra.Graph.KingLaunchbury (
  GraphKL(..),
  fromAdjacencyMap, toAdjacenyMap, toAdjacenyMap2,
  dfsForest, dfsForestFrom, dfs, topSort, isTopSort, scc
  ) where

import qualified Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.AdjacencyMap.Internal

import Data.Tree
import Data.Set (Set)
import Data.Foldable (toList)
import Data.Maybe

import qualified Data.Graph as KL
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Array          as Array
import qualified Data.List           as List

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
fromAdjacencyMap (AM m) = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, Set.toAscList us) | (v, us) <- Map.toAscList m ]

-- | Build an 'AdjacencyMap' from a 'GraphKL'.
toAdjacenyMap :: Ord a => GraphKL a -> AdjacencyMap a
toAdjacenyMap (GraphKL g from _) = AM $ Map.fromList
  $ map (\v -> (from v, Set.fromList $ map from $ g Array.! v)) $ KL.vertices g

-- | Build an 'AdjacencyMap' from a 'GraphKL'.
-- Alternative, possible faster implementation -> TODO: Benchmark!
toAdjacenyMap2 :: Ord a => GraphKL a -> AdjacencyMap a
toAdjacenyMap2 (GraphKL g from _) = AM $ Map.mapKeys from $ Map.fromDistinctAscList
  $ map (\v -> (v, Set.fromList $ map from $ g Array.! v)) $ KL.vertices g

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

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)             == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)             == Nothing
-- fmap (flip 'isTopSort' x) (topSort x) /= Just False
-- @
topSort :: GraphKL a -> Maybe [a]
topSort m@(GraphKL g r _) =
    if isTopSort result m then Just result else Nothing
  where
    result = map r (KL.topSort g)

-- | Check if a given list of vertices is a valid /topological sort/ of a graph.
--
-- @
-- isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True
-- isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False
-- isTopSort []        (1 * 2 + 3 * 1) == False
-- isTopSort []        'empty'           == True
-- isTopSort [x]       ('vertex' x)      == True
-- isTopSort [x]       ('edge' x x)      == False
-- @
isTopSort :: [a] -> GraphKL a -> Bool
isTopSort xs (GraphKL g _ t) = maybe False (go []) (mapM t xs)
  where
    go seen []     = seen == KL.vertices g
    go seen (v:vs) = let newSeen = seen `seq` v:seen
        in KL.reachable g v `List.intersect` newSeen == [] && go newSeen vs

-- | Compute the /condensation/ of a graph, where each vertex corresponds to a
-- /strongly-connected component/ of the original graph.
--
-- @
-- scc 'empty'               == 'empty'
-- scc ('vertex' x)          == 'vertex' (Set.'Set.singleton' x)
-- scc ('edge' x y)          == 'edge' (Set.'Set.singleton' x) (Set.'Set.singleton' y)
-- scc ('circuit' (1:xs))    == 'edge' (Set.'Set.fromList' (1:xs)) (Set.'Set.fromList' (1:xs))
-- scc (3 * 1 * 4 * 1 * 5) == 'edges' [ (Set.'Set.fromList' [1,4], Set.'Set.fromList' [1,4])
--                                  , (Set.'Set.fromList' [1,4], Set.'Set.fromList' [5]  )
--                                  , (Set.'Set.fromList' [3]  , Set.'Set.fromList' [1,4])
--                                  , (Set.'Set.fromList' [3]  , Set.'Set.fromList' [5]  )]
-- @
scc :: Ord a => AdjacencyMap a -> GraphKL a -> AdjacencyMap (Set a)
scc m (GraphKL g r _) =
    AM.gmap (\v -> Map.findWithDefault Set.empty v components) m
  where
    components = Map.fromList $ concatMap (expand . fmap r . toList) (KL.scc g)
    expand xs  = let s = Set.fromList xs in map (\x -> (x, s)) xs
