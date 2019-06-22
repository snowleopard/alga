{-# language LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyIntMap.Algorithm
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides basic graph algorithms, such as /depth-first search/,
-- implemented for the "Algebra.Graph.AdjacencyIntMap" data type.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyIntMap.Algorithm (
    -- * Algorithms
    dfsForest, dfsForestFrom, dfs, reachable, topSort, isAcyclic,
    bfsForest, bfsForestFrom, bfs, 
    -- * Correctness properties
    isDfsForestOf, isTopSortOf
    ) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe
import Data.Tree

import Algebra.Graph.AdjacencyIntMap

import qualified Data.Graph.Typed   as Typed
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet

-- | Compute the /depth-first search/ forest of a graph that corresponds to
-- searching from each of the graph vertices in the 'Ord' @a@ order.
--
-- @
-- dfsForest 'empty'                       == []
-- 'forest' (dfsForest $ 'edge' 1 1)         == 'vertex' 1
-- 'forest' (dfsForest $ 'edge' 1 2)         == 'edge' 1 2
-- 'forest' (dfsForest $ 'edge' 2 1)         == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ dfsForest x) x == True
-- 'isDfsForestOf' (dfsForest x) x         == True
-- dfsForest . 'forest' . dfsForest        == dfsForest
-- dfsForest ('vertices' vs)               == 'map' (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
-- 'dfsForestFrom' ('vertexList' x) x        == dfsForest x
-- dfsForest $ 3 * (1 + 4) * (1 + 5)     == [ Node { rootLabel = 1
--                                                 , subForest = [ Node { rootLabel = 5
--                                                                      , subForest = [] }]}
--                                          , Node { rootLabel = 3
--                                                 , subForest = [ Node { rootLabel = 4
--                                                                      , subForest = [] }]}]
-- 'forest' (dfsForest ('circuit' [1..5] + 'transpose' ('circuit' [1..5]))) == 'path' [1..5]
-- @
dfsForest :: AdjacencyIntMap -> Forest Int
dfsForest g = dfsForestFrom (vertexList g) g

-- | Compute the /depth-first search/ forest of a graph, searching from each of
-- the given vertices in order. Note that the resulting forest does not
-- necessarily span the whole graph, as some vertices may be unreachable.
--
-- @
-- dfsForestFrom vs 'empty'                           == []
-- 'forest' (dfsForestFrom [1]   $ 'edge' 1 1)          == 'vertex' 1
-- 'forest' (dfsForestFrom [1]   $ 'edge' 1 2)          == 'edge' 1 2
-- 'forest' (dfsForestFrom [2]   $ 'edge' 1 2)          == 'vertex' 2
-- 'forest' (dfsForestFrom [3]   $ 'edge' 1 2)          == 'empty'
-- 'forest' (dfsForestFrom [2,1] $ 'edge' 1 2)          == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ dfsForestFrom vs x) x     == True
-- 'isDfsForestOf' (dfsForestFrom ('vertexList' x) x) x == True
-- dfsForestFrom ('vertexList' x) x                   == 'dfsForest' x
-- dfsForestFrom vs             ('vertices' vs)       == 'map' (\\v -> Node v []) ('Data.List.nub' vs)
-- dfsForestFrom []             x                   == []
-- dfsForestFrom [1,4] $ 3 * (1 + 4) * (1 + 5)      == [ Node { rootLabel = 1
--                                                            , subForest = [ Node { rootLabel = 5
--                                                                                 , subForest = [] }
--                                                     , Node { rootLabel = 4
--                                                            , subForest = [] }]
-- @
dfsForestFrom :: [Int] -> AdjacencyIntMap -> Forest Int
dfsForestFrom vs g = prune $ evalState (dff vs) IntSet.empty where
  dff (v:vs) | not (hasVertex v g) = dff vs
             | otherwise = (:) <$> unfoldTreeM walk v <*> dff vs
  dff _ = return []                              
  prune ts = [ Node t (prune xs) | Node (Just t) xs <- ts ]
  walk v = discovered v >>= \case
    False -> pure (Nothing,[])
    True -> (Just v,) <$> adjacentM v
  adjacentM v = filterM discovered' $ IntSet.toList (postIntSet v g)
  discovered' v = gets (not . IntSet.member v)
  discovered v = do unseen <- gets (not . IntSet.member v)
                    when unseen $ modify' (IntSet.insert v)
                    return unseen

-- | Compute the list of vertices visited by the /depth-first search/ in a graph,
-- when searching from each of the given vertices in order.
--
-- @
-- dfs vs    $ 'empty'                    == []
-- dfs [1]   $ 'edge' 1 1                 == [1]
-- dfs [1]   $ 'edge' 1 2                 == [1,2]
-- dfs [2]   $ 'edge' 1 2                 == [2]
-- dfs [3]   $ 'edge' 1 2                 == []
-- dfs [1,2] $ 'edge' 1 2                 == [1,2]
-- dfs [2,1] $ 'edge' 1 2                 == [2,1]
-- dfs []    $ x                        == []
-- dfs [1,4] $ 3 * (1 + 4) * (1 + 5)    == [1,5,4]
-- 'isSubgraphOf' ('vertices' $ dfs vs x) x == True
-- @
dfs :: [Int] -> AdjacencyIntMap -> [Int]
dfs vs = dfsForestFrom vs >=> flatten

-- | Compute the list of vertices that are /reachable/ from a given source
-- vertex in a graph. The vertices in the resulting list appear in the
-- /depth-first order/.
--
-- @
-- reachable x $ 'empty'                       == []
-- reachable 1 $ 'vertex' 1                    == [1]
-- reachable 1 $ 'vertex' 2                    == []
-- reachable 1 $ 'edge' 1 1                    == [1]
-- reachable 1 $ 'edge' 1 2                    == [1,2]
-- reachable 4 $ 'path'    [1..8]              == [4..8]
-- reachable 4 $ 'circuit' [1..8]              == [4..8] ++ [1..3]
-- reachable 8 $ 'clique'  [8,7..1]            == [8] ++ [1..7]
-- 'isSubgraphOf' ('vertices' $ reachable x y) y == True
-- @
reachable :: Int -> AdjacencyIntMap -> [Int]
reachable x = dfs [x]

-- | Compute the forest of a graph's vertices in breadth first order.
--
-- @
-- bfsForest 'empty'                         == []
-- 'forest' (bfsForest $ 'edge' 1 1)           == 'vertex' 1
-- 'forest' (bfsForest $ 'edge' 1 2)           == 'edge' 1 2
-- 'forest' (bfsForest $ 'edge' 2 1)           == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForest x) x   == True
-- bfsForest . 'forest' . bfsForest          == bfsForest
-- 'forest' (bfsForest ('circuit' [1..5] + 'transpose' ('circuit' [1..5]))) == 'path' [1,2,3] + 'path' [1,5,4]
-- @
bfsForest :: AdjacencyIntMap -> Forest Int
bfsForest g = bfsForestFrom (vertexList g) g

-- | Like 'bfsForest', but the traversal is seeded by a list of vertices,
-- which may not include all of the given graph's vertices. Seed vertices not
-- in the graph are ignored.
--
-- @
-- 'forest' (bfsForestFrom [1,2] $ 'edge' 1 2) == 'vertices' [1,2]
-- 'forest' (bfsForestFrom [2]   $ 'edge' 2 1) == 'vertex' 2
-- 'forest' (bfsForestFrom [3]   $ 'edge' 1 2) == empty
-- 'forest' (bfsForestFrom [3] ('circuit' [1..5] + 'transpose' ('circuit' [1..5]))) == 'path' [3,2,1] + 'path' [3,4,5]
-- @
bfsForestFrom :: [Int] -> AdjacencyIntMap -> Forest Int
bfsForestFrom vs g = evalState (bff vs) IntSet.empty where
  bff (v:vs) = (hasVertex v g &&) <$> discovered v >>= \case
    False -> bff vs
    True -> (:) <$> unfoldTreeM_BF walk v <*> bff vs
  bff _ = return []
  walk v = (v,) <$> adjacentM v
  adjacentM v = filterM discovered $ IntSet.toList (postIntSet v g)
  discovered v = do seen <- gets (IntSet.member v)
                    unless seen $ modify' (IntSet.insert v)
                    return $ not seen

-- | Like 'bfsForestFrom' with the resulting forest flattened to a list of vertices.
--
-- @
-- bfs [3] ('circuit' [1..5] + 'transpose' ('circuit' [1..5])) == [3,2,1,4,5]
-- @
bfs :: [Int] -> AdjacencyIntMap -> [Int]
bfs vs =  bfsForestFrom vs >=> concat . levels

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)               == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)               == Nothing
-- fmap ('flip' 'isTopSortOf' x) (topSort x) /= Just False
-- 'isJust' . topSort                      == 'isAcyclic'
-- @
topSort :: AdjacencyIntMap -> Maybe [Int]
topSort m = if isTopSortOf result m then Just result else Nothing
  where
    result = Typed.topSort (Typed.fromAdjacencyIntMap m)

-- | Check if a given graph is /acyclic/.
--
-- @
-- isAcyclic (1 * 2 + 3 * 1) == True
-- isAcyclic (1 * 2 + 2 * 1) == False
-- isAcyclic . 'circuit'       == 'null'
-- isAcyclic                 == 'isJust' . 'topSort'
-- @
isAcyclic :: AdjacencyIntMap -> Bool
isAcyclic = isJust . topSort

-- | Check if a given forest is a correct /depth-first search/ forest of a graph.
-- The implementation is based on the paper "Depth-First Search and Strong
-- Connectivity in Coq" by François Pottier.
--
-- @
-- isDfsForestOf []                              'empty'            == True
-- isDfsForestOf []                              ('vertex' 1)       == False
-- isDfsForestOf [Node 1 []]                     ('vertex' 1)       == True
-- isDfsForestOf [Node 1 []]                     ('vertex' 2)       == False
-- isDfsForestOf [Node 1 [], Node 1 []]          ('vertex' 1)       == False
-- isDfsForestOf [Node 1 []]                     ('edge' 1 1)       == True
-- isDfsForestOf [Node 1 []]                     ('edge' 1 2)       == False
-- isDfsForestOf [Node 1 [], Node 2 []]          ('edge' 1 2)       == False
-- isDfsForestOf [Node 2 [], Node 1 []]          ('edge' 1 2)       == True
-- isDfsForestOf [Node 1 [Node 2 []]]            ('edge' 1 2)       == True
-- isDfsForestOf [Node 1 [], Node 2 []]          ('vertices' [1,2]) == True
-- isDfsForestOf [Node 2 [], Node 1 []]          ('vertices' [1,2]) == True
-- isDfsForestOf [Node 1 [Node 2 []]]            ('vertices' [1,2]) == False
-- isDfsForestOf [Node 1 [Node 2 [Node 3 []]]]   ('path' [1,2,3])   == True
-- isDfsForestOf [Node 1 [Node 3 [Node 2 []]]]   ('path' [1,2,3])   == False
-- isDfsForestOf [Node 3 [], Node 1 [Node 2 []]] ('path' [1,2,3])   == True
-- isDfsForestOf [Node 2 [Node 3 []], Node 1 []] ('path' [1,2,3])   == True
-- isDfsForestOf [Node 1 [], Node 2 [Node 3 []]] ('path' [1,2,3])   == False
-- @
isDfsForestOf :: Forest Int -> AdjacencyIntMap -> Bool
isDfsForestOf f am = case go IntSet.empty f of
    Just seen -> seen == vertexIntSet am
    Nothing   -> False
  where
    go seen []     = Just seen
    go seen (t:ts) = do
        let root = rootLabel t
        guard $ root `IntSet.notMember` seen
        guard $ and [ hasEdge root (rootLabel subTree) am | subTree <- subForest t ]
        newSeen <- go (IntSet.insert root seen) (subForest t)
        guard $ postIntSet root am `IntSet.isSubsetOf` newSeen
        go newSeen ts

-- | Check if a given list of vertices is a correct /topological sort/ of a graph.
--
-- @
-- isTopSortOf [3,1,2] (1 * 2 + 3 * 1) == True
-- isTopSortOf [1,2,3] (1 * 2 + 3 * 1) == False
-- isTopSortOf []      (1 * 2 + 3 * 1) == False
-- isTopSortOf []      'empty'           == True
-- isTopSortOf [x]     ('vertex' x)      == True
-- isTopSortOf [x]     ('edge' x x)      == False
-- @
isTopSortOf :: [Int] -> AdjacencyIntMap -> Bool
isTopSortOf xs m = go IntSet.empty xs
  where
    go seen []     = seen == IntMap.keysSet (adjacencyIntMap m)
    go seen (v:vs) = postIntSet v m `IntSet.intersection` newSeen == IntSet.empty
                  && go newSeen vs
      where
        newSeen = IntSet.insert v seen
