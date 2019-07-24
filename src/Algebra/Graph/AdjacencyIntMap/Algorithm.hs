{-# language LambdaCase, ViewPatterns, PatternSynonyms #-}

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
--
-- Some of the worst-case complexities include the term /min(n,W)/.
-- Following 'IntSet.IntSet' and 'IntMap.IntMap', the /W/ stands for
-- word size (usually 32 or 64 bits).
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyIntMap.Algorithm (
    -- * Algorithms
    bfsForest, bfsForestFrom, bfs, dfsForest, dfsForestFrom, dfs, reachable,
    topSort, isAcyclic,
    
    -- * Correctness properties
    isDfsForestOf, isTopSortOf
    ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Cont
import Data.List.NonEmpty (NonEmpty(..),(<|))
import Data.Tree

import Algebra.Graph.AdjacencyIntMap

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet

-- | Compute the /breadth-first search/ forest of a graph, such that
--   adjacent vertices are explored in increasing order with respect
--   to their 'Ord' instance.
-- 
--   Complexity: /O((n+m)*min(n,W))/ time and /O(n)/ space.
--
-- @
-- bfsForest 'empty'                         == []
-- 'forest' (bfsForest $ 'edge' 1 1)           == 'vertex' 1
-- 'forest' (bfsForest $ 'edge' 1 2)           == 'edge' 1 2
-- 'forest' (bfsForest $ 'edge' 2 1)           == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForest x) x   == True
-- bfsForest . 'forest' . bfsForest          == bfsForest
-- bfsForest (3 * (1 + 4) * (1 + 5))       == [ Node { rootLabel = 1
--                                                   , subForest = [ Node { rootLabel = 5
--                                                                        , subForest = [] }]}
--                                            , Node { rootLabel = 3
--                                                   , subForest = [ Node { rootLabel = 4
--                                                                        , subForest = [] }]}]
-- 'forest' (bfsForest ('circuit' [1..5] + 'circuit' [5,4..1])) == 'path' [1,2,3] + 'path' [1,5,4]
-- @
bfsForest :: AdjacencyIntMap -> Forest Int
bfsForest g = bfsForestFrom' (vertexList g) g

-- | Like 'bfsForest', but the traversal is seeded by a list of
--   vertices. Vertices not in the graph are ignored.
--
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*min(n,W))/ time and /O(n)/ space.
--
-- @
-- 'forest' (bfsForestFrom [1,2] $ 'edge' 1 2)      == 'vertices' [1,2]
-- 'forest' (bfsForestFrom [2]   $ 'edge' 1 2)      == 'vertex' 2
-- 'forest' (bfsForestFrom [3]   $ 'edge' 1 2)      == 'empty'
-- 'forest' (bfsForestFrom [2,1] $ 'edge' 1 2)      == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForestFrom vs x) x == True
-- bfsForestFrom ('vertexList' x) x               == 'bfsForest' x
-- bfsForestFrom vs ('vertices' vs)               == 'map' (\v -> Node v []) ('nub' vs)
-- bfsForestFrom [] x                           == []
-- bfsForestFrom [1,4] (3 * (1 + 4) * (1 + 5))  == [ Node { rootLabel = 1
--                                                        , subForest = [ Node { rootLabel = 5
--                                                                             , subForest = [] }]}
--                                                 , Node { rootLabel = 4
--                                                        , subForest = [] }]
-- 'forest' (bfsForestFrom [3] ('circuit' [1..5] + 'circuit' [5,4..1])) == 'path' [3,2,1] + 'path' [3,4,5]
-- 
-- @
bfsForestFrom :: [Int] -> AdjacencyIntMap -> Forest Int
bfsForestFrom vs g = bfsForestFrom' [ v | v <- vs, hasVertex v g ] g

bfsForestFrom' :: [Int] -> AdjacencyIntMap -> Forest Int
bfsForestFrom' vs g = evalState (explore vs) IntSet.empty where
  explore (v:vs) = discovered v >>= \case
    True -> (:) <$> unfoldTreeM_BF walk v <*> explore vs
    False -> explore vs
  explore [] = return []
  walk v = (v,) <$> adjacentM v
  adjacentM v = filterM discovered $ IntSet.toList (postIntSet v g)
  discovered v = do new <- gets (not . IntSet.member v)
                    when new $ modify' (IntSet.insert v)
                    return new

-- | Like 'bfsForestFrom' with the resulting forest converted to a
--   level structure.  Flattening the result via @'concat' . 'bfs' vs@
--   gives an enumeration of vertices reachable from @vs@ in breadth
--   first order. Adjacent vertices are explored in increasing order
--   with respect to their 'Ord' instance.
--
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*min(n,W))/ time and /O(n)/ space.
-- 
-- @
-- bfs vs 'empty'                                         == []
-- bfs [] g                                             == []
-- bfs [1] ('edge' 1 1)                                   == [[1]]
-- bfs [1] ('edge' 1 2)                                   == [[1],[2]]
-- bfs [2] ('edge' 1 2)                                   == [[2]]
-- bfs [3] ('edge' 1 2)                                   == []
-- bfs [1,2] ('edge' 1 2)                                 == [[1],[2]]
-- bfs [2,1] ('edge' 1 2)                                 == [[2],[1]]
-- bfs [1,2] ( (1*2) + (3*4) + (5*6) )                  == [[1],[2]]
-- bfs [1,3] ( (1*2) + (3*4) + (5*6) )                  == [[1],[2],[3],[4]]
-- bfs [3] (3 * (1 + 4) * (1 + 5))                      == [[3],[1,4,5]]
-- bfs [2] ('circuit' [1..5] + 'circuit' [5,4..1])          == [[2],[1,3],[5,4]]
-- 'concat' (bfs [3] $ 'circuit' [1..5] + 'circuit' [5,4..1]) == [3,2,4,1,5]
-- @
bfs :: [Int] -> AdjacencyIntMap -> [[Int]]
bfs vs = bfsForestFrom vs >=> levels

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
-- 'forest' (dfsForest $ 'circuit' [1..5] + 'circuit' [5,4..1]) == 'path' [1,2,3,4,5]
-- @
dfsForest :: AdjacencyIntMap -> Forest Int
dfsForest g = dfsForestFrom' (vertexList g) g

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
-- 'forest' (dfsForestFrom [3] $ 'circuit' [1..5] + 'circuit' [5,4..1]) == 'path' [3,2,1,5,4]
-- @
dfsForestFrom :: [Int] -> AdjacencyIntMap -> Forest Int
dfsForestFrom vs g = dfsForestFrom' [ v | v <- vs, hasVertex v g ] g

dfsForestFrom' :: [Int] -> AdjacencyIntMap -> Forest Int
dfsForestFrom' vs g = evalState (explore vs) mempty where
  explore (v:vs) = discovered v >>= \case
    True -> (:) <$> walk v <*> explore vs
    False -> explore vs 
  explore [] = return []
  walk v = Node v <$> (explore =<< adjacentM v)
  adjacentM v = filterM undiscovered $ IntSet.toList (postIntSet v g)
  undiscovered v = gets (not . IntSet.member v)
  discovered v = do new <- undiscovered v
                    when new $ modify' (IntSet.insert v)
                    return new

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
-- dfs [3] $ 'circuit' [1..5] + 'circuit' [5,4..1] == [3,2,1,5,4]
-- @
dfs :: [Int] -> AdjacencyIntMap -> [Int]
dfs vs = dfsForestFrom vs >=> flatten

-- | Compute the list of vertices that are /reachable/ from a given source
-- vertex in a graph. The vertices in the resulting list appear in
-- /breadth-first order/. 
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
reachable x = concat . bfs [x]

type Cycle = NonEmpty Int
type TopSort = Either Cycle [Int]
type ParentTable = IntMap.IntMap (Maybe Int,Bool)
data S = S { table    :: !ParentTable
           , order     :: [Int] }

retrace :: Int -> Cycle -> ParentTable -> Cycle
retrace v vs@(u :| _) table
  | v == u = vs
  | Parent p <- IntMap.lookup u table = retrace v (p <| vs) table
  | otherwise = vs -- impossible

pattern TreeEdge :: Maybe (Maybe Int, Bool)
pattern TreeEdge <- Nothing
pattern BackEdge :: Maybe (Maybe Int, Bool)
pattern BackEdge <- Just (_,False)
pattern Parent :: Int -> Maybe (Maybe Int, Bool)
pattern Parent p <- Just (Just p,_)

topSort' :: (MonadState S m, MonadCont m) => AdjacencyIntMap -> m TopSort
topSort' g = callCC $ \cyclic -> do
  let unexplored u = gets (not . IntMap.member u . table)
      parent u v = modify' (\(S p vs) -> S (IntMap.insert v (u,False) p) vs)
      exit v = modify' (\(S p vs) -> S (IntMap.alter done v p) (v:vs)) where
        done = fmap (fmap (const True))
      edge_type v = gets (IntMap.lookup v . table)
      dfs u =
        do forM_ (IntSet.toDescList $ postIntSet u g) $ \v ->
             edge_type v >>= \case
               TreeEdge -> parent (Just u) v >> dfs v
               BackEdge -> cyclic . Left . retrace v (u :| []) =<< gets table
               _        -> return ()
           exit u
  forM_ (map fst $ IntMap.toDescList $ adjacencyIntMap g) $
    \v -> do new_tree <- unexplored v
             when new_tree $ parent Nothing v >> dfs v
  Right <$> gets order

-- | Compute a topological sort of the vertices of a graph. Given a
--  DAG, the lexicographically least topological ordering is returned,
--  otherwise, a cycle is.
--
-- @
-- topSort (1 * 2 + 3 * 1)               == Right [3,1,2]
-- topSort ('path' [1..5])                 == Right [1..5]
-- topSort (3 * (1 * 4 + 2 * 5))         == Right [3,1,2,4,5]
-- topSort (1 * 2 + 2 * 1)               == Left [1,2]
-- topSort ('path' [5,4..1] + 'edge' 2 4)    == Left [4,3,2]
-- topSort ('circuit' [1..5])              == Left [1..5]
-- fmap ('flip' 'isTopSortOf' x) (topSort x) /= Right False
-- 'isRight' . topSort                     == 'isAcyclic'
-- @
topSort :: AdjacencyIntMap -> Either Cycle [Int]
topSort g = runContT (evalStateT (topSort' g) initialState) id where
  initialState = S mempty mempty 

-- | Check if a given graph is /acyclic/.
--
-- @
-- isAcyclic (1 * 2 + 3 * 1) == True
-- isAcyclic (1 * 2 + 2 * 1) == False
-- isAcyclic . 'circuit'       == 'null'
-- isAcyclic                 == 'isRight' . 'topSort'
-- @
isAcyclic :: AdjacencyIntMap -> Bool
isAcyclic g = case topSort g of
  Right _ -> True
  Left  _ -> False

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
