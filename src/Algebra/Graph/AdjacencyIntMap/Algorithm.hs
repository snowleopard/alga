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
    isDfsForestOf, isTopSortOf,

    -- * Type synonyms
    Cycle
    ) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Either
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

-- | Compute the /depth-first search/ forest of a graph, where
--   adjacent vertices are expanded in increasing order with respect
--   to their 'Ord' instance.
--
--   Complexity: /O((n+m)*min(n,W))/ time and /O(n)/ space.
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

-- | Compute the /depth-first search/ forest of a graph from the given
--   vertices, where adjacent vertices are expanded in increasing
--   order with respect to to their 'Ord' instance. Note that the
--   resulting forest does not necessarily span the whole graph, as
--   some vertices may be unreachable.
-- 
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*min(n,W))/ time and /O(n)/ space.
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
dfsForestFrom' vs g = evalState (explore vs) IntSet.empty where
  explore (v:vs) = discovered v >>= \case
    True -> (:) <$> walk v <*> explore vs
    False -> explore vs
  explore [] = return []
  walk v = Node v <$> explore (adjacent v)
  adjacent v = IntSet.toList (postIntSet v g)
  discovered v = do new <- gets (not . IntSet.member v)
                    when new $ modify' (IntSet.insert v)
                    return new

-- | Compute the vertices visited by /depth-first search/ in a graph
--   from the given vertices. Adjacent vertices are explored in
--   increasing order with respect to their 'Ord' instance.
-- 
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*min(n,W))/ time and /O(n)/ space.
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

-- | Compute the list of vertices that are /reachable/ from a given
--   source vertex in a graph. The vertices in the resulting list
--   appear in /depth-first order/.
--
--   Complexity: /O(m*min(n,W))/ time and /O(n)/ space.
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

type Cycle = NonEmpty
type TopSort = Either (Cycle Int) [Int]
data Entry = Entered Int | Exited Int | OpenRoot | ClosedRoot
type ParentTable = IntMap.IntMap Entry
data S = S { table :: !ParentTable, order :: [Int] }

topSort' :: (MonadState S m, MonadCont m) => AdjacencyIntMap -> m TopSort
topSort' g = callCC $ \cyclic ->
  do let vertices = map fst $ IntMap.toDescList $ adjacencyIntMap g
         adjacent = IntSet.toDescList . flip postIntSet g
         dfs prev curr =
           do enter prev curr
              forM_ (adjacent curr) $ \next ->
                nodeState next >>= \case
                  Nothing -> dfs (Entered curr) next
                  Just (Exited v) -> return ()
                  Just ClosedRoot -> return ()
                  _ -> cyclic . Left . retrace curr next =<< gets table
              exit curr
     forM_ vertices $ \v -> nodeState v >>= \case
       Nothing -> dfs OpenRoot v
       _ -> return ()
     Right <$> gets order
  where
    nodeState v = gets (IntMap.lookup v . table)
    enter u v = modify' (\(S p vs) -> S (IntMap.insert v u p) vs)
    exit v = modify' (\(S p vs) -> S (IntMap.alter (fmap leave) v p) (v:vs))
      where leave = \case
              Entered x -> Exited x
              OpenRoot -> ClosedRoot
              _ -> error $ "Internal error: dfs search order violated"
    retrace curr head table = aux (curr :| []) where
      aux xs@(curr :| _)
        | head == curr = xs
        | otherwise = case table IntMap.! curr of
            Entered prev -> aux (prev <| xs)
            _ -> error "Internal error: dfs back edge mistakingly identified"

-- | Compute a topological sort of a DAG or discover a cycle.
--
--   Vertices are expanded in increasing order with respect to their
--   'Ord' instance. This gives the lexicographically smallest
--   topological ordering in the case of success. In the case of
--   failure, the cycle is characterized by being the
--   lexicographically smallest up to rotation with respect to @Ord
--   (Dual Int)@ in the first connected component of the graph
--   containing a cycle, where the connected components are ordered by
--   their largest vertex with respect to @Ord a@.
--
--   Complexity: /O((n+m)*min(n,W))/ time and /O(n)/ space.
--
-- @
-- topSort (1 * 2 + 3 * 1)                    == Right [3,1,2]
-- topSort ('path' [1..5])                      == Right [1..5]
-- topSort (3 * (1 * 4 + 2 * 5))              == Right [3,1,2,4,5]
-- topSort (1 * 2 + 2 * 1)                    == Left (2 ':|' [1])
-- topSort ('path' [5,4..1] + 'edge' 2 4)         == Left (4 ':|' [3,2])
-- topSort ('circuit' [1..3])                   == Left (3 ':|' [1,2])
-- topSort ('circuit' [1..3] + 'circuit' [3,2,1]) == Left (3 ':|' [2])
-- topSort (1*2 + 2*1 + 3*4 + 4*3 + 5*1)      == Left (1 ':|' [2])
-- fmap ('flip' 'isTopSortOf' x) (topSort x)      /= Right False
-- topSort . 'vertices'                         == Right . 'nub' . 'sort'
-- @
topSort :: AdjacencyIntMap -> Either (Cycle Int) [Int]
topSort g = runContT (evalStateT (topSort' g) (S IntMap.empty [])) id 

-- | Check if a given graph is /acyclic/.
--
--   Complexity: /O((n+m)*min(n,W))/ time and /O(n)/ space.
--
-- @
-- isAcyclic (1 * 2 + 3 * 1) == True
-- isAcyclic (1 * 2 + 2 * 1) == False
-- isAcyclic . 'circuit'       == 'null'
-- isAcyclic                 == 'isRight' . 'topSort'
-- @
isAcyclic :: AdjacencyIntMap -> Bool
isAcyclic = isRight . topSort

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
