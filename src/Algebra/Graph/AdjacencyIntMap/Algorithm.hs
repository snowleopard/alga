{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyIntMap.Algorithm
-- Copyright  : (c) Andrey Mokhov 2016-2023
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
    bfsForest, bfs, dfsForest, dfsForestFrom, dfs, reachable,
    topSort, isAcyclic,

    -- * Correctness properties
    isDfsForestOf, isTopSortOf,

    -- * Type synonyms
    Cycle
    ) where

import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State.Strict
import Data.Either
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Tree

import Algebra.Graph.AdjacencyIntMap

import qualified Data.List          as List
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet        as IntSet

-- | Compute the /breadth-first search/ forest of a graph, such that adjacent
-- vertices are explored in the increasing order. The search is seeded by a list
-- of vertices that will become the roots of the resulting forest. Duplicates in
-- the list will have their first occurrence explored and subsequent ones
-- ignored. The seed vertices that do not belong to the graph are also ignored.
--
-- Complexity: /O((L + m) * log n)/ time and /O(n)/ space, where /L/ is the
-- number of seed vertices.
--
-- @
-- 'forest' $ bfsForest ('edge' 1 2) [0]        == 'empty'
-- 'forest' $ bfsForest ('edge' 1 2) [1]        == 'edge' 1 2
-- 'forest' $ bfsForest ('edge' 1 2) [2]        == 'vertex' 2
-- 'forest' $ bfsForest ('edge' 1 2) [0,1,2]    == 'vertices' [1,2]
-- 'forest' $ bfsForest ('edge' 1 2) [2,1,0]    == 'vertices' [1,2]
-- 'forest' $ bfsForest ('edge' 1 1) [1]        == 'vertex' 1
-- 'isSubgraphOf' ('forest' $ bfsForest x vs) x == True
-- bfsForest x ('vertexList' x)               == 'map' (\\v -> Node v []) ('Data.List.nub' $ 'vertexList' x)
-- bfsForest x []                           == []
-- bfsForest 'empty' vs                       == []
-- bfsForest (3 * (1 + 4) * (1 + 5)) [1,4]  == [ Node { rootLabel = 1
--                                                    , subForest = [ Node { rootLabel = 5
--                                                                         , subForest = [] }]}
--                                             , Node { rootLabel = 4
--                                                    , subForest = [] }]
-- 'forest' $ bfsForest ('circuit' [1..5] + 'circuit' [5,4..1]) [3] == 'path' [3,2,1] + 'path' [3,4,5]
--
-- @
bfsForest :: AdjacencyIntMap -> [Int] -> Forest Int
bfsForest g vs= evalState (explore [ v | v <- vs, hasVertex v g ]) IntSet.empty
  where
    explore = filterM discovered >=> unfoldForestM_BF walk
    walk v = (v,) <$> adjacentM v
    adjacentM v = filterM discovered $ IntSet.toList (postIntSet v g)
    discovered v = do new <- gets (not . IntSet.member v)
                      when new $ modify' (IntSet.insert v)
                      return new

-- | A version of 'bfsForest' where the resulting forest is converted to a level
-- structure. Adjacent vertices are explored in the increasing order. Flattening
-- the result via @'concat'@ @.@ @'bfs'@ @x@ gives an enumeration of reachable
-- vertices in the breadth-first search order.
--
-- Complexity: /O((L + m) * min(n,W))/ time and /O(n)/ space, where /L/ is the
-- number of seed vertices.
--
-- @
-- bfs ('edge' 1 2) [0]                == []
-- bfs ('edge' 1 2) [1]                == [[1], [2]]
-- bfs ('edge' 1 2) [2]                == [[2]]
-- bfs ('edge' 1 2) [1,2]              == [[1,2]]
-- bfs ('edge' 1 2) [2,1]              == [[2,1]]
-- bfs ('edge' 1 1) [1]                == [[1]]
-- bfs 'empty' vs                      == []
-- bfs x []                          == []
-- bfs (1 * 2 + 3 * 4 + 5 * 6) [1,2] == [[1,2]]
-- bfs (1 * 2 + 3 * 4 + 5 * 6) [1,3] == [[1,3], [2,4]]
-- bfs (3 * (1 + 4) * (1 + 5)) [3]   == [[3], [1,4,5]]
--
-- bfs ('circuit' [1..5] + 'circuit' [5,4..1]) [3]          == [[2], [1,3], [5,4]]
-- 'concat' $ bfs ('circuit' [1..5] + 'circuit' [5,4..1]) [3] == [3,2,4,1,5]
-- 'map' 'concat' . 'List.transpose' . 'map' 'levels' . 'bfsForest' x    == bfs x
-- @
bfs :: AdjacencyIntMap -> [Int] -> [[Int]]
bfs g = map concat . List.transpose . map levels . bfsForest g

dfsForestFromImpl :: AdjacencyIntMap -> [Int] -> Forest Int
dfsForestFromImpl g vs = evalState (explore vs) IntSet.empty
  where
    explore (v:vs) = discovered v >>= \case
      True -> (:) <$> walk v <*> explore vs
      False -> explore vs
    explore [] = return []
    walk v = Node v <$> explore (adjacent v)
    adjacent v = IntSet.toList (postIntSet v g)
    discovered v = do new <- gets (not . IntSet.member v)
                      when new $ modify' (IntSet.insert v)
                      return new

-- | Compute the /depth-first search/ forest of a graph, where adjacent vertices
-- are explored in the increasing order.
--
-- Complexity: /O((n + m) * min(n,W))/ time and /O(n)/ space.
--
-- @
-- 'forest' $ dfsForest 'empty'              == 'empty'
-- 'forest' $ dfsForest ('edge' 1 1)         == 'vertex' 1
-- 'forest' $ dfsForest ('edge' 1 2)         == 'edge' 1 2
-- 'forest' $ dfsForest ('edge' 2 1)         == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ dfsForest x) x == True
-- 'isDfsForestOf' (dfsForest x) x         == True
-- dfsForest . 'forest' . dfsForest        == dfsForest
-- dfsForest ('vertices' vs)               == 'map' (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
-- dfsForest $ 3 * (1 + 4) * (1 + 5)     == [ Node { rootLabel = 1
--                                                 , subForest = [ Node { rootLabel = 5
--                                                                      , subForest = [] }]}
--                                          , Node { rootLabel = 3
--                                                 , subForest = [ Node { rootLabel = 4
--                                                                      , subForest = [] }]}]
-- 'forest' (dfsForest $ 'circuit' [1..5] + 'circuit' [5,4..1]) == 'path' [1,2,3,4,5]
-- @
dfsForest :: AdjacencyIntMap -> Forest Int
dfsForest g = dfsForestFromImpl g (vertexList g)

-- | Compute the /depth-first search/ forest of a graph starting from the given
-- seed vertices, where adjacent vertices are explored in the increasing order.
-- Note that the resulting forest does not necessarily span the whole graph, as
-- some vertices may be unreachable. The seed vertices which do not belong to
-- the graph are ignored.
--
-- Complexity: /O((L + m) * log n)/ time and /O(n)/ space, where /L/ is the
-- number of seed vertices.
--
-- @
-- 'forest' $ dfsForestFrom 'empty'      vs             == 'empty'
-- 'forest' $ dfsForestFrom ('edge' 1 1) [1]            == 'vertex' 1
-- 'forest' $ dfsForestFrom ('edge' 1 2) [0]            == 'empty'
-- 'forest' $ dfsForestFrom ('edge' 1 2) [1]            == 'edge' 1 2
-- 'forest' $ dfsForestFrom ('edge' 1 2) [2]            == 'vertex' 2
-- 'forest' $ dfsForestFrom ('edge' 1 2) [1,2]          == 'edge' 1 2
-- 'forest' $ dfsForestFrom ('edge' 1 2) [2,1]          == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ dfsForestFrom x vs) x     == True
-- 'isDfsForestOf' (dfsForestFrom x ('vertexList' x)) x == True
-- dfsForestFrom x ('vertexList' x)                   == 'dfsForest' x
-- dfsForestFrom x []                               == []
-- dfsForestFrom (3 * (1 + 4) * (1 + 5)) [1,4]      == [ Node { rootLabel = 1
--                                                            , subForest = [ Node { rootLabel = 5
--                                                                                 , subForest = [] }
--                                                     , Node { rootLabel = 4
--                                                            , subForest = [] }]
-- 'forest' $ dfsForestFrom ('circuit' [1..5] + 'circuit' [5,4..1]) [3] == 'path' [3,2,1,5,4]
-- @
dfsForestFrom :: AdjacencyIntMap -> [Int] -> Forest Int
dfsForestFrom g vs = dfsForestFromImpl g [ v | v <- vs, hasVertex v g ]


-- | Return the list vertices visited by the /depth-first search/ in a graph,
-- starting from the given seed vertices. Adjacent vertices are explored in the
-- increasing order.
--
-- Complexity: /O((L + m) * log n)/ time and /O(n)/ space, where /L/ is the
-- number of seed vertices.
--
-- @
-- dfs 'empty'      vs    == []
-- dfs ('edge' 1 1) [1]   == [1]
-- dfs ('edge' 1 2) [0]   == []
-- dfs ('edge' 1 2) [1]   == [1,2]
-- dfs ('edge' 1 2) [2]   == [2]
-- dfs ('edge' 1 2) [1,2] == [1,2]
-- dfs ('edge' 1 2) [2,1] == [2,1]
-- dfs x          []    == []
--
-- 'Data.List.and' [ 'hasVertex' v x | v <- dfs x vs ]       == True
-- dfs (3 * (1 + 4) * (1 + 5)) [1,4]           == [1,5,4]
-- dfs ('circuit' [1..5] + 'circuit' [5,4..1]) [3] == [3,2,1,5,4]
-- @
dfs :: AdjacencyIntMap -> [Int] -> [Int]
dfs x = concatMap flatten . dfsForestFrom x

-- | Return the list of vertices /reachable/ from a source vertex in a graph.
-- The vertices in the resulting list appear in the /depth-first search order/.
--
-- Complexity: /O(m * log n)/ time and /O(n)/ space.
--
-- @
-- reachable 'empty'              x == []
-- reachable ('vertex' 1)         1 == [1]
-- reachable ('edge' 1 1)         1 == [1]
-- reachable ('edge' 1 2)         0 == []
-- reachable ('edge' 1 2)         1 == [1,2]
-- reachable ('edge' 1 2)         2 == [2]
-- reachable ('path'    [1..8]  ) 4 == [4..8]
-- reachable ('circuit' [1..8]  ) 4 == [4..8] ++ [1..3]
-- reachable ('clique'  [8,7..1]) 8 == [8] ++ [1..7]
--
-- 'Data.List.and' [ 'hasVertex' v x | v <- reachable x y ] == True
-- @
reachable :: AdjacencyIntMap -> Int -> [Int]
reachable x y = dfs x [y]

type Cycle = NonEmpty
type Result = Either (Cycle Int) [Int]
data NodeState = Entered | Exited
data S = S { parent :: IntMap.IntMap Int
           , entry  :: IntMap.IntMap NodeState
           , order  :: [Int] }

topSortImpl :: AdjacencyIntMap -> StateT S (Cont Result) Result
topSortImpl g = liftCallCC' callCC $ \cyclic ->
  do let vertices = map fst $ IntMap.toDescList $ adjacencyIntMap g
         adjacent = IntSet.toDescList . flip postIntSet g
         dfsRoot x = nodeState x >>= \case
           Nothing -> enterRoot x >> dfs x >> exit x
           _       -> return ()
         dfs x = forM_ (adjacent x) $ \y ->
                   nodeState y >>= \case
                     Nothing      -> enter x y >> dfs y >> exit y
                     Just Exited  -> return ()
                     Just Entered -> cyclic . Left . retrace x y =<< gets parent
     forM_ vertices dfsRoot
     Right <$> gets order
  where
    nodeState v = gets (IntMap.lookup v . entry)
    enter u v = modify' (\(S m n vs) -> S (IntMap.insert v u m)
                                          (IntMap.insert v Entered n)
                                          vs)
    enterRoot v = modify' (\(S m n vs) -> S m (IntMap.insert v Entered n) vs)
    exit v = modify' (\(S m n vs) -> S m (IntMap.alter (fmap leave) v n) (v:vs))
      where leave = \case
              Entered -> Exited
              Exited  -> error "Internal error: dfs search order violated"
    retrace curr head parent = aux (curr :| []) where
      aux xs@(curr :| _)
        | head == curr = xs
        | otherwise = aux (parent IntMap.! curr <| xs)

-- | Compute a topological sort of a graph or discover a cycle.
--
-- Vertices are explored in the decreasing order according to their 'Ord'
-- instance. This gives the lexicographically smallest topological ordering in
-- the case of success. In the case of failure, the cycle is characterized by
-- being the lexicographically smallest up to rotation with respect to
-- @Ord@ @(Dual@ @Int)@ in the first connected component of the graph containing
-- a cycle, where the connected components are ordered by their largest vertex
-- with respect to @Ord a@.
--
-- Complexity: /O((n + m) * min(n,W))/ time and /O(n)/ space.
--
-- @
-- topSort (1 * 2 + 3 * 1)                    == Right [3,1,2]
-- topSort ('path' [1..5])                      == Right [1..5]
-- topSort (3 * (1 * 4 + 2 * 5))              == Right [3,1,2,4,5]
-- topSort (1 * 2 + 2 * 1)                    == Left (2 ':|' [1])
-- topSort ('path' [5,4..1] + 'edge' 2 4)         == Left (4 ':|' [3,2])
-- topSort ('circuit' [1..3])                   == Left (3 ':|' [1,2])
-- topSort ('circuit' [1..3] + 'circuit' [3,2,1]) == Left (3 ':|' [2])
-- topSort (1 * 2 + (5 + 2) * 1 + 3 * 4 * 3)  == Left (1 ':|' [2])
-- fmap ('flip' 'isTopSortOf' x) (topSort x)      /= Right False
-- topSort . 'vertices'                         == Right . 'nub' . 'sort'
-- @
topSort :: AdjacencyIntMap -> Either (Cycle Int) [Int]
topSort g = runCont (evalStateT (topSortImpl g) initialState) id
  where
    initialState = S IntMap.empty IntMap.empty []

-- | Check if a given graph is /acyclic/.
--
-- Complexity: /O((n + m) * min(n,W))/ time and /O(n)/ space.
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
