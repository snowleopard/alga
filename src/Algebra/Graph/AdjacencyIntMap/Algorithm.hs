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
    bfsForest, bfs, dfsForest, dfsForestFrom, dfs, reachable,
    topSort, isAcyclic, scc,
    
    -- * Correctness properties
    isDfsForestOf, isTopSortOf,

    -- * Type synonyms
    Cycle
    ) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Coerce
import Data.Either
import Data.List.NonEmpty (NonEmpty(..),(<|))
import Data.Tree

import Algebra.Graph.AdjacencyIntMap

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.IntMap.Strict         as IntMap
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

-- | Compute the /breadth-first search/ forest of a graph, such that
--   adjacent vertices are explored in increasing order with respect
--   to their 'Ord' instance. The search is seeded by a list of
--   argument vertices that will be the roots of the resulting
--   forest. Duplicates in the list will have their first occurrence
--   expanded and subsequent ones ignored. Argument vertices not in
--   the graph are also ignored. 
--
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*min(n,W))/ time and /O(n)/ space.
--
-- @
-- 'forest' (bfsForest [1,2] $ 'edge' 1 2)      == 'vertices' [1,2]
-- 'forest' (bfsForest [2]   $ 'edge' 1 2)      == 'vertex' 2
-- 'forest' (bfsForest [3]   $ 'edge' 1 2)      == 'empty'
-- 'forest' (bfsForest [2,1] $ 'edge' 1 2)      == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForest vs x) x == True
-- bfsForest ('vertexList' g) g               == 'map' (\v -> Node v []) ('nub' $ 'vertexList' g)
-- bfsForest [] x                           == []
-- bfsForest [1,4] (3 * (1 + 4) * (1 + 5))  == [ Node { rootLabel = 1
--                                                    , subForest = [ Node { rootLabel = 5
--                                                                         , subForest = [] }]}
--                                             , Node { rootLabel = 4
--                                                    , subForest = [] }]
-- 'forest' (bfsForest [3] ('circuit' [1..5] + 'circuit' [5,4..1])) == 'path' [3,2,1] + 'path' [3,4,5]
-- 
-- @
bfsForest :: [Int] -> AdjacencyIntMap -> Forest Int
bfsForest vs g = evalState (explore [ v | v <- vs, hasVertex v g ]) IntSet.empty where
  explore = unfoldForestM_BF walk <=< filterM discovered
  walk v = (v,) <$> adjacentM v
  adjacentM v = filterM discovered $ IntSet.toList (postIntSet v g)
  discovered v = do new <- gets (not . IntSet.member v)
                    when new $ modify' (IntSet.insert v)
                    return new

-- | This is 'bfsForest' with the resulting forest converted to a
--   level structure. Adjacent vertices are explored in increasing
--   order with respect to their 'Ord' instance. Flattening the result
--   via @'concat' . 'bfs' vs@ gives an enumeration of vertices
--   reachable from @vs@ in breadth first order.
--
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*min(n,W))/ time and /O(n)/ space.
-- 
-- @
-- bfs vs 'empty'                                         == []
-- bfs [] g                                             == []
-- bfs [1]   ('edge' 1 1)                                 == [[1]]
-- bfs [1]   ('edge' 1 2)                                 == [[1],[2]]
-- bfs [2]   ('edge' 1 2)                                 == [[2]]
-- bfs [1,2] ('edge' 1 2)                                 == [[1,2]]
-- bfs [2,1] ('edge' 1 2)                                 == [[2,1]]
-- bfs [3]   ('edge' 1 2)                                 == []
-- bfs [1,2] ( (1*2) + (3*4) + (5*6) )                  == [[1,2]]
-- bfs [1,3] ( (1*2) + (3*4) + (5*6) )                  == [[1,3],[2,4]]
-- bfs [3] (3 * (1 + 4) * (1 + 5))                      == [[3],[1,4,5]]
-- bfs [2] ('circuit' [1..5] + 'circuit' [5,4..1])          == [[2],[1,3],[5,4]]
-- 'concat' (bfs [3] $ 'circuit' [1..5] + 'circuit' [5,4..1]) == [3,2,4,1,5]
-- bfs vs == 'map' 'concat' . 'List.transpose' . 'map' 'levels' . 'bfsForest' vs
-- @
bfs :: [Int] -> AdjacencyIntMap -> [[Int]]
bfs vs = map concat . List.transpose . map levels . bfsForest vs

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
--   some vertices may be unreachable. Any of the given vertices which
--   are not in the graph are ignored.
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
data NodeState = Entered | Exited
data S = S { parent :: IntMap.IntMap Int
           , entry  :: IntMap.IntMap NodeState
           , order  :: [Int] }

topSort' :: (MonadState S m, MonadCont m)
         => AdjacencyIntMap -> m (Either (Cycle Int) [Int])
topSort' g = callCC $ \cyclic ->
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

-- | Compute a topological sort of a DAG or discover a cycle.
--
--   Vertices are expanded in decreasing order with respect to their
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
topSort g = runContT (evalStateT (topSort' g) initialState) id where
  initialState = S IntMap.empty IntMap.empty []

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

data C = C { current :: !Int -- current preorder number
           , componentId :: !Int -- current component id 
           , boundary :: ![(Int,Int)] -- list of preorder numbers and vertices
           , dfsPath :: ![Int] -- current path in search tree
           , preorders :: !(IntMap.IntMap Int) -- map vertices to preorder num
           , componentIds :: !(IntMap.IntMap Int) -- map vertices to component id
           , componentSets :: !(IntMap.IntMap AdjacencyIntMap) -- adjancymaps of components
           } deriving (Show)

-- gabow path-based scc algorithm
scc' :: MonadState C m => AdjacencyIntMap -> m ()
scc' g =
  do let adjacent = IntSet.toList . flip postIntSet g
         dfs v = do modify' (enter v)
                    forM_ (adjacent v) $ \w ->
                      gets (IntMap.lookup w . preorders) >>= \case
                        -- if w not assigned preorder num, dfs
                        Nothing -> dfs w
                        Just pw -> gets (IntMap.member w . componentIds) >>= \case
                          -- if w is in a new component keep searching
                          True -> return ()
                          -- else grow the current scc
                          False -> modify' (popBoundary pw)
                    modify' (exit v)
     forM_ (vertexList g) $ \v -> do 
       -- v has no preorder iff it is unexplored
       vHasPreorderNum <- gets (IntMap.member v . preorders)
       when (not vHasPreorderNum) $ dfs v
  where
    -- called when visiting vertex v. increments preorder number, logs
    -- it in the table, adds it to the boundary stack b, and adds it
    -- to the path stack s.
    enter v (C c i b s t ids vs) =
      C (1+c) i ((c,v):b) (v:s) (IntMap.insert v c t) ids vs

    -- called on back edges. pops the boundary stack until a vertex
    -- with a smaller preorder num is at the top
    popBoundary x (C c i b s t ids vs) = C c i (dropWhile ((>x).fst) b) s t ids vs

    -- called when exiting vertex v. if v is the bottom of a scc
    -- boundary, we add a new SCC, otherwise we're still building a
    -- scc and the state is unchanged.
    exit v st@(C c i b s t ids vs)
      | v /= snd (head b) = st
      | otherwise = C c (1+i) (tail b) (tail s') t ids' vs' where
          -- find path up to v
          curr = v:takeWhile (/= v) s
          -- find path below v
          s' = dropWhile (/= v) s
          -- assign scc vertices a component id
          ids' = List.foldl' (\sccs x -> IntMap.insert x i sccs) ids curr
          -- get the induced subgraph of the scc
          vs' = IntMap.insert i (induce (`IntSet.member`IntSet.fromList curr) g) vs

scc :: AdjacencyIntMap -> AM.AdjacencyMap AdjacencyIntMap
scc g = AM.gmap (components IntMap.!) $ convert $ removeSelfLoops $ gmap (assignment IntMap.!) g where
  convert = coerce . Map.fromAscList . IntMap.toList . fmap setOfIntSet . adjacencyIntMap
  setOfIntSet = Set.fromAscList . IntSet.toList
  removeSelfLoops m = foldr (\x -> removeEdge x x) m (vertexList m)
  C _ _ _ _ _ assignment components = execState (scc' g) initialState
  initialState = C 0 0 [] [] IntMap.empty IntMap.empty IntMap.empty

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
