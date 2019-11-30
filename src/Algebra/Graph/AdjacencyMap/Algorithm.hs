{-# language LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.Algorithm
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
-- implemented for the "Algebra.Graph.AdjacencyMap" data type.
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.Algorithm (
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
import Data.Either
import Data.List.NonEmpty (NonEmpty(..),(<|))
import Data.Maybe
import Data.Monoid
import Data.Tree

import Algebra.Graph.AdjacencyMap

import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.List                           as List
import qualified Data.Map.Strict                     as Map
import qualified Data.IntMap.Strict                  as IntMap
import qualified Data.Set                            as Set

-- | Compute the /breadth-first search/ forest of a graph, such that
--   adjacent vertices are explored in increasing order with respect
--   to their 'Ord' instance. The search is seeded by a list of
--   argument vertices that will be the roots of the resulting
--   forest. Duplicates in the list will have their first occurrence
--   expanded and subsequent ones ignored. Argument vertices not in
--   the graph are also ignored. 
--
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*log n)/ time and /O(n)/ space.
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
bfsForest :: Ord a => [a] -> AdjacencyMap a -> Forest a
bfsForest vs g = evalState (explore [ v | v <- vs, hasVertex v g ]) Set.empty where
  explore = unfoldForestM_BF walk <=< filterM discovered
  walk v = (v,) <$> adjacentM v
  adjacentM v = filterM discovered $ Set.toList (postSet v g)
  discovered v = do new <- gets (not . Set.member v)
                    when new $ modify' (Set.insert v)
                    return new

-- | This is 'bfsForest' with the resulting forest converted to a
--   level structure. Adjacent vertices are explored in increasing
--   order with respect to their 'Ord' instance. Flattening the result
--   via @'concat' . 'bfs' vs@ gives an enumeration of vertices
--   reachable from @vs@ in breadth first order.
--
--   Let /L/ be the number of seed vertices. Complexity:
--   /O((L+m)*log n)/ time and /O(n)/ space.
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
bfs :: Ord a => [a] -> AdjacencyMap a -> [[a]]
bfs vs = map concat . List.transpose . map levels . bfsForest vs

-- | Compute the /depth-first search/ forest of a graph, where
--   adjacent vertices are expanded in increasing order with respect
--   to their 'Ord' instance.
--
--   Complexity: /O((n+m)*log n)/ time and /O(n)/ space.
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
dfsForest :: Ord a => AdjacencyMap a -> Forest a
dfsForest g = dfsForestFrom' (vertexList g) g

-- | Compute the /depth-first search/ forest of a graph from the given
--   vertices, where adjacent vertices are expanded in increasing
--   order with respect to their 'Ord' instance. Note that the
--   resulting forest does not necessarily span the whole graph, as
--   some vertices may be unreachable. Any of the given vertices which
--   are not in the graph are ignored.
-- 
--   Let /L/ be the number of seed vertices. Complexity: /O((L+m)*log n)/
--   time and /O(n)/ space.
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
--  'forest' (dfsForestFrom [3] $ 'circuit' [1..5] + 'circuit' [5,4..1]) == 'path' [3,2,1,5,4]
-- @
dfsForestFrom :: Ord a => [a] -> AdjacencyMap a -> Forest a
dfsForestFrom vs g = dfsForestFrom' [ v | v <- vs, hasVertex v g ] g

dfsForestFrom' :: Ord a => [a] -> AdjacencyMap a -> Forest a
dfsForestFrom' vs g = evalState (explore vs) Set.empty where
  explore (v:vs) = discovered v >>= \case
    True -> (:) <$> walk v <*> explore vs
    False -> explore vs 
  explore [] = return []
  walk v = Node v <$> explore (adjacent v)
  adjacent v = Set.toList (postSet v g)
  discovered v = do new <- gets (not . Set.member v)
                    when new $ modify' (Set.insert v)
                    return new

-- | Compute the vertices visited by /depth-first search/ in a graph
--   from the given vertices. Adjacent vertices are expanded in
--   increasing order with respect to their 'Ord' instance.
--
--   Let /L/ be the number of seed vertices. Complexity: /O((L+m)*log n)/
--   time and /O(n)/ space.
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
dfs :: Ord a => [a] -> AdjacencyMap a -> [a]
dfs vs = dfsForestFrom vs >=> flatten

-- | Compute the list of vertices that are /reachable/ from a given
--   source vertex in a graph. The vertices in the resulting list
--   appear in /depth-first order/.
-- 
--   Complexity: /O(m*log n)/ time and /O(n)/ space.
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
reachable :: Ord a => a -> AdjacencyMap a -> [a]
reachable x = dfs [x]

type Cycle = NonEmpty 
data NodeState = Entered | Exited
data S a = S { parent :: Map.Map a a
             , entry  :: Map.Map a NodeState
             , order  :: [a] }

topSort' :: (Ord a, MonadState (S a) m, MonadCont m)
         => AdjacencyMap a -> m (Either (Cycle a) [a])
topSort' g = callCC $ \cyclic ->
  do let vertices = map fst $ Map.toDescList $ adjacencyMap g
         adjacent = Set.toDescList . flip postSet g
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
    nodeState v = gets (Map.lookup v . entry)
    enter u v = modify' (\(S m n vs) -> S (Map.insert v u m)
                                          (Map.insert v Entered n)
                                          vs)
    enterRoot v = modify' (\(S m n vs) -> S m (Map.insert v Entered n) vs)
    exit v = modify' (\(S m n vs) -> S m (Map.alter (fmap leave) v n) (v:vs))
      where leave = \case
              Entered -> Exited
              Exited  -> error "Internal error: dfs search order violated"
    retrace curr head parent = aux (curr :| []) where
      aux xs@(curr :| _)
        | head == curr = xs
        | otherwise = aux (parent Map.! curr <| xs)

-- | Compute a topological sort of a DAG or discover a cycle.
--
--   Vertices are expanded in decreasing order with respect to their
--   'Ord' instance. This gives the lexicographically smallest
--   topological ordering in the case of success. In the case of
--   failure, the cycle is characterized by being the
--   lexicographically smallest up to rotation with respect to @Ord
--   (Dual a)@ in the first connected component of the graph
--   containing a cycle, where the connected components are ordered by
--   their largest vertex with respect to @Ord a@.
--
--   Complexity: /O((n+m)*log n)/ time and /O(n)/ space.
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
-- 'isRight' . topSort                          == 'isAcyclic'
-- topSort . 'vertices'                         == Right . 'nub' . 'sort'
-- @
topSort :: Ord a => AdjacencyMap a -> Either (Cycle a) [a]
topSort g = runContT (evalStateT (topSort' g) initialState) id where
  initialState = S Map.empty Map.empty []

-- | Check if a given graph is /acyclic/.
-- 
--   Complexity: /O((n+m)*log n)/ time and /O(n)/ space.
--
-- @
-- isAcyclic (1 * 2 + 3 * 1) == True
-- isAcyclic (1 * 2 + 2 * 1) == False
-- isAcyclic . 'circuit'       == 'null'
-- isAcyclic                 == 'isRight' . 'topSort'
-- @
isAcyclic :: Ord a => AdjacencyMap a -> Bool
isAcyclic = isRight . topSort

-- TODO: Benchmark and optimise.
-- | Compute the /condensation/ of a graph, where each vertex corresponds to a
-- /strongly-connected component/ of the original graph. Note that component
-- graphs are non-empty, and are therefore of type
-- "Algebra.Graph.NonEmpty.AdjacencyMap".
--
-- @
-- scc 'empty'               == 'empty'
-- scc ('vertex' x)          == 'vertex' (NonEmpty.'NonEmpty.vertex' x)
-- scc ('vertices' xs)       == 'vertices' ('map' 'NonEmpty.vertex' xs)
-- scc ('edge' 1 1)          == 'vertex' (NonEmpty.'NonEmpty.edge' 1 1)
-- scc ('edge' 1 2)          == 'edge'   (NonEmpty.'NonEmpty.vertex' 1) (NonEmpty.'NonEmpty.vertex' 2)
-- scc ('circuit' (1:xs))    == 'vertex' (NonEmpty.'NonEmpty.circuit1' (1 'Data.List.NonEmpty.:|' xs))
-- scc (3 * 1 * 4 * 1 * 5) == 'edges'  [ (NonEmpty.'NonEmpty.vertex'  3      , NonEmpty.'NonEmpty.vertex'  5      )
--                                   , (NonEmpty.'NonEmpty.vertex'  3      , NonEmpty.'NonEmpty.clique1' [1,4,1])
--                                   , (NonEmpty.'NonEmpty.clique1' [1,4,1], NonEmpty.'NonEmpty.vertex'  5      ) ]
-- 'isAcyclic' . scc == 'const' True
-- 'isAcyclic' x     == (scc x == 'gmap' NonEmpty.'NonEmpty.vertex' x)
-- @
scc :: Ord a => AdjacencyMap a -> AdjacencyMap (NonEmpty.AdjacencyMap a)
scc g = evalState (scc' g) initialState where
  initialState = C 0 0 [] [] Map.empty Map.empty

data StateSCC a
  = C { current       :: !Int
      , componentId   :: !Int
      , boundary      :: ![(Int,a)]
      , dfsPath       :: ![a]
      , preorders     :: !(Map.Map a Int)
      , components    :: !(Map.Map a Int) }

-- gabow path-based scc algorithm
scc' :: Ord a => AdjacencyMap a ->
        State (StateSCC a) (AdjacencyMap (NonEmpty.AdjacencyMap a))
scc' g =
  do let adjacent = Set.toList . flip postSet g
         dfs u = do enter u
                    forM_ (adjacent u) $ \v ->
                      preorderId v >>= \case
                        Nothing  -> dfs v
                        Just p_v -> do
                          scc_v <- hasComponent v
                          unless scc_v $ popBoundary p_v
                    exit u
     forM_ (vertexList g) $ \v -> do
       assigned <- hasPreorderId v
       unless assigned $ dfs v
     convertRepresentation
  where
    -- called when visiting vertex v. assigns preorder number to v,
    -- adds the (id, v) pair to the boundary stack b, and adds v to
    -- the path stack s.
    enter v = modify'
      (\(C c i b s t ids) ->
         C (c + 1) i ((c,v):b) (v:s) (Map.insert v c t) ids)

    -- called on back edges. pops the boundary stack until a vertex
    -- with a strictly smaller preorder number than p_v is at the top
    popBoundary p_v = modify'
      (\(C c i b s t ids) ->
         C c i (dropWhile ((>p_v).fst) b) s t ids)

    -- called when exiting vertex v. if v is the bottom of a scc
    -- boundary, we add a new SCC, otherwise v is part of a larger scc
    -- being constructed and we continue.
    exit v = modify'
      (\sccState@(C c i b s t ids) ->
       if v /= snd (head b) then sccState
       else let curr = v:takeWhile (/= v) s
                s' = tail $ dropWhile (/= v) s
                ids' = List.foldl' (\sccs x -> Map.insert x i sccs) ids curr
             in C c (i + 1) (tail b) s' t ids')

    hasPreorderId v = gets (Map.member v . preorders)
    preorderId    v = gets (Map.lookup v . preorders)
    hasComponent  v = gets (Map.member v . components)

    convertRepresentation = do
      scc_count <- gets componentId
      if scc_count == 1
      then return (vertex $ fromJust $ NonEmpty.toNonEmpty g)
      else classifyEdges <$> gets components

    runE = flip appEndo []
    classifyEdges assignment = gmap (sccs IntMap.!) es where
      sccs = fromJust . NonEmpty.toNonEmpty . overlays . runE <$> sccs'
      es = overlays $ runE es'
      (sccs',es') = Map.foldrWithKey condense (IntMap.empty,mempty) (adjacencyMap g) where
        condense u vs (m_scc,es_scc) = (m_scc',es_scc' <> es_scc) where
          es_scc' = Endo $ (++) (vertex scc_u:(edge scc_u.snd <$> inters))
          m_scc' = IntMap.insertWith (<>) scc_u (Endo $ (++) (vertex u:(edge u.fst <$> intras))) m_scc
          scc_vs = [ (v,assignment Map.! v) | v <- Set.toList vs ]
          (intras,inters) = List.partition ((==scc_u).snd) scc_vs
          scc_u = assignment Map.! u

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
isDfsForestOf :: Ord a => Forest a -> AdjacencyMap a -> Bool
isDfsForestOf f am = case go Set.empty f of
    Just seen -> seen == vertexSet am
    Nothing   -> False
  where
    go seen []     = Just seen
    go seen (t:ts) = do
        let root = rootLabel t
        guard $ root `Set.notMember` seen
        guard $ and [ hasEdge root (rootLabel subTree) am | subTree <- subForest t ]
        newSeen <- go (Set.insert root seen) (subForest t)
        guard $ postSet root am `Set.isSubsetOf` newSeen
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
isTopSortOf :: Ord a => [a] -> AdjacencyMap a -> Bool
isTopSortOf xs m = go Set.empty xs
  where
    go seen []     = seen == Map.keysSet (adjacencyMap m)
    go seen (v:vs) = postSet v m `Set.intersection` newSeen == Set.empty
                  && go newSeen vs
      where
        newSeen = Set.insert v seen
