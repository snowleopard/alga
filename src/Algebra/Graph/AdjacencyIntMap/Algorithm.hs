{-# language LambdaCase, BangPatterns #-}

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
import Data.Either
import Data.List.NonEmpty (NonEmpty(..),(<|))
import Data.Monoid
import Data.Tree
import GHC.Exts (toList,fromList)

import Algebra.Graph.AdjacencyIntMap
import Algebra.Graph.Internal

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.IntMap.Strict         as IntMap
import qualified Data.IntSet                as IntSet
import qualified Data.List                  as List

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
scc :: AdjacencyIntMap -> AM.AdjacencyMap AdjacencyIntMap
scc g = condense g $ execState (gabowSCC g) initialState where
  initialState = C 0 0 [] [] IntMap.empty IntMap.empty IntMap.empty [] []


-- StateSCC is the data type used to keep track of state while
-- calculating the strongly connected components of a graph with
-- gabowSCC.
type PreorderId = Int
type ComponentId = Int

data StateSCC
  = C { -- on entry, a vertex is given a preorder id to be stored in
        -- the preorders table.
        preorder      :: !PreorderId
        
        -- when a scc is found, its vertices are assigned a component
        -- id.
      , component     :: !ComponentId
      
        -- the so called boundary stack of Gabow's algorithm. We store
        -- (preorder id, vertex) pairs to avoid log(n) cost in lookup
        -- when calling popBoundary.
      , boundary      :: ![(PreorderId,Int)]
      
        -- the so called path stack of Gabow's algorithm. It is a list
        -- of vertices in the path of the dfs search tree during
        -- traversal. Vertices are popped when a new scc is found.
      , dfsPath       :: ![Int]
      
        -- the table storing preorder ids for vertices.
      , preorders     :: !(IntMap.IntMap PreorderId)
      
        -- the table storing the component id to which vertices have
        -- been assigned.
      , components    :: !(IntMap.IntMap ComponentId)
      
        -- the remaining fields are used for calculating the
        -- condesation of the traversed graph.  inner_graphs stores a
        -- table of strongly connected components indexed by their
        -- component id.  inner_edges holds a list of edges that has
        -- been classified as inner during the traversal. They are
        -- stored with a preorderId of the target vertex to
        -- distinguish which connected component each vertex belongs
        -- to. outer_edges holds a list of edges classified as between
        -- strongly connected components. It and inner_graphs are lazy
        -- because they are not always required to compute the
        -- condensation.
      , inner_graphs  :: IntMap.IntMap (List AdjacencyIntMap)
      , inner_edges   :: ![(PreorderId,(Int,Int))]
      , outer_edges   :: [(Int,Int)]
      } deriving (Show)

gabowSCC :: AdjacencyIntMap -> State StateSCC ()
gabowSCC g =
  do let adjacent = IntSet.toList . flip postIntSet g
         dfs u = do p_u <- enter u
                    forM_ (adjacent u) $ \v ->
                      preorderId v >>= \case
                        Nothing  -> do
                          updated <- dfs v
                          if updated then outedge (u,v) else inedge (p_u,(u,v))
                        Just p_v -> do
                          scc_v <- hasComponent v
                          if scc_v
                            then outedge (u,v)
                            else popBoundary p_v >> inedge (p_u,(u,v))
                    exit u
     forM_ (vertexList g) $ \v -> do
       assigned <- hasPreorderId v
       unless assigned $ void $ dfs v
  where
    -- called when visiting vertex v. assigns preorder number to v,
    -- adds the (id, v) pair to the boundary stack b, and adds v to
    -- the path stack s.
    enter v = do C pre scc bnd pth pres sccs gs es_i es_o <- get
                 let !pre' = pre+1
                     !bnd' = (pre,v):bnd
                     !pth' = v:pth
                     !pres' = IntMap.insert v pre pres
                 put $! C pre' scc bnd' pth' pres' sccs gs es_i es_o
                 return pre

    -- called on back edges. pops the boundary stack while the top
    -- vertex has a larger preorder number than p_v.
    popBoundary p_v = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         C pre scc (dropWhile ((>p_v).fst) bnd) pth pres sccs gs es_i es_o)

    -- called when exiting vertex v. if v is the bottom of a scc
    -- boundary, we add a new SCC, otherwise v is part of a larger scc
    -- being constructed and we continue.
    exit v = do
      new_component <- ((v==).snd.head) <$> gets boundary
      when new_component $ insert_component v
      return new_component

    insert_component v = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         let gs' = IntMap.insert scc g_i gs
             sccs' = List.foldl' (\sccs x -> IntMap.insert x scc sccs) sccs curr
             scc' = scc + 1
             bnd' = tail bnd
             p_v = fst $ head bnd
             g_i = fromList (vertex <$> curr) <> fromList (uncurry edge.snd <$> es)
             (es,es_i') = span ((>=p_v).fst) es_i
             pth' = tail $ dropWhile (/=v) pth
             curr = v:takeWhile(/=v) pth
          in C pre scc' bnd' pth' pres sccs' gs' es_i' es_o)

    inedge uv = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         C pre scc bnd pth pres sccs gs (uv:es_i) es_o)

    outedge uv = modify'
      (\(C pre scc bnd pth pres sccs gs es_i es_o) ->
         C pre scc bnd pth pres sccs gs es_i (uv:es_o))

    hasPreorderId v = gets (IntMap.member v . preorders)
    preorderId    v = gets (IntMap.lookup v . preorders)
    hasComponent  v = gets (IntMap.member v . components)

condense :: AdjacencyIntMap -> StateSCC -> AM.AdjacencyMap AdjacencyIntMap
condense g (C _ n _ _ _ assignment inner _ outer)
  | n == 1 = AM.vertex g
  | otherwise = AM.gmap (inner' IntMap.!) outer'
  where inner' = overlays . toList <$> inner
        outer' = AM.overlays $ es ++ vs
        vs = AM.vertex <$> [0..n-1]
        es = [ AM.edge (sccid x) (sccid y) | (x,y) <- outer ]
        sccid v = assignment IntMap.! v

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
