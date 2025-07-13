{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.Algorithm
-- Copyright  : (c) Andrey Mokhov 2016-2025
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
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State.Strict
import Data.Foldable (for_)
import Data.Either
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Maybe
import Data.Tree (Forest, Tree (..), flatten, levels, unfoldForestM_BF)

import Algebra.Graph.AdjacencyMap

import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Array                          as Array
import qualified Data.List                           as List
import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set

-- | Compute the /breadth-first search/ forest of a graph, such that adjacent
-- vertices are explored in increasing order according to their 'Ord' instance.
-- The search is seeded by a list of vertices that will become the roots of the
-- resulting forest. Duplicates in the list will have their first occurrence
-- explored and subsequent ones ignored. The seed vertices that do not belong to
-- the graph are also ignored.
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
bfsForest :: Ord a => AdjacencyMap a -> [a] -> Forest a
bfsForest x vs = evalState (explore [ v | v <- vs, hasVertex v x ]) Set.empty
  where
    explore = filterM discovered >=> unfoldForestM_BF walk
    walk v = (v,) <$> adjacentM v
    adjacentM v = filterM discovered $ Set.toList (postSet v x)
    discovered v = do new <- gets (not . Set.member v)
                      when new $ modify' (Set.insert v)
                      return new

-- | A version of 'bfsForest' where the resulting forest is converted to a level
-- structure. Adjacent vertices are explored in the increasing order according
-- to their 'Ord' instance. Flattening the result via @'concat'@ @.@ @'bfs'@ @x@
-- gives an enumeration of reachable vertices in the breadth-first search order.
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
bfs :: Ord a => AdjacencyMap a -> [a] -> [[a]]
bfs x = map concat . List.transpose . map levels . bfsForest x

dfsForestFromImpl :: Ord a => AdjacencyMap a -> [a] -> Forest a
dfsForestFromImpl g vs = evalState (explore vs) Set.empty
  where
    explore (v:vs) = discovered v >>= \case
      True -> (:) <$> walk v <*> explore vs
      False -> explore vs
    explore [] = return []
    walk v = Node v <$> explore (adjacent v)
    adjacent v = Set.toList (postSet v g)
    discovered v = do new <- gets (not . Set.member v)
                      when new $ modify' (Set.insert v)
                      return new

-- | Compute the /depth-first search/ forest of a graph, where adjacent vertices
-- are explored in the increasing order according to their 'Ord' instance.
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
dfsForest :: Ord a => AdjacencyMap a -> Forest a
dfsForest g = dfsForestFromImpl g (vertexList g)

-- | Compute the /depth-first search/ forest of a graph starting from the given
-- seed vertices, where adjacent vertices are explored in the increasing order
-- according to their 'Ord' instance. Note that the resulting forest does not
-- necessarily span the whole graph, as some vertices may be unreachable. The
-- seed vertices which do not belong to the graph are ignored.
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
dfsForestFrom :: Ord a => AdjacencyMap a -> [a] -> Forest a
dfsForestFrom g vs = dfsForestFromImpl g [ v | v <- vs, hasVertex v g ]

-- | Return the list vertices visited by the /depth-first search/ in a graph,
-- starting from the given seed vertices. Adjacent vertices are explored in the
-- increasing order according to their 'Ord' instance.
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
dfs :: Ord a => AdjacencyMap a -> [a] -> [a]
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
reachable :: Ord a => AdjacencyMap a -> a -> [a]
reachable x y = dfs x [y]

type Cycle = NonEmpty
type Result a = Either (Cycle a) [a]
data NodeState = Entered | Exited
data S a = S { parent :: Map.Map a a
             , entry  :: Map.Map a NodeState
             , order  :: [a] }

topSortImpl :: Ord a => AdjacencyMap a -> StateT (S a) (Cont (Result a)) (Result a)
topSortImpl g = liftCallCC' callCC $ \cyclic ->
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
-- 'isRight' . topSort                          == 'isAcyclic'
-- topSort . 'vertices'                         == Right . 'nub' . 'sort'
-- @
topSort :: Ord a => AdjacencyMap a -> Either (Cycle a) [a]
topSort g = runCont (evalStateT (topSortImpl g) initialState) id
  where
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

-- | Compute the /condensation/ of a graph, where each vertex corresponds to a
-- /strongly-connected component/ of the original graph. Note that component
-- graphs are non-empty, and are therefore of type
-- "Algebra.Graph.NonEmpty.AdjacencyMap".
--
-- Details about the implementation can be found at
-- <https://github.com/jitwit/alga-notes/blob/master/gabow.org gabow-notes>.
--
-- Complexity: /O((n+m)*log n)/ time and /O(n+m)/ space.
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
scc g = condense g $ execState (gabowSCC g) initialState where
  initialState = SCC 0 0 [] [] Map.empty Map.empty [] [] []

data StateSCC a
  = SCC { _preorder     :: {-# unpack #-} !Int
        , _component    :: {-# unpack #-} !Int
        , boundaryStack :: [(Int,a)]
        , _pathStack    :: [a]
        , preorders     :: Map.Map a Int
        , components    :: Map.Map a Int
        , _innerGraphs  :: [AdjacencyMap a]
        , _innerEdges   :: [(Int,(a,a))]
        , _outerEdges   :: [(a,a)]
        } deriving (Show)

gabowSCC :: Ord a => AdjacencyMap a -> State (StateSCC a) ()
gabowSCC g =
  do let dfs u = do p_u <- enter u
                    for_ (postSet u g) $ \v -> do
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
    enter v = do SCC pre scc bnd pth pres sccs gs es_i es_o <- get
                 let pre' = pre+1
                     bnd' = (pre,v):bnd
                     pth' = v:pth
                     pres' = Map.insert v pre pres
                 put $! SCC pre' scc bnd' pth' pres' sccs gs es_i es_o
                 return pre

    -- called on back edges. pops the boundary stack while the top
    -- vertex has a larger preorder number than p_v.
    popBoundary p_v = modify'
      (\(SCC pre scc bnd pth pres sccs gs es_i es_o) ->
         SCC pre scc (dropWhile ((>p_v).fst) bnd) pth pres sccs gs es_i es_o)

    -- called when exiting vertex v. if v is the bottom of a scc
    -- boundary, we add a new SCC, otherwise v is part of a larger scc
    -- being constructed and we continue.
    exit v = do boundaryStack <- gets boundaryStack
                case boundaryStack of
                    (p_top, top) : newBoundaryStack | v == top -> do
                       insertComponent p_top top newBoundaryStack
                       return True

                    _ -> return False

    insertComponent p_v v newBoundaryStack = modify'
      (\(SCC pre scc _oldBoundaryStack pth pres sccs gs es_i es_o) ->
         let (curr,v_pth') = span (/=v) pth
             pth' = drop 1 v_pth' -- Here we know that v_pth' starts with v
             (es,es_i') = span ((>=p_v).fst) es_i
             g_i | null es = vertex v
                 | otherwise = edges (snd <$> es)
             scc' = scc + 1
             sccs' = List.foldl' (\sccs x -> Map.insert x scc sccs) sccs (v:curr)
             gs' = g_i:gs
          in SCC pre scc' newBoundaryStack pth' pres sccs' gs' es_i' es_o)

    inedge uv = modify'
      (\(SCC pre scc bnd pth pres sccs gs es_i es_o) ->
         SCC pre scc bnd pth pres sccs gs (uv:es_i) es_o)

    outedge uv = modify'
      (\(SCC pre scc bnd pth pres sccs gs es_i es_o) ->
         SCC pre scc bnd pth pres sccs gs es_i (uv:es_o))

    hasPreorderId v = gets (Map.member v . preorders)
    preorderId    v = gets (Map.lookup v . preorders)
    hasComponent  v = gets (Map.member v . components)

condense :: Ord a => AdjacencyMap a -> StateSCC a -> AdjacencyMap (NonEmpty.AdjacencyMap a)
condense g (SCC _ n _ _ _ assignment inner _ outer)
  | n == 1 = vertex $ convert g
  | otherwise = gmap (\c -> inner' Array.! (n-1-c)) outer'
  where inner' = Array.listArray (0,n-1) (convert <$> inner)
        outer' = es `overlay` vs
        vs = vertices [0..n-1]
        es = edges [ (sccid x, sccid y) | (x,y) <- outer ]
        sccid v = assignment Map.! v
        convert = fromJust . NonEmpty.toNonEmpty

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
