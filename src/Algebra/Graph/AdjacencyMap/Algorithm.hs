{-# language LambdaCase, ViewPatterns, PatternSynonyms #-}

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
    bfsForest, bfsForestFrom, bfs, dfsForest, dfsForestFrom, dfs, reachable,
    topSort, isAcyclic, scc,
    
    -- * Correctness properties
    isDfsForestOf, isTopSortOf
    ) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..),(<|))
import Data.Maybe
import Data.Tree

import Algebra.Graph.AdjacencyMap

import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Graph                          as KL
import qualified Data.Graph.Typed                    as Typed
import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set

-- | Compute the /breadth-first search/ forest of a graph, such that
--   adjacent vertices are explored in increasing order with respect
--   to their 'Ord' instance.
-- 
--   Complexity: /O((n+m)*log n)/ time and /O(n)/ space.
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

bfsForest :: Ord a => AdjacencyMap a -> Forest a
bfsForest g = bfsForestFrom' (vertexList g) g

-- | Like 'bfsForest', but the traversal is seeded by a list of
--   vertices. Vertices not in the graph are ignored.
--
--   Let /L/ be the number of seed vertices. Complexity: /O((L+m)*log n)/
--   time and /O(n)/ space.
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
bfsForestFrom :: Ord a => [a] -> AdjacencyMap a -> Forest a
bfsForestFrom vs g = bfsForestFrom' [ v | v <- vs, hasVertex v g ] g

bfsForestFrom' :: Ord a => [a] -> AdjacencyMap a -> Forest a
bfsForestFrom' vs g = evalState (explore vs) Set.empty where
  explore (v:vs) = discovered v >>= \case
    True -> (:) <$> unfoldTreeM_BF walk v <*> explore vs
    False -> explore vs
  explore [] = return []
  walk v = (v,) <$> adjacentM v
  adjacentM v = filterM discovered $ Set.toList (postSet v g)
  discovered v = do new <- gets (not . Set.member v)
                    when new $ modify' (Set.insert v)
                    return new

-- | Like 'bfsForestFrom' with the resulting forest converted to a
--   level structure.  Flattening the result via @'concat' . 'bfs' vs@
--   gives an enumeration of vertices reachable from @vs@ in breadth
--   first order. Adjacent vertices are explored in increasing order
--   with respect to their 'Ord' instance.
--
--   Let /L/ be the number of seed vertices. Complexity: /O((L+m)*log n)/
--   time and /O(n)/ space.
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
bfs :: Ord a => [a] -> AdjacencyMap a -> [[a]]
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
dfsForest :: Ord a => AdjacencyMap a -> Forest a
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
--  'forest' (dfsForestFrom [3] $ 'circuit' [1..5] + 'circuit' [5,4..1]) == 'path' [3,2,1,5,4]
-- @
dfsForestFrom :: Ord a => [a] -> AdjacencyMap a -> Forest a
dfsForestFrom vs g = dfsForestFrom' [ v | v <- vs, hasVertex v g ] g

dfsForestFrom' :: Ord a => [a] -> AdjacencyMap a -> Forest a
dfsForestFrom' vs g = evalState (explore vs) mempty where
  explore (v:vs) = discovered v >>= \case
    True -> (:) <$> walk v <*> explore vs
    False -> explore vs 
  explore [] = return []
  walk v = Node v <$> (explore =<< adjacentM v)
  adjacentM v = filterM undiscovered $ Set.toList (postSet v g)
  undiscovered v = gets (not . Set.member v)
  discovered v = do new <- undiscovered v
                    when new $ modify' (Set.insert v)
                    return new

-- | Compute the list of vertices visited by the /depth-first search/ in a
-- graph, when searching from each of the given vertices in order.
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
-- source vertex in a graph. The vertices in the resulting list appear
-- in /breadth-first order/.
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
reachable x = concat . bfs [x]

type Cycle = NonEmpty 
type Entry a = (Maybe a, Bool)
type TopSort a = Either (Cycle a) [a]
type ParentTable a = Map.Map a (Entry a)
data S a = S { table :: !(ParentTable a), order :: [a] }

-- Tree edge when node has not been explored
pattern TreeEdge :: Maybe (Maybe a , Bool)
pattern TreeEdge <- Nothing
-- Back edge when node is being processed, but not done
pattern BackEdge :: Maybe (Maybe a, Bool)
pattern BackEdge <- Just (_,False)
-- Root of component is denoted by Nothing
pattern Root :: Maybe a
pattern Root = Nothing
-- Pattern to pull out parent from table
pattern Parent :: a -> Maybe (Maybe a, Bool)
pattern Parent p <- Just (Just p,_)

unexplored :: (Ord a, MonadState (S a) m) => a -> m Bool
unexplored u = gets (not . Map.member u . table)

enter :: (Ord a, MonadState (S a) m) => Maybe a -> a -> m ()
enter u v = modify' (\(S p vs) -> S (Map.insert v (u,False) p) vs)

exit :: (Ord a, MonadState (S a) m) => a -> m ()
exit v = modify' (\(S p vs) -> S (Map.alter mark_done v p) (v:vs)) where
  mark_done = fmap (fmap (const True))

classify :: (Ord a, MonadState (S a) m) => a -> m (Maybe (Entry a))
classify v = gets (Map.lookup v . table)

retrace :: Ord a => a -> a -> ParentTable a -> Cycle a
retrace x x0 table = aux (x :| []) where
  aux xs@(x :| _) | x0 == x = xs
                  | otherwise = case Map.lookup x table of
                      Parent z -> aux (z <| xs)
                      _ -> error "impossible"

topSort' :: (Ord a, MonadState (S a) m, MonadCont m)
         => AdjacencyMap a -> m (TopSort a)
topSort' g = callCC $ \cyclic -> do
  let vertices = map fst $ Map.toDescList $ adjacencyMap g
      adjacent = Set.toDescList . flip postSet g
      dfs z x = do
        enter z x
        forM_ (adjacent x) $ \y ->
          classify y >>= \case
            TreeEdge -> dfs (Just x) y
            BackEdge -> cyclic . Left . retrace x y =<< gets table
            _        -> return ()
        exit x
  forM_ vertices $ \v -> do
    new <- unexplored v
    when new $ dfs Root v
  Right <$> gets order

-- | Compute a topological sort of the vertices of a graph. Given a
--  DAG, the lexicographically least topological ordering is returned,
--  otherwise, a cycle is.
--
-- @
-- topSort (1 * 2 + 3 * 1)                    == Right [3,1,2]
-- topSort ('path' [1..5])                      == Right [1..5]
-- topSort (3 * (1 * 4 + 2 * 5))              == Right [3,1,2,4,5]
-- topSort (1 * 2 + 2 * 1)                    == Left (2 ':|' [1])
-- topSort ('path' [5,4..1] + 'edge' 2 4)         == Left (4 ':|' [3,2])
-- topSort ('circuit' [1..5])                   == Left (5 ':|' [1..4])
-- topSort ('circuit' [1..3] + 'circuit' [3,2,1]) == Left (3 ':|' [2])
-- topSort (1*2+2*1+3*4+4*3+5*1)              == Left (1 ':|' [2])
-- fmap ('flip' 'isTopSortOf' x) (topSort x)      /= Right False
-- 'isRight' . topSort                          == 'isAcyclic'
-- @
topSort :: Ord a => AdjacencyMap a -> Either (Cycle a) [a]
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
isAcyclic :: Ord a => AdjacencyMap a -> Bool
isAcyclic g = case topSort g of
  Right _ -> True
  Left  _ -> False

-- TODO: Benchmark and optimise.
-- | Compute the /condensation/ of a graph, where each vertex corresponds to a
-- /strongly-connected component/ of the original graph. Note that component
-- graphs are non-empty, and are therefore of type
-- "Algebra.Graph.NonEmpty.AdjacencyMap".
--
-- @
-- scc 'empty'               == 'empty'
-- scc ('vertex' x)          == 'vertex' (NonEmpty.'NonEmpty.vertex' x)
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
scc m = gmap (component Map.!) $ removeSelfLoops $ gmap (leader Map.!) m
  where
    Typed.GraphKL g decode _ = Typed.fromAdjacencyMap m
    sccs      = map toList (KL.scc g)
    leader    = Map.fromList [ (decode y, x)      | x:xs <- sccs, y <- x:xs ]
    component = Map.fromList [ (x, expand (x:xs)) | x:xs <- sccs ]
    expand xs = fromJust $ NonEmpty.toNonEmpty $ induce (`Set.member` s) m
      where
        s = Set.fromList (map decode xs)

-- Remove all self loops from a graph.
removeSelfLoops :: Ord a => AdjacencyMap a -> AdjacencyMap a
removeSelfLoops m = foldr (\x -> removeEdge x x) m (vertexList m)

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
