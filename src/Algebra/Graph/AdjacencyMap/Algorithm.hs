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
    dfsForest, dfsForestFrom, dfs, bfsForest, bfsForestFrom, bfs, reachable, topSort, isAcyclic, scc,

    -- * Correctness properties
    isDfsForestOf, isTopSortOf
    ) where

import Control.Monad
import Data.Foldable (toList)
import Data.Maybe
import Data.Tree

import Algebra.Graph.AdjacencyMap

import qualified Algebra.Graph.AdjacencyMap.Internal as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Graph                          as KL
import qualified Data.Graph.Typed                    as Typed
import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set
import qualified Data.Sequence                       as Seq

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
-- @
dfsForestFrom :: Ord a => [a] -> AdjacencyMap a -> Forest a
dfsForestFrom vs = Typed.dfsForestFrom vs . Typed.fromAdjacencyMap

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
-- @
dfs :: Ord a => [a] -> AdjacencyMap a -> [a]
dfs vs = concatMap flatten . dfsForestFrom vs

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
reachable :: Ord a => a -> AdjacencyMap a -> [a]
reachable x = dfs [x]

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)               == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)               == Nothing
-- fmap ('flip' 'isTopSortOf' x) (topSort x) /= Just False
-- 'isJust' . topSort                      == 'isAcyclic'
-- @
topSort :: Ord a => AdjacencyMap a -> Maybe [a]
topSort m = if isTopSortOf result m then Just result else Nothing
  where
    result = Typed.topSort (Typed.fromAdjacencyMap m)

-- | Check if a given graph is /acyclic/.
--
-- @
-- isAcyclic (1 * 2 + 3 * 1) == True
-- isAcyclic (1 * 2 + 2 * 1) == False
-- isAcyclic . 'circuit'       == 'null'
-- isAcyclic                 == 'isJust' . 'topSort'
-- @
isAcyclic :: Ord a => AdjacencyMap a -> Bool
isAcyclic = isJust . topSort

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
removeSelfLoops (AM.AM m) = AM.AM (Map.mapWithKey Set.delete m)

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

-- | Compute the /breadth-first search/ forest of a graph that corresponds to
-- searching from each of the graph vertices in the 'Ord' @a@ order.
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
-- @
-- bfsForest 'empty'                       == []
-- 'forest' (bfsForest $ 'edge' 1 1)         == 'vertex' 1
-- 'forest' (bfsForest $ 'edge' 1 2)         == 'edge' 1 2
-- 'forest' (bfsForest $ 'edge' 2 1)         == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForest x) x == True
-- 'isbfsForestOf' (bfsForest x) x         == True
-- bfsForest . 'forest' . bfsForest        == bfsForest
-- bfsForest ('vertices' vs)               == 'map' (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
-- bfsForest $ 1 * (3+5+7) + 3 * (5+4) + (4+3+5+7) * 6 ==  [Node {rootLabel = 1
--                                                               , subForest = [Node {rootLabel = 3
--                                                                                   , subForest = [ Node {rootLabel = 4
--                                                                                                        , subForest = [] }
--                                                                                                 , Node {rootLabel = 6
--                                                                                                        , subForest = [] }]}
--                                                                             , Node {rootLabel = 5
--                                                                                    , subForest = [] }
--                                                                             , Node {rootLabel = 7
--                                                                                    , subForest = [] }]}]
-- @
bfsForest :: Ord a => AdjacencyMap a -> Forest a
bfsForest g = bfsForestFrom (vertexList g) g


-- | Compute the /breadth-first search/ AdjacencyMap of a graph that corresponds to
-- searching from a single vertex of the graph. 
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
bfsTreeAdjacencyMap :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeAdjacencyMap2 s g = case (hasVertex s g) of
    True -> bfsTreeAdjacencyMapUtil2 (Seq.singleton s) initVisited g 
        where initVisited = Map.unionsWith (||) $ ( Map.singleton s True):(map (\x -> Map.singleton x False) (vertexList g))
    _ -> empty

-- | Compute the /breadth-first search/ AdjacencyMap of a graph that corresponds to
-- searching from the head of a queue (followed by other vertices to search from), 
-- given a Set of seen vertices (vertices that shouldn't be visited).
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
bfsTreeAdjacencyMapUtil :: Ord a => Seq.Seq a -> Map.Map a Bool -> AdjacencyMap a -> AdjacencyMap a
bfsTreeAdjacencyMapUtil2 queue visited g
    | queue == Seq.empty = empty
    | otherwise = overlay (AM.AM $ Map.singleton v vSet) (bfsTreeAdjacencyMapUtil2 newQueue newVisited g)
        where
            v Seq.:< qv = Seq.viewl queue
            neighbors = postSet v g
            (newQueue, newVisited, vSet) = bfsTreeNewParams neighbors visited qv


-- | Compute the /breadth-first search/ intermediate values for `bfsTreeAdjacencyMapUtil`. Given a set of neighbors
-- (source doesnt matter), a map of visisted nodes (Map a Bool) and a queue (Sequence), obtain the new queue, update
-- map and set of vertices to add to the graph.
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
bfsTreeNewParams :: (Ord a) => Set.Set a -> Map.Map a Bool -> Seq.Seq a -> (Seq.Seq a, Map.Map a Bool, Set.Set a)
bfsTreeNewParams neighbors visited queue = (newQueue, newVisited, vSet )
            where vSet = Set.filter (\x -> (not . fromJust . Map.lookup x) visited) neighbors
                  vList = Set.toAscList vSet
                  newQueue = foldl (Seq.|>) queue vList
                  newVisited = Map.unionsWith (||) $ visited : (map (\x -> Map.singleton x True) vList)

-- | Compute the /breadth-first search/ Tree of a graph that corresponds to
-- searching from a single vertex of the graph. This is just for internal use. 
-- Might move it to `*.Internal` then?
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
bfsTree :: Ord a => a -> AdjacencyMap a -> Tree a
bfsTree s g = unfoldTree neighbors s
    where neighbors b = (b, Set.toAscList . postSet b $ bfsAM)
          bfsAM = bfsTreeAdjacencyMap s g

-- | Compute the /breadth-first search/ forest of a graph, searching from each of
-- the given vertices in order. Note that the resulting forest does not
-- necessarily span the whole graph, as some vertices may be unreachable.
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
-- bfsForestFrom vs 'empty'                                      == []
-- 'forest' (bfsForestFrom [1]   $ 'edge' 1 1)                   == 'vertex' 1
-- 'forest' (bfsForestFrom [1]   $ 'edge' 1 2)                   == 'edge' 1 2
-- 'forest' (bfsForestFrom [2]   $ 'edge' 1 2)                   == 'vertex' 2
-- 'forest' (bfsForestFrom [3]   $ 'edge' 1 2)                   == 'empty'
-- 'forest' (bfsForestFrom [2,1] $ 'edge' 1 2)                   == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForestFrom vs x) x              == True
-- bfsForestFrom ('vertexList' x) x                              == 'bfsForest' x
-- bfsForestFrom vs             ('vertices' vs)                  == 'map' (\\v -> Node v []) ('Data.List.nub' vs)
-- bfsForestFrom []             x                                == []
-- bfsForestFrom [1,4] $ 1 * (3+5+7) + 3 * (5+4) + (4+3+5+7) * 6 ==  [ Node { rootLabel = 3
--                                                                          , subForest = [ Node { rootLabel = 4
--                                                                                                 , subForest = []}
--                                                                                        , Node { rootLabel = 5
--                                                                                               , subForest = []}
--                                                                                        , Node { rootLabel = 6
--                                                                                               , subForest = [] }]}
--                                                                   , Node { rootLabel = 1
--                                                                          , subForest = [ Node { rootLabel = 7
--                                                                                               , subForest = [] }]}]
-- @
bfsForestFrom :: Ord a => [a] -> AdjacencyMap a -> Forest a
bfsForestFrom [] _ = []
bfsForestFrom (v:vs) g
    | hasVertex v g = headTree:bfsForestFrom vs (induce remove g)
    | otherwise = bfsForestFrom vs g
        where headTree = bfsTree v g
              removedVertices = flatten headTree
              remove x = not $ elem x removedVertices

-- -- | Compute the list of vertices visited by the /breadth-first search/ by level in a
-- graph, when searching from each of the given vertices in order.
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
-- @
-- bfs vs    $ 'empty'                    == []
-- bfs [1]   $ 'edge' 1 1                 == [[1]]
-- bfs [1]   $ 'edge' 1 2                 == [[1],[2]]
-- bfs [2]   $ 'edge' 1 2                 == [[2]]
-- bfs [3]   $ 'edge' 1 2                 == []
-- bfs [1,2] $ 'edge' 1 2                 == [[1],[2]]
-- bfs [2,1] $ 'edge' 1 2                 == [[2,1]]
-- bfs []    $ x                        == []
-- bfs [1,4] $ 3 * (1 + 4) * (1 + 5)    == [[1,4],[5]]
-- @
bfs :: Ord a => [a] -> AdjacencyMap a -> [[a]]
bfs vs g = foldr (zipWith (++)) acc (map (++ repeat []) l)
    where l = bfsPerTree vs g 
          maxLength = case l of
            [] -> 0
            _ -> maximum (map length l)
          acc = [ [] | _<-[1..maxLength]]


-- -- | Compute the list of vertices visited by the /breadth-first search/ in a graph.
-- For every tree in the forest, a different list of vertices by level is given.
-- Complexity: /O((n + m) * log(n))/ time and O(n+m) memory.
-- @
-- bfsPerTree vs    $ 'empty'                    == []
-- bfsPerTree [1]   $ 'edge' 1 1                 == [[[1]]]
-- bfsPerTree [1]   $ 'edge' 1 2                 == [[[1],[2]]]
-- bfsPerTree [2]   $ 'edge' 1 2                 == [[[2]]]
-- bfsPerTree [3]   $ 'edge' 1 2                 == []
-- bfsPerTree [1,2] $ 'edge' 1 2                 == [[[1],[2]]]
-- bfsPerTree [2,1] $ 'edge' 1 2                 == [[[2]],[[1]]]
-- bfsPerTree []    $ x                        == []
-- bfsPerTree [1,4] $ 3 * (1 + 4) * (1 + 5)    == [[[1],[5]],[[4]]]
-- @
bfsPerTree :: Ord a => [a] -> AdjacencyMap a -> [[[a]]]
bfsPerTree vs = (map levels . bfsForestFrom vs)