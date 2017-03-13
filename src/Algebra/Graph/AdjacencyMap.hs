-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of adjacency maps, associated operations and
-- algorithms. For adjacency maps specialised to graphs with @Int@ vertices
-- see "Algebra.Graph.IntAdjacencyMap".
--
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap (
    -- * Data structure
    AdjacencyMap, adjacencyMap,

    -- * Properties
    isEmpty, hasVertex, hasEdge, toSet,

    -- * Operations
    gmap, edgeList, edges, adjacencyList, fromAdjacencyList, postset,

    -- * Algorithms
    dfsForest, topSort, isTopSort, scc,

    -- * Interoperability with King-Launchbury graphs
    GraphKL, getGraph, getVertex, graphKL, fromGraphKL
  ) where

import Data.Array
import Data.Foldable (toList)
import Data.Set (Set)
import Data.Tree

import Algebra.Graph.AdjacencyMap.Internal

import qualified Data.Graph      as KL
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- | Check if a graph is empty.
--
-- @
-- isEmpty 'Algebra.Graph.empty'      == True
-- isEmpty ('Algebra.Graph.vertex' x) == False
-- @
isEmpty :: AdjacencyMap a -> Bool
isEmpty = Map.null . adjacencyMap

-- | Check if a graph contains a given vertex.
--
-- @
-- hasVertex x 'Algebra.Graph.empty'      == False
-- hasVertex x ('Algebra.Graph.vertex' x) == True
-- @
hasVertex :: Ord a => a -> AdjacencyMap a -> Bool
hasVertex v = Map.member v . adjacencyMap

-- | Check if a graph contains a given edge.
--
-- @
-- hasEdge x y 'Algebra.Graph.empty'      == False
-- hasEdge x y ('Algebra.Graph.vertex' z) == False
-- hasEdge x y ('Algebra.Graph.edge' x y) == True
-- @
hasEdge :: Ord a => a -> a -> AdjacencyMap a -> Bool
hasEdge u v a = case Map.lookup u (adjacencyMap a) of
    Nothing -> False
    Just vs -> Set.member v vs

-- | The set of vertices of a given graph.
--
-- @
-- toSet 'Algebra.Graph.empty'         == Set.empty
-- toSet ('Algebra.Graph.vertex' x)    == Set.singleton x
-- toSet ('Algebra.Graph.vertices' xs) == Set.fromList xs
-- toSet ('Algebra.Graph.clique' xs)   == Set.fromList xs
-- @
toSet :: Ord a => AdjacencyMap a -> Set a
toSet = Map.keysSet . adjacencyMap

-- | The /postset/ of a vertex @x@ is the set of its /direct successors/.
--
-- @
-- postset x 'Algebra.Graph.empty'      == Set.empty
-- postset x ('Algebra.Graph.vertex' x) == Set.empty
-- postset x ('Algebra.Graph.edge' x y) == Set.fromList [y]
-- postset y ('Algebra.Graph.edge' x y) == Set.empty
-- @
postset :: Ord a => a -> AdjacencyMap a -> Set a
postset v = Map.findWithDefault Set.empty v . adjacencyMap

-- | A data type encapsulating King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library.
data GraphKL a = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    getGraph :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @a@.
    getVertex :: KL.Vertex -> a }

-- | Build 'GraphKL' from the adjacency map of a graph.
graphKL :: Ord a => AdjacencyMap a -> GraphKL a
graphKL m = GraphKL g $ \u -> case r u of (_, v, _) -> v
  where
    (g, r) = KL.graphFromEdges' [ ((), v, us) | (v, us) <- adjacencyList m ]

-- | Extract the adjacency map of a King-Launchbury graph.
fromGraphKL :: Ord a => GraphKL a -> AdjacencyMap a
fromGraphKL (GraphKL g r) = fromAdjacencyList $ map (\(x, ys) -> (r x, map r ys)) (assocs g)

-- | Compute the /depth-first search/ forest of a graph.
--
-- @
-- dfsForest $ 3 * (1 + 4) * (1 + 5)     == [ Node { rootLabel = 1
--                                                 , subForest = [ Node { rootLabel = 5
--                                                                      , subForest = [] }]}
--                                          , Node { rootLabel = 3
--                                                 , subForest = [ Node { rootLabel = 4
--                                                                      , subForest = [] }]}]
-- 'Algebra.Graph.forest' (dfsForest $ 'Algebra.Graph.edge' 1 1)         == 'Algebra.Graph.vertex' 1
-- 'Algebra.Graph.forest' (dfsForest $ 'Algebra.Graph.edge' 1 2)         == 'Algebra.Graph.edge' 1 2
-- 'Algebra.Graph.forest' (dfsForest $ 'Algebra.Graph.edge' 2 1)         == 'Algebra.Graph.vertices' [1, 2]
-- 'Algebra.Graph.isSubgraphOf' ('Algebra.Graph.forest' $ dfsForest x) x == True
-- dfsForest . 'Algebra.Graph.forest' . dfsForest        == dfsForest
-- @
dfsForest :: Ord a => AdjacencyMap a -> Forest a
dfsForest m = let GraphKL g r = graphKL m in fmap (fmap r) (KL.dff g)

-- | Compute the /topological sort/ of a graph or return @Nothing@ if the graph
-- is cyclic.
--
-- @
-- topSort (1 * 2 + 3 * 1)             == Just [3,1,2]
-- topSort (1 * 2 + 2 * 1)             == Nothing
-- fmap (flip 'isTopSort' x) (topSort x) /= Just False
-- @
topSort :: Ord a => AdjacencyMap a -> Maybe [a]
topSort m = if isTopSort result m then Just result else Nothing
  where
    GraphKL g r = graphKL m
    result      = map r (KL.topSort g)

-- | Check if a given list of vertices is a valid /topological sort/ of a graph.
--
-- @
-- isTopSort [3, 1, 2] (1 * 2 + 3 * 1) == True
-- isTopSort [1, 2, 3] (1 * 2 + 3 * 1) == False
-- isTopSort []        (1 * 2 + 3 * 1) == False
-- isTopSort []        'Algebra.Graph.empty'           == True
-- isTopSort [x]       ('Algebra.Graph.vertex' x)      == True
-- isTopSort [x]       ('Algebra.Graph.edge' x x)      == False
-- @
isTopSort :: Ord a => [a] -> AdjacencyMap a -> Bool
isTopSort xs m = go Set.empty xs
  where
    go seen []     = seen == Map.keysSet (adjacencyMap m)
    go seen (v:vs) = let newSeen = seen `seq` Set.insert v seen
        in postset v m `Set.intersection` newSeen == Set.empty && go newSeen vs

-- | Compute the /condensation/ of a graph, where each vertex corresponds to a
-- /strongly-connected component/ of the original graph.
--
-- @
-- scc 'Algebra.Graph.empty'               == 'Algebra.Graph.empty'
-- scc ('Algebra.Graph.vertex' x)          == 'Algebra.Graph.vertex' (Set.singleton x)
-- scc ('Algebra.Graph.edge' x y)          == 'Algebra.Graph.edge' (Set.singleton x) (Set.singleton y)
-- scc ('Algebra.Graph.circuit' xs)        == 'Algebra.Graph.edge' (Set.fromList xs) (Set.fromList xs)
-- scc (3 * 1 * 4 * 1 * 5) == 'Algebra.Graph.edges' [ (Set.fromList [1,4], Set.fromList [1,4])
--                                  , (Set.fromList [1,4], Set.fromList [5]  )
--                                  , (Set.fromList [3]  , Set.fromList [1,4])
--                                  , (Set.fromList [3]  , Set.fromList [5]  )]
-- @
scc :: Ord a => AdjacencyMap a -> AdjacencyMap (Set a)
scc m = gmap (\v -> Map.findWithDefault Set.empty v components) m
  where
    GraphKL g r = graphKL m
    components  = Map.fromList $ concatMap (expand . fmap r . toList) (KL.scc g)
    expand xs   = let s = Set.fromList xs in map (\x -> (x, s)) xs
