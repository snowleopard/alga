-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.AdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps. The API is unstable
-- and unsafe. Where possible use non-internal module "Algebra.Graph.AdjacencyMap"
-- instead.
--
-----------------------------------------------------------------------------
module Algebra.Graph.AdjacencyMap.Internal (
    -- * Adjacency map
    AdjacencyMap (..), consistent,

    -- * Operations on adjacency maps
    gmap, edgeList, edges, adjacencyList, fromAdjacencyList
  ) where

import Data.Map.Strict (Map, keysSet, fromSet)
import Data.Set (Set)

import Algebra.Graph.Class

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- | The 'AdjacencyMap' data type represents a graph by a map of vertices
-- to their adjacency sets.
-- The 'Show' instance is defined using basic graph construction primitives:
--
-- @
-- show ('empty'     :: AdjacencyMap Int) == "empty"
-- show (1         :: AdjacencyMap Int) == "vertex 1"
-- show (1 + 2     :: AdjacencyMap Int) == "vertices [1,2]"
-- show (1 * 2     :: AdjacencyMap Int) == "edge 1 2"
-- show (1 * 2 * 3 :: AdjacencyMap Int) == "edges [(1,2),(1,3),(2,3)]"
-- show (1 * 2 + 3 :: AdjacencyMap Int) == "graph [1,2,3] [(1,2)]"
-- @
newtype AdjacencyMap a = AdjacencyMap {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: Map a (Set a)
  } deriving Eq

instance (Ord a, Show a) => Show (AdjacencyMap a) where
    show a@(AdjacencyMap m)
        | m == Map.empty = "empty"
        | es == []       = if Set.size vs > 1 then "vertices " ++ show (Set.toAscList vs)
                                              else "vertex "   ++ show v
        | vs == related  = if length es > 1 then "edges " ++ show es
                                            else "edge "  ++ show e ++ " " ++ show f
        | otherwise      = "graph " ++ show (Set.toAscList vs) ++ " " ++ show es
      where
        vs      = keysSet m
        es      = edgeList a
        v       = head $ Set.toList vs
        (e,f)   = head es
        related = Set.fromList . uncurry (++) $ unzip es

instance Ord a => Graph (AdjacencyMap a) where
    type Vertex (AdjacencyMap a) = a
    empty       = AdjacencyMap $ Map.empty
    vertex  x   = AdjacencyMap $ Map.singleton x Set.empty
    overlay x y = AdjacencyMap $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)
    connect x y = AdjacencyMap $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance (Ord a, Num a) => Num (AdjacencyMap a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
--
-- @
-- consistent 'empty'                  == True
-- consistent ('vertex' x)             == True
-- consistent ('overlay' x y)          == True
-- consistent ('connect' x y)          == True
-- consistent ('Algebra.Graph.edge' x y)             == True
-- consistent ('edges' xs)             == True
-- consistent ('Algebra.Graph.graph' xs ys)          == True
-- consistent ('fromAdjacencyList' xs) == True
-- @
consistent :: Ord a => AdjacencyMap a -> Bool
consistent m = Set.fromList (uncurry (++) $ unzip $ edgeList m)
    `Set.isSubsetOf` keysSet (adjacencyMap m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'AdjacencyMap'.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap f = AdjacencyMap . Map.map (Set.map f) . Map.mapKeysWith Set.union f . adjacencyMap

-- | Construct a graph from an /adjacency list/.
--
-- @
-- fromAdjacencyList []                                  == 'empty'
-- fromAdjacencyList [(x, [])]                           == 'vertex' x
-- fromAdjacencyList [(x, [y])]                          == 'Algebra.Graph.edge' x y
-- fromAdjacencyList . 'adjacencyList'                     == id
-- 'overlay' (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)
-- @
fromAdjacencyList :: Ord a => [(a, [a])] -> AdjacencyMap a
fromAdjacencyList as = AdjacencyMap $ Map.unionWith Set.union vs es
  where
    ss = map (fmap Set.fromList) as
    vs = fromSet (const Set.empty) . Set.unions $ map snd ss
    es = Map.fromListWith Set.union ss

-- | Construct a graph from an /edge list/.
--
-- @
-- edges []         == 'empty'
-- edges [(x, y)]   == 'Algebra.Graph.edge' x y
-- 'edgeList' . edges == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: Ord a => [(a, a)] -> AdjacencyMap a
edges = fromAdjacencyList . map (fmap return)

-- | Extract the /adjacency list/ of a graph.
--
-- @
-- adjacencyList 'empty'               == []
-- adjacencyList ('vertex' x)          == [(x, [])]
-- adjacencyList ('Algebra.Graph.edge' 1 2)          == [(1, [2]), (2, [])]
-- adjacencyList ('Algebra.Graph.star' 2 [1,3])      == [(1, []), (2, [1,3]), (3, [])]
-- 'fromAdjacencyList' . adjacencyList == id
-- @
adjacencyList :: AdjacencyMap a -> [(a, [a])]
adjacencyList = map (fmap Set.toAscList) . Map.toAscList . adjacencyMap

-- | Extract the /edge list/ of a graph.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('Algebra.Graph.edge' x y)     == [(x,y)]
-- edgeList ('Algebra.Graph.star' 2 [1,3]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList
