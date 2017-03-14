-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.IntAdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of adjacency maps specialised to @Int@
-- vertices. The API is unstable and unsafe. Where possible use non-internal
-- module "Algebra.Graph.IntAdjacencyMap" instead. For the parametric version of
-- adjacency maps see "Algebra.Graph.AdjacencyMap".
--
-----------------------------------------------------------------------------
module Algebra.Graph.IntAdjacencyMap.Internal (
    -- * Adjacency map
    IntAdjacencyMap (..), consistent,

    -- * Operations on adjacency maps
    gmap, edgeList, edges, adjacencyList, fromAdjacencyList
  ) where

import Data.IntMap.Strict (IntMap, keysSet, fromSet)
import Data.IntSet (IntSet)

import Algebra.Graph.Classes

import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet        as Set

-- | The 'IntAdjacencyMap' data type represents a graph by a map of @Int@
-- vertices to their adjacency sets.
-- The 'Show' instance is defined using basic graph construction primitives:
--
-- @
-- show ('empty'     :: IntAdjacencyMap) == "empty"
-- show (1         :: IntAdjacencyMap) == "vertex 1"
-- show (1 + 2     :: IntAdjacencyMap) == "vertices [1,2]"
-- show (1 * 2     :: IntAdjacencyMap) == "edge 1 2"
-- show (1 * 2 * 3 :: IntAdjacencyMap) == "edges [(1,2),(1,3),(2,3)]"
-- show (1 * 2 + 3 :: IntAdjacencyMap) == "graph [1,2,3] [(1,2)]"
-- @
newtype IntAdjacencyMap = IntAdjacencyMap {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: IntMap IntSet
  } deriving Eq

instance Show IntAdjacencyMap where
    show a@(IntAdjacencyMap m)
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

instance Graph IntAdjacencyMap where
    type Vertex IntAdjacencyMap = Int
    empty       = IntAdjacencyMap $ Map.empty
    vertex  x   = IntAdjacencyMap $ Map.singleton x Set.empty
    overlay x y = IntAdjacencyMap $ Map.unionWith Set.union (adjacencyMap x) (adjacencyMap y)
    connect x y = IntAdjacencyMap $ Map.unionsWith Set.union [ adjacencyMap x, adjacencyMap y,
        fromSet (const . keysSet $ adjacencyMap y) (keysSet $ adjacencyMap x) ]

instance Num IntAdjacencyMap where
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
consistent :: IntAdjacencyMap -> Bool
consistent m = Set.fromList (uncurry (++) $ unzip $ edgeList m)
    `Set.isSubsetOf` keysSet (adjacencyMap m)

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with monomorphic
-- 'IntAdjacencyMap'.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Int -> Int) -> IntAdjacencyMap -> IntAdjacencyMap
gmap f = IntAdjacencyMap . Map.map (Set.map f) . Map.mapKeysWith Set.union f . adjacencyMap

-- | Construct a graph from an /adjacency list/.
--
-- @
-- fromAdjacencyList []                                  == 'empty'
-- fromAdjacencyList [(x, [])]                           == 'vertex' x
-- fromAdjacencyList [(x, [y])]                          == 'Algebra.Graph.edge' x y
-- fromAdjacencyList . 'adjacencyList'                     == id
-- 'overlay' (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)
-- @
fromAdjacencyList :: [(Int, [Int])] -> IntAdjacencyMap
fromAdjacencyList as = IntAdjacencyMap $ Map.unionWith Set.union vs es
  where
    vs = fromSet (const Set.empty) . Set.fromList $ concatMap (\(x, ys) -> x : ys) as
    es = Map.fromListWith Set.union $ map (\(x, ys) -> (x, Set.fromList ys)) as

-- | Construct a graph from an /edge list/.
--
-- @
-- edges []         == 'empty'
-- edges [(x, y)]   == 'Algebra.Graph.edge' x y
-- 'edgeList' . edges == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: [(Int, Int)] -> IntAdjacencyMap
edges = IntAdjacencyMap . Map.fromListWith Set.union . map (\(x, y) -> (x, Set.singleton y))

-- | Extract the /adjacency list/ of a graph.
--
-- @
-- adjacencyList 'empty'               == []
-- adjacencyList ('vertex' x)          == [(x, [])]
-- adjacencyList ('Algebra.Graph.edge' x y)          == [(x, [y])]
-- adjacencyList ('Algebra.Graph.star' 2 [1,3])      == [(1, []), (2, [1,3]), (3, [])]
-- 'fromAdjacencyList' . adjacencyList == id
-- @
adjacencyList :: IntAdjacencyMap -> [(Int, [Int])]
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
edgeList :: IntAdjacencyMap -> [(Int, Int)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList
