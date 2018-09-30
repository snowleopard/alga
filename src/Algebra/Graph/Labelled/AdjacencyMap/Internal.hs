-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.AdjdacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of edge-labelled adjacency maps. The API is unstable
-- and unsafe, and is exposed only for documentation. You should use the
-- non-internal module "Algebra.Graph.Labelled.AdjdacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled.AdjacencyMap.Internal (
    -- * Adjacency map implementation
    AdjacencyMap(..), empty, vertex, overlay, connect, connectBy,
    fromAdjacencyMaps, consistent, edgeLabel
    ) where

import Control.DeepSeq
import Data.List
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Algebra.Graph.Label

newtype AdjacencyMap e a = AM {
    -- | The /adjacency map/ of an edge-labelled graph: each vertex is
    -- associated with a map from its direct successors to the corresponding
    -- edge labels.
    adjacencyMap :: Map a (Map a e) } deriving (Eq, NFData)

-- TODO: Show labels
instance (Ord a, Show a, Ord e, Show e, Dioid e) => Show (AdjacencyMap e a) where
    show (AM m)
        | null vs    = "empty"
        | null es    = vshow vs
        | vs == used = eshow es
        | otherwise  = "overlay (" ++ vshow (vs \\ used) ++ ") (" ++ eshow es ++ ")"
      where
        vs             = Set.toAscList (Map.keysSet m)
        es             = internalEdgeList m
        vshow [x]      = "vertex "   ++ show x
        vshow xs       = "vertices " ++ show xs
        eshow [(x, y)] = "edge "     ++ show x ++ " " ++ show y
        eshow xs       = "edges "    ++ show xs
        used           = Set.toAscList (referredToVertexSet m)

empty :: AdjacencyMap e a
empty = AM Map.empty

vertex :: a -> AdjacencyMap e a
vertex x = AM $ Map.singleton x Map.empty

overlay :: (Ord a, Semilattice e) => AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
overlay (AM x) (AM y) = AM $ Map.unionWith (Map.unionWith (\/)) x y

connect :: (Ord a, Dioid e) => AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
connect = connectBy one

connectBy :: (Ord a, Dioid e) => e -> AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
connectBy e (AM x) (AM y) = AM $ Map.unionsWith (Map.unionWith (\/))
    [ x, y, Map.fromSet (const targets) (Map.keysSet x) ]
  where
    targets = Map.fromSet (const e) (Map.keysSet y)

edgeLabel :: (Ord a, Dioid e) => a -> a -> AdjacencyMap e a -> e
edgeLabel x y (AM m) = fromMaybe zero (Map.lookup x m >>= Map.lookup y)

instance (Ord a, Num a, Dioid e) => Num (AdjacencyMap e a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- | Construct a graph from a list of adjacency sets.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencyMaps []                                        == 'Algebra.Graph.Labelled.AdjdacencyMap.empty'
-- fromAdjacencyMaps [(x, Set.'Set.empty')]                          == 'Algebra.Graph.Labelled.AdjdacencyMap.vertex' x
-- fromAdjacencyMaps [(x, Set.'Set.singleton' y)]                    == 'Algebra.Graph.Labelled.AdjdacencyMap.edge' x y
-- fromAdjacencyMaps . map (fmap Set.'Set.fromList') . 'Algebra.Graph.Labelled.AdjdacencyMap.adjacencyList' == id
-- 'Algebra.Graph.Labelled.AdjdacencyMap.overlay' (fromAdjacencyMaps xs) (fromAdjacencyMaps ys)       == fromAdjacencySets (xs ++ ys)
-- @
fromAdjacencyMaps :: (Ord a, Eq e, Semilattice e) => [(a, Map a e)] -> AdjacencyMap e a
fromAdjacencyMaps ss = AM $ Map.unionWith (Map.unionWith (\/)) vs es
  where
    vs = Map.fromSet (const Map.empty) . Set.unions $ map (Map.keysSet . snd) ss
    es = Map.fromListWith (Map.unionWith (\/)) $ map (fmap $ Map.filter (/= zero)) ss

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices, and there are no 'zero'-labelled edges. It
-- should be impossible to create an inconsistent adjacency map, and we use this
-- function in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent 'Algebra.Graph.Labelled.AdjdacencyMap.empty'                  == True
-- consistent ('Algebra.Graph.Labelled.AdjdacencyMap.vertex' x)             == True
-- consistent ('Algebra.Graph.Labelled.AdjdacencyMap.overlay' x y)          == True
-- consistent ('Algebra.Graph.Labelled.AdjdacencyMap.connect' x y)          == True
-- consistent ('Algebra.Graph.Labelled.AdjdacencyMap.edge' x y)             == True
-- consistent ('Algebra.Graph.Labelled.AdjdacencyMap.edges' xs)             == True
-- consistent ('Algebra.Graph.Labelled.AdjdacencyMap.graph' xs ys)          == True
-- consistent ('Algebra.Graph.Labelled.AdjdacencyMap.fromAdjacencyList' xs) == True
-- @
consistent :: (Ord a, Eq e, Semilattice e) => AdjacencyMap e a -> Bool
consistent (AM m) = referredToVertexSet m `Set.isSubsetOf` Map.keysSet m
    && and [ e /= zero | (_, es) <- Map.toAscList m, (_, e) <- Map.toAscList es ]

-- The set of vertices that are referred to by the edges
referredToVertexSet :: Ord a => Map a (Map a e) -> Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: Map a (Map a e) -> [(a, a)]
internalEdgeList m =
    [ (x, y) | (x, ys) <- Map.toAscList m, (y, _) <- Map.toAscList ys ]
