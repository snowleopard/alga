-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.AdjdacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of edge-labelled adjacency maps. The
-- API is unstable and unsafe, and is exposed only for documentation. You should
-- use the non-internal module "Algebra.Graph.Labelled.AdjdacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled.AdjacencyMap.Internal (
    -- * Labelled adjacency map implementation
    AdjacencyMap (..), empty, vertex, overlay, connect, fromAdjacencyMaps,
    consistent
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq
import Data.Map.Strict (Map)
import Data.Semigroup (Semigroup (..), (<>))
import Data.Set (Set, (\\))

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Algebra.Graph.Label

newtype AdjacencyMap e a = AM {
    -- | The /adjacency map/ of an edge-labelled graph: each vertex is
    -- associated with a map from its direct successors to the corresponding
    -- edge labels.
    adjacencyMap :: Map a (Map a e) } deriving (Eq, NFData)

instance (Ord a, Show a, Ord e, Show e) => Show (AdjacencyMap e a) where
    show (AM m)
        | Set.null vs = "empty"
        | null es     = vshow vs
        | vs == used  = eshow es
        | otherwise   = "overlay (" ++ vshow (vs \\ used) ++ ") (" ++ eshow es ++ ")"
      where
        vs   = Map.keysSet m
        es   = internalEdgeList m
        used = referredToVertexSet m
        vshow vs = case Set.toAscList vs of
            [x] -> "vertex "   ++ show x
            xs  -> "vertices " ++ show xs
        eshow es = case es of
            [(e, x, y)] -> "edge "  ++ show e ++ " " ++ show x ++ " " ++ show y
            xs          -> "edges " ++ show xs

-- | Construct the /empty graph/.
-- Complexity: /O(1)/ time and memory.
empty :: AdjacencyMap e a
empty = AM Map.empty

-- | Construct the graph comprising /a single isolated vertex/.
-- Complexity: /O(1)/ time and memory.
vertex :: a -> AdjacencyMap e a
vertex x = AM $ Map.singleton x Map.empty

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
overlay :: (Ord a, Semigroup e) => AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
overlay (AM x) (AM y) = AM $ Map.unionWith (Map.unionWith (<>)) x y

-- | /Connect/ two graphs with edges labelled by a given label. When applied to
-- the same labels, this is an associative operation with the identity 'empty',
-- which distributes over 'overlay' and obeys the decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the
-- number of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
connect :: (Ord a, Semigroup e) => e -> AdjacencyMap e a -> AdjacencyMap e a -> AdjacencyMap e a
connect e (AM x) (AM y) = AM $ Map.unionsWith (Map.unionWith (<>))
    [ x, y, Map.fromSet (const targets) (Map.keysSet x) ]
  where
    targets = Map.fromSet (const e) (Map.keysSet y)

instance (Ord a, Num a, Dioid e) => Num (AdjacencyMap e a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect one
    signum      = const empty
    abs         = id
    negate      = id

-- | Construct a graph from a list of adjacency sets.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
fromAdjacencyMaps :: (Ord a, Eq e, Monoid e) => [(a, Map a e)] -> AdjacencyMap e a
fromAdjacencyMaps ss = AM $ Map.unionWith (Map.unionWith (<>)) vs es
  where
    vs = Map.fromSet (const Map.empty) . Set.unions $ map (Map.keysSet . snd) ss
    es = Map.fromListWith (Map.unionWith (<>)) $ map (fmap $ Map.filter (/= mempty)) ss

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices, and there are no 'zero'-labelled edges. It
-- should be impossible to create an inconsistent adjacency map, and we use this
-- function in testing.
-- /Note: this function is for internal use only/.
consistent :: (Ord a, Eq e, Monoid e) => AdjacencyMap e a -> Bool
consistent (AM m) = referredToVertexSet m `Set.isSubsetOf` Map.keysSet m
    && and [ e /= mempty | (_, es) <- Map.toAscList m, (_, e) <- Map.toAscList es ]

-- The set of vertices that are referred to by the edges in an adjacency map
referredToVertexSet :: Ord a => Map a (Map a e) -> Set a
referredToVertexSet m = Set.fromList $ concat
    [ [x, y] | (x, ys) <- Map.toAscList m, (y, _) <- Map.toAscList ys ]

-- The list of edges in an adjacency map
internalEdgeList :: Map a (Map a e) -> [(e, a, a)]
internalEdgeList m =
    [ (e, x, y) | (x, ys) <- Map.toAscList m, (y, e) <- Map.toAscList ys ]
