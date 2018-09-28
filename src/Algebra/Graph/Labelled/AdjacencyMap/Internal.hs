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
    AdjacencyMap(..), empty, vertex, overlay, connect,
    (-<), (>-), fromAdjacencySets, consistent, edgeLabel,
  ) where

import Control.DeepSeq
import Data.List
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Algebra.Graph.Label

newtype AdjacencyMap a e = LAM {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    adjacencyMap :: Map a (Map a e) } deriving Eq

-- TODO: Show labels
instance (Ord a, Show a, Ord e, Show e, Dioid e) => Show (AdjacencyMap a e) where
    show (LAM m)
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

empty :: AdjacencyMap a e
empty = LAM Map.empty

vertex :: a -> AdjacencyMap a e
vertex x = LAM $ Map.singleton x Map.empty

overlay :: (Ord a, Semilattice e) => AdjacencyMap a e -> AdjacencyMap a e -> AdjacencyMap a e
overlay (LAM x) (LAM y) = LAM $ Map.unionWith (Map.unionWith (\/)) x y

connect :: (Ord a, Dioid e) => AdjacencyMap a e -> AdjacencyMap a e -> AdjacencyMap a e
connect = lconnect one

lconnect :: (Ord a, Dioid e) => e -> AdjacencyMap a e -> AdjacencyMap a e -> AdjacencyMap a e
lconnect e (LAM x) (LAM y) = LAM $ Map.unionsWith (Map.unionWith (\/))
    [ x, y, Map.fromSet (const cset) (Map.keysSet x) ]
  where
    cset = Map.fromSet (const e) (Map.keysSet y)

(-<) :: AdjacencyMap a e -> e -> (AdjacencyMap a e, e)
g -< e = (g, e)

(>-) :: (Ord a, Dioid e) => (AdjacencyMap a e, e) -> AdjacencyMap a e -> AdjacencyMap a e
(g, e) >- h = lconnect e g h

infixl 5 -<
infixl 5 >-

edgeLabel :: (Ord a, Dioid e) => a -> a -> AdjacencyMap a e -> e
edgeLabel x y (LAM m) = fromMaybe zero (Map.lookup x m >>= Map.lookup y)

instance (Ord a, Num a, Dioid e) => Num (AdjacencyMap a e) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance (NFData a, NFData e) => NFData (AdjacencyMap a e) where
    rnf (LAM a) = rnf a

-- | Construct a graph from a list of adjacency sets.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencySets []                                        == 'Algebra.Graph.Labelled.AdjdacencyMap.empty'
-- fromAdjacencySets [(x, Set.'Set.empty')]                          == 'Algebra.Graph.Labelled.AdjdacencyMap.vertex' x
-- fromAdjacencySets [(x, Set.'Set.singleton' y)]                    == 'Algebra.Graph.Labelled.AdjdacencyMap.edge' x y
-- fromAdjacencySets . map (fmap Set.'Set.fromList') . 'Algebra.Graph.Labelled.AdjdacencyMap.adjacencyList' == id
-- 'Algebra.Graph.Labelled.AdjdacencyMap.overlay' (fromAdjacencySets xs) (fromAdjacencySets ys)       == fromAdjacencySets (xs ++ ys)
-- @
fromAdjacencySets :: (Ord a, Dioid e) => [(a, Set a)] -> AdjacencyMap a e
fromAdjacencySets ss = LAM $ Map.unionWith (Map.unionWith (\/)) vs es
    where
        vs = Map.fromSet (const Map.empty) . Set.unions $ map snd ss
        es = Map.fromListWith (Map.unionWith (\/))
                              (fmap (\(a, s) -> (a, set2map s)) ss)
        set2map = Map.fromSet (const one)


-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
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
consistent :: (Ord a) => AdjacencyMap a e -> Bool
consistent (LAM m) = referredToVertexSet m `Set.isSubsetOf` Map.keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: (Ord a) => Map a (Map a e) -> Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: Map a (Map a e) -> [(a, a)]
internalEdgeList m = do
        (x, ys) <- Map.toAscList m
        (y, _ ) <- Map.toAscList ys
        return (x, y)
