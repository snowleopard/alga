{-# LANGUAGE DeriveGeneric #-}
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
    AdjacencyMap (..), consistent
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq
import Data.Map.Strict (Map)
import Data.Monoid (getSum, Sum (..))
import Data.Set (Set, (\\))
import GHC.Generics

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Algebra.Graph.Label

-- | Edge-labelled graphs, where the type variable @e@ stands for edge labels.
-- For example, 'AdjacencyMap' @Bool@ @a@ is isomorphic to unlabelled graphs
-- defined in the top-level module "Algebra.Graph.AdjacencyMap", where @False@
-- and @True@ denote the lack of and the existence of an unlabelled edge,
-- respectively.
newtype AdjacencyMap e a = AM {
    -- | The /adjacency map/ of an edge-labelled graph: each vertex is
    -- associated with a map from its direct successors to the corresponding
    -- edge labels.
    adjacencyMap :: Map a (Map a e) } deriving (Eq, Generic, NFData)

instance (Ord a, Show a, Ord e, Show e) => Show (AdjacencyMap e a) where
    showsPrec p (AM m)
        | Set.null vs = showString "empty"
        | null es     = showParen (p > 10) $ vshow vs
        | vs == used  = showParen (p > 10) $ eshow es
        | otherwise   = showParen (p > 10) $
                            showString "overlay (" . vshow (vs \\ used) .
                            showString ") ("       . eshow es . showString ")"
      where
        vs   = Map.keysSet m
        es   = internalEdgeList m
        used = referredToVertexSet m
        vshow vs = case Set.toAscList vs of
            [x] -> showString "vertex "   . showsPrec 11 x
            xs  -> showString "vertices " . showsPrec 11 xs
        eshow es = case es of
            [(e, x, y)] -> showString "edge "  . showsPrec 11 e .
                           showString " "      . showsPrec 11 x .
                           showString " "      . showsPrec 11 y
            xs          -> showString "edges " . showsPrec 11 xs

instance (Ord e, Monoid e, Ord a) => Ord (AdjacencyMap e a) where
    compare (AM x) (AM y) = mconcat
        [ compare (vNum x) (vNum y)
        , compare (vSet x) (vSet y)
        , compare (eNum x) (eNum y)
        , compare (eSet x) (eSet y)
        , cmp ]
      where
        vNum   = Map.size
        vSet   = Map.keysSet
        eNum   = getSum . foldMap (Sum . Map.size)
        eSet m = [ (x, y) | (x, ys) <- Map.toAscList m, (y, _) <- Map.toAscList ys ]
        cmp | x == y               = EQ
            | overlays [x, y] == y = LT
            | otherwise            = compare x y

-- Overlay a list of adjacency maps.
overlays :: (Eq e, Monoid e, Ord a) => [Map a (Map a e)] -> Map a (Map a e)
overlays = Map.unionsWith (\x -> Map.filter (/= zero) . Map.unionWith mappend x)

-- | __Note:__ this does not satisfy the usual ring laws; see 'AdjacencyMap'
-- for more details.
instance (Eq e, Dioid e, Num a, Ord a) => Num (AdjacencyMap e a) where
    fromInteger x = AM $ Map.singleton (fromInteger x) Map.empty
    AM x + AM y   = AM $ overlays [x, y]
    AM x * AM y   = AM $ overlays $ x : y :
        [ Map.fromSet (const targets) (Map.keysSet x) ]
      where
        targets = Map.fromSet (const one) (Map.keysSet y)
    signum      = const (AM Map.empty)
    abs         = id
    negate      = id

-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices, and there are no 'zero'-labelled edges. It
-- should be impossible to create an inconsistent adjacency map, and we use this
-- function in testing.
-- /Note: this function is for internal use only/.
consistent :: (Ord a, Eq e, Monoid e) => AdjacencyMap e a -> Bool
consistent (AM m) = referredToVertexSet m `Set.isSubsetOf` Map.keysSet m
    && and [ e /= zero | (_, es) <- Map.toAscList m, (_, e) <- Map.toAscList es ]

-- The set of vertices that are referred to by the edges in an adjacency map
referredToVertexSet :: Ord a => Map a (Map a e) -> Set a
referredToVertexSet m = Set.fromList $ concat
    [ [x, y] | (x, ys) <- Map.toAscList m, (y, _) <- Map.toAscList ys ]

-- The list of edges in an adjacency map
internalEdgeList :: Map a (Map a e) -> [(e, a, a)]
internalEdgeList m =
    [ (e, x, y) | (x, ys) <- Map.toAscList m, (y, e) <- Map.toAscList ys ]
