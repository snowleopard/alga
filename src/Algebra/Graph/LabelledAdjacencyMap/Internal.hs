-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.LabelledAdjacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of labelled adjacency maps. The API is unstable
-- and unsafe, and is exposed only for documentation. You should use the
-- non-internal module "Algebra.Graph.LabelledAdjacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.LabelledAdjacencyMap.Internal
        (
    -- * Adjacency map implementation
          LabelledAdjacencyMap(..)
        , consistent
        , edgeLabel
        , empty
        , connect
        , overlay
        , vertex
        , (-<)
        , (>-)
        , fromAdjacencySets
        )
where

import           Data.List
import           Data.Map.Strict                ( Map
                                                , keysSet
                                                , fromSet
                                                )
import           Data.Set                       ( Set )
import Data.Maybe (fromMaybe)
import           Algebra.Graph.Labelled         ( Dioid(..)
                                                , (|+|)
                                                , (|*|)
                                                )
import Control.DeepSeq (NFData (..))
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

newtype LabelledAdjacencyMap a e = LAM {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    labelledAdjacencyMap :: Map a (Map a e) } deriving Eq

lAdjacencyMap :: LabelledAdjacencyMap a e -> Map a (Map a e)
lAdjacencyMap = labelledAdjacencyMap



instance (Ord a, Show a, Ord e, Show e, Dioid e) => Show (LabelledAdjacencyMap a e) where
    show (LAM m)
        | null vs    = "empty"
        | null es    = vshow vs
        | vs == used = eshow es
        | otherwise  = "overlay (" ++ vshow (vs \\ used) ++ ") (" ++ eshow es ++ ")"
      where
        vs             = Set.toAscList (keysSet m)
        es             = internalEdgeList m
        vshow [x]      = "vertex "   ++ show x
        vshow xs       = "vertices " ++ show xs
        eshow [(x, y)] = "edge "     ++ show x ++ " " ++ show y
        eshow xs       = "edges "    ++ show xs
        used           = Set.toAscList (referredToVertexSet m)


empty :: LabelledAdjacencyMap a e
empty = LAM Map.empty

vertex :: a -> LabelledAdjacencyMap a e
vertex x = LAM $ Map.singleton x Map.empty

overlay ::
     (Ord a, Dioid e)
  => LabelledAdjacencyMap a e
  -> LabelledAdjacencyMap a e
  -> LabelledAdjacencyMap a e
overlay x y =
  LAM $
  Map.unionWith (Map.unionWith (|+|)) (lAdjacencyMap x) (lAdjacencyMap y)

connect  :: (Ord a, Dioid e)
        => LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
connect = lconnect one

lconnect
        :: (Ord a, Dioid e)
        => e
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
lconnect e x y =
  LAM $
  Map.unionsWith
    (Map.unionWith (|*|))
    [ lAdjacencyMap x
    , lAdjacencyMap y
    , Map.fromSet (const cset) (keysSet $ lAdjacencyMap x)
    ]
  where
    cset = fromSet (const e) (keysSet $ lAdjacencyMap y)
    
(-<)
        :: LabelledAdjacencyMap a e
        -> e
        -> (LabelledAdjacencyMap a e, e)
g -< e = (g, e)

(>-)
        :: (Ord a, Dioid e)
        => (LabelledAdjacencyMap a e, e)
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
(g, e) >- h = lconnect e g h

infixl 5 -<
infixl 5 >-

edgeLabel :: (Ord a, Dioid e) => a -> a -> LabelledAdjacencyMap a e -> e
edgeLabel x y g =
  fromMaybe zero (Map.lookup y =<< Map.lookup x (lAdjacencyMap g))

instance (Ord a, Num a, Dioid e) => Num (LabelledAdjacencyMap a e) where
  fromInteger = vertex . fromInteger
  (+) = overlay
  (*) = connect
  signum = const empty
  abs = id
  negate = id

instance (NFData a, NFData e) => NFData (LabelledAdjacencyMap a e) where
    rnf (LAM a) = rnf a

-- | Construct a graph from a list of adjacency sets.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- fromAdjacencySets []                                        == 'Algebra.Graph.LabelledAdjacencyMap.empty'
-- fromAdjacencySets [(x, Set.'Set.empty')]                          == 'Algebra.Graph.LabelledAdjacencyMap.vertex' x
-- fromAdjacencySets [(x, Set.'Set.singleton' y)]                    == 'Algebra.Graph.LabelledAdjacencyMap.edge' x y
-- fromAdjacencySets . map (fmap Set.'Set.fromList') . 'Algebra.Graph.LabelledAdjacencyMap.adjacencyList' == id
-- 'Algebra.Graph.LabelledAdjacencyMap.overlay' (fromAdjacencySets xs) (fromAdjacencySets ys)       == fromAdjacencySets (xs ++ ys)
-- @
fromAdjacencySets :: (Ord a, Dioid e) => [(a, Set a)] -> LabelledAdjacencyMap a e
fromAdjacencySets ss = LAM $ Map.unionWith (Map.unionWith (|+|)) vs es
  where
    vs = Map.fromSet (const Map.empty) . Set.unions $ map snd ss
    es =
      Map.fromListWith
        (Map.unionWith (|+|))
        (fmap (\(a, s) -> (a, set2map s)) ss)
    set2map = Map.fromSet (const one)


-- | Check if the internal graph representation is consistent, i.e. that all
-- edges refer to existing vertices. It should be impossible to create an
-- inconsistent adjacency map, and we use this function in testing.
-- /Note: this function is for internal use only/.
--
-- @
-- consistent 'Algebra.Graph.LabelledAdjacencyMap.empty'                  == True
-- consistent ('Algebra.Graph.LabelledAdjacencyMap.vertex' x)             == True
-- consistent ('Algebra.Graph.LabelledAdjacencyMap.overlay' x y)          == True
-- consistent ('Algebra.Graph.LabelledAdjacencyMap.connect' x y)          == True
-- consistent ('Algebra.Graph.LabelledAdjacencyMap.edge' x y)             == True
-- consistent ('Algebra.Graph.LabelledAdjacencyMap.edges' xs)             == True
-- consistent ('Algebra.Graph.LabelledAdjacencyMap.graph' xs ys)          == True
-- consistent ('Algebra.Graph.LabelledAdjacencyMap.fromAdjacencyList' xs) == True
-- @
consistent :: (Ord a) => LabelledAdjacencyMap a e -> Bool
consistent (LAM m) = referredToVertexSet m `Set.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: (Ord a) => Map a (Map a e) -> Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: Map a (Map a e) -> [(a, a)]
internalEdgeList m = do
        (x, ys) <- Map.toAscList m
        (y, _ ) <- Map.toAscList ys
        return (x, y)
