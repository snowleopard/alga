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
        , mkLAM
        , consistent
        , edgeLabel
        , (-<)
        , (>-)


    -- * Interoperability with King-Launchbury graphs
        , GraphKL(..)
        , mkGraphKL
        )
where

import           Data.List
import           Data.Map.Strict                ( Map
                                                , keysSet
                                                , fromSet
                                                )
import           Data.Set                       ( Set )
import Data.Maybe (fromMaybe)
import           Algebra.Graph.Class           as C
import           Algebra.Graph.Labelled         ( Dioid(..)
                                                , (|+|)
                                                , (|*|)
                                                )

import qualified Data.Graph                    as KL
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

data LabelledAdjacencyMap a e = LAM {
    -- | The /adjacency map/ of the graph: each vertex is associated with a set
    -- of its direct successors.
    labelledAdjacencyMap :: !(Map a (Map a e)),
    -- | Cached King-Launchbury representation.
    -- /Note: this field is for internal use only/.
    graphKL :: GraphKL a }

lAdjacencyMap :: LabelledAdjacencyMap a e -> Map a (Map a e)
lAdjacencyMap = labelledAdjacencyMap

-- | Construct an 'LabelledAdjacencyMap' from a map of successor sets and (lazily)
-- compute the corresponding King-Launchbury representation.
-- /Note: this function is for internal use only/.
mkLAM :: (Ord a) => Map a (Map a e) -> LabelledAdjacencyMap a e
mkLAM m = LAM m (mkGraphKL m)

instance (Eq a, Eq e) => Eq (LabelledAdjacencyMap a e) where
    x == y = lAdjacencyMap x == lAdjacencyMap y

instance (Ord a, Show a, Ord e, Show e, Dioid e) => Show (LabelledAdjacencyMap a e) where
    show (LAM m _)
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

instance (Ord a, Dioid e) => C.Graph (LabelledAdjacencyMap a e) where
  type Vertex (LabelledAdjacencyMap a e) = a
  empty = mkLAM Map.empty
  vertex x = mkLAM $ Map.singleton x Map.empty
  overlay x y =
    mkLAM $
    Map.unionWith (Map.unionWith (|+|)) (lAdjacencyMap x) (lAdjacencyMap y)
  connect = lconnect one

lconnect
        :: (Ord a, Dioid e)
        => e
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
        -> LabelledAdjacencyMap a e
lconnect e x y =
  mkLAM $
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

instance ToGraph (LabelledAdjacencyMap a e) where
  type ToVertex (LabelledAdjacencyMap a e) = a
  toGraph =
    overlays .
    map (uncurry star . fmap (fmap fst . Map.toList)) .
    Map.toList . lAdjacencyMap
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
consistent (LAM m _) = referredToVertexSet m `Set.isSubsetOf` keysSet m

-- The set of vertices that are referred to by the edges
referredToVertexSet :: (Ord a) => Map a (Map a e) -> Set a
referredToVertexSet = Set.fromList . uncurry (++) . unzip . internalEdgeList

-- The list of edges in adjacency map
internalEdgeList :: Map a (Map a e) -> [(a, a)]
internalEdgeList m = do
        (x, ys) <- Map.toAscList m
        (y, _ ) <- Map.toAscList ys
        return (x, y)


-- | 'GraphKL' encapsulates King-Launchbury graphs, which are implemented in
-- the "Data.Graph" module of the @containers@ library.
-- /Note: this data structure is for internal use only/.
--
-- If @mkGraphKL (adjacencyMap g) == h@ then the following holds:
--
-- @
-- map ('fromVertexKL' h) ('Data.Graph.vertices' $ 'toGraphKL' h)                               == 'Algebra.Graph.LabelledAdjacencyMap.vertexList' g
-- map (\\(x, y) -> ('fromVertexKL' h x, 'fromVertexKL' h y)) ('Data.Graph.edges' $ 'toGraphKL' h) == 'Algebra.Graph.LabelledAdjacencyMap.edgeList' g
-- @
data GraphKL a = GraphKL {
    -- | Array-based graph representation (King and Launchbury, 1995).
    toGraphKL :: KL.Graph,
    -- | A mapping of "Data.Graph.Vertex" to vertices of type @a@.
    fromVertexKL :: KL.Vertex -> a,
    -- | A mapping from vertices of type @a@ to "Data.Graph.Vertex".
    -- Returns 'Nothing' if the argument is not in the graph.
    toVertexKL :: a -> Maybe KL.Vertex }

-- | Build 'GraphKL' from a map of successor sets.
-- /Note: this function is for internal use only/.
mkGraphKL :: Ord a => Map a (Map a e) -> GraphKL a
mkGraphKL m = GraphKL
        { toGraphKL    = g
        , fromVertexKL = \u -> case r u of
                (_, v, _) -> v
        , toVertexKL   = t
        }
    where
        (g, r, t) = KL.graphFromEdges
                [ ((), v, Set.toAscList (Map.keysSet us))
                | (v, us) <- Map.toAscList m
                ]
