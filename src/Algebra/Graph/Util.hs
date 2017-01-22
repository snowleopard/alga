{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Algebra.Graph.Util (
    Dfs, dfsForest, TopSort, isTopSort, topSort, mapVertices, vertexSet,
    transpose, removeVertex, induce, gmap
    ) where

import qualified Data.Graph as Std
import Data.Graph (dff, graphFromEdges', Forest)
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Set as Set
import           Data.Set (Set)

import Algebra.Graph
import qualified Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.AdjacencyMap hiding (mapVertices, vertexSet, transpose)

newtype Dfs a = D { fromDfs :: AdjacencyMap a } deriving (Show, Num)

instance Ord a => Eq (Dfs a) where
    x == y = dfsForest x == dfsForest y

dfsForest :: Ord a => Dfs a -> Forest a
dfsForest (D x) = map (fmap get) $ dff g
  where
    (g, get) = toStd x

toStd :: Ord a => AdjacencyMap a -> (Std.Graph, Std.Vertex -> a)
toStd x = (g, \v -> case get v of (_, u, _) -> u)
  where
    (g, get) = graphFromEdges' . map (\(v, us) -> ((), v, us)) $ adjacencyList x

newtype TopSort a = TS { fromTopSort :: AdjacencyMap a } deriving (Show, Num)

instance Ord a => Eq (TopSort a) where
    x == y = topSort x == topSort y

mapVertices :: (Ord a, Ord b) => (a -> b) -> TopSort a -> TopSort b
mapVertices f = TS . AM.mapVertices f . fromTopSort

vertexSet :: TopSort a -> Set a
vertexSet = AM.vertexSet . fromTopSort

topSort :: Ord a => TopSort a -> Maybe [a]
topSort (TS x) = if isTopSort x result then Just result else Nothing
  where
    (g, get) = toDownStd x
    result   = map get $ Std.topSort g

toDownStd :: Ord a => AdjacencyMap a -> (Std.Graph, Std.Vertex -> a)
toDownStd x = (g, \v -> case get v of (_, Down u, _) -> u)
  where
    (g, get) = graphFromEdges' . map (\(v, us) -> ((), Down v, map Down us))
             $ adjacencyList x

isTopSort :: forall a. Ord a => AdjacencyMap a -> [a] -> Bool
isTopSort x = go Set.empty
  where
    go :: Set a -> [a] -> Bool
    go seen []     = seen == Map.keysSet (adjacencyMap x)
    go seen (v:vs) = let newSeen = seen `seq` Set.insert v seen
        in postset v x `Set.intersection` newSeen == Set.empty && go newSeen vs

-- Note: Transpose can only transpose polymorphic graphs.
newtype Transpose a = T { transpose :: a }

instance Graph g => Graph (Transpose g) where
    type Vertex (Transpose g) = Vertex g
    empty       = T empty
    vertex      = T . vertex
    overlay x y = T $ overlay (transpose x) (transpose y)
    connect x y = T $ connect (transpose y) (transpose x)

instance (Num g, Graph g) => Num (Transpose g) where
    fromInteger = T . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

newtype Induce a g = I { getInduce :: (a -> Bool) -> g }

induce :: (Vertex g -> Bool) -> Induce (Vertex g) g -> g
induce = flip getInduce

removeVertex :: Eq (Vertex g) => Vertex g -> Induce (Vertex g) g -> g
removeVertex v = induce (/= v)

instance (Graph g, Vertex g ~ a) => Graph (Induce a g) where
    type Vertex (Induce a g) = a
    empty       = I $ \_ -> empty
    vertex  x   = I $ \f -> if f x then vertex x else empty
    overlay x y = I $ \f -> getInduce x f `overlay` getInduce y f
    connect x y = I $ \f -> getInduce x f `connect` getInduce y f

instance (Graph g, Num a, Vertex g ~ a) => Num (Induce a g) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

newtype GraphF a b g = GF { getGraphF :: (a -> b) -> g }

gmap :: (a -> Vertex g) -> GraphF a (Vertex g) g -> g
gmap = flip getGraphF

instance (Graph g, Vertex g ~ b) => Graph (GraphF a b g) where
    type Vertex (GraphF a b g) = a
    empty       = GF $ \_ -> empty
    vertex  x   = GF $ \f -> vertex (f x)
    overlay x y = GF $ \f -> getGraphF x f `overlay` getGraphF y f
    connect x y = GF $ \f -> getGraphF x f `connect` getGraphF y f

instance (Graph g, Num a, Vertex g ~ b) => Num (GraphF a b g) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (Dfs a) where
    type Vertex (Dfs a) = a
    empty       = D empty
    vertex      = D . vertex
    overlay x y = D $ fromDfs x `overlay` fromDfs y
    connect x y = D $ fromDfs x `connect` fromDfs y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (TopSort a) where
    type Vertex (TopSort a) = a
    empty       = TS empty
    vertex      = TS . vertex
    overlay x y = TS $ fromTopSort x `overlay` fromTopSort y
    connect x y = TS $ fromTopSort x `connect` fromTopSort y
