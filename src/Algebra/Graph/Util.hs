{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Algebra.Graph.Util (Dfs, dfsForest, adjacencyList, edgeList, transpose) where

import Data.Graph (dff, graphFromEdges', Forest)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Algebra.Graph
import Algebra.Graph.AdjacencyMap

newtype Dfs a = D { fromDfs :: AdjacencyMap a } deriving (Show, Num)

instance Ord a => Eq (Dfs a) where
    x == y = dfsForest x == dfsForest y

dfsForest :: Ord a => Dfs a -> Forest a
dfsForest (D x) = map (fmap $ (\(_, v, _) -> v) . get) $ dff g
  where
    (g, get) = graphFromEdges' . map (\(v, us) -> ((), v, us)) $ adjacencyList x

adjacencyList :: AdjacencyMap a -> [(a, [a])]
adjacencyList = map (fmap Set.toAscList) . Map.toAscList . adjacencyMap

edgeList :: AdjacencyMap a -> [(a, a)]
edgeList = concatMap (\(x, ys) -> map (x,) ys) . adjacencyList

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

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (Dfs a) where
    type Vertex (Dfs a) = a
    empty       = D empty
    vertex      = D . vertex
    overlay x y = D $ fromDfs x `overlay` fromDfs y
    connect x y = D $ fromDfs x `connect` fromDfs y
