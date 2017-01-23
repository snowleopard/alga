{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.Dfs (Dfs, dfsForest, toStd) where

import qualified Data.Graph as Std
import Data.Graph (dff, graphFromEdges', Forest)

import Algebra.Graph
import Algebra.Graph.AdjacencyMap

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

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (Dfs a) where
    type Vertex (Dfs a) = a
    empty       = D empty
    vertex      = D . vertex
    overlay x y = D $ fromDfs x `overlay` fromDfs y
    connect x y = D $ fromDfs x `connect` fromDfs y
