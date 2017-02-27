module Algebra.Graph.Dfs (DfsForest, dfsForest) where

import Data.Graph (dff, Forest)
import Algebra.Graph
import Algebra.Graph.AdjacencyMap

newtype DfsForest a = D { fromDfsForest :: AdjacencyMap a } deriving (Show, Num)

instance Ord a => Eq (DfsForest a) where
    D x == D y = dfsForest x == dfsForest y

dfsForest :: Ord a => AdjacencyMap a -> Forest a
dfsForest x = let (g, r) = toKL x in fmap (fmap r) $ dff g

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (DfsForest a) where
    type Vertex (DfsForest a) = a
    empty       = D empty
    vertex      = D . vertex
    overlay x y = D $ fromDfsForest x `overlay` fromDfsForest y
    connect x y = D $ fromDfsForest x `connect` fromDfsForest y
