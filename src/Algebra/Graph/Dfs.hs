module Algebra.Graph.Dfs (Dfs, dfs) where

import Data.Graph (dff, Forest)
import Algebra.Graph
import Algebra.Graph.AdjacencyMap

newtype Dfs a = D { fromDfs :: AdjacencyMap a } deriving (Show, Num)

instance Ord a => Eq (Dfs a) where
    D x == D y = dfs x == dfs y

dfs :: Ord a => AdjacencyMap a -> Forest a
dfs x = let (g, r) = toKL x in fmap (fmap r) $ dff g

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (Dfs a) where
    type Vertex (Dfs a) = a
    empty       = D empty
    vertex      = D . vertex
    overlay x y = D $ fromDfs x `overlay` fromDfs y
    connect x y = D $ fromDfs x `connect` fromDfs y
