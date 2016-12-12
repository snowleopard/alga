{-# LANGUAGE FlexibleContexts, TypeFamilies, TupleSections #-}
module Algebra.Graph (
    Graph (..), vertices, clique, fromEdgeList, path, circuit, box,
    arbitraryGraph, isSubgraphOf, foldg, overlays, connects
    ) where

import Data.Foldable
import Test.QuickCheck

class Graph g where
    type Vertex g
    empty   :: g
    vertex  :: Vertex g -> g
    overlay :: g -> g -> g
    connect :: g -> g -> g

vertices :: Graph g => [Vertex g] -> g
vertices = overlays . map vertex

clique :: Graph g => [Vertex g] -> g
clique = connects . map vertex

fromEdgeList :: Graph g => [(Vertex g, Vertex g)] -> g
fromEdgeList = overlays . map edge
  where
    edge (x, y) = vertex x `connect` vertex y

isSubgraphOf :: (Graph g, Eq g) => g -> g -> Bool
isSubgraphOf x y = overlay x y == y

path :: Graph g => [Vertex g] -> g
path []  = empty
path [x] = vertex x
path xs  = fromEdgeList $ zip xs (tail xs)

circuit :: Graph g => [Vertex g] -> g
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

arbitraryGraph :: (Graph g, Arbitrary (Vertex g)) => Gen g
arbitraryGraph = sized graph
  where
    graph 0 = return empty
    graph 1 = vertex <$> arbitrary
    graph n = do
        left <- choose (0, n)
        oneof [ overlay <$> (graph left) <*> (graph $ n - left)
              , connect <$> (graph left) <*> (graph $ n - left) ]

box :: (Functor f, Foldable f, Graph (f (a, b))) => f a -> f b -> f (a, b)
box x y = overlays $ xs ++ ys
  where
    xs = map (\b -> fmap (,b) x) $ toList y
    ys = map (\a -> fmap (a,) y) $ toList x

-- 'foldr f empty' adds a redundant empty to the result; foldg avoids this
foldg :: Graph g => (g -> g -> g) -> [g] -> g
foldg _ [] = empty
foldg f gs = foldr1 f gs

overlays :: Graph g => [g] -> g
overlays = foldg overlay

connects :: Graph g => [g] -> g
connects = foldg connect
