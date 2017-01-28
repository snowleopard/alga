{-# LANGUAGE FlexibleContexts #-}
module Algebra.Graph (
    Graph (..), edge, vertices, clique, fromEdgeList, path, circuit, tree,
    forest, arbitraryGraph, isSubgraphOf, foldg, overlays, connects
    ) where

import Test.QuickCheck
import Data.Tree

class Graph g where
    type Vertex g
    empty   :: g
    vertex  :: Vertex g -> g
    overlay :: g -> g -> g
    connect :: g -> g -> g

edge :: Graph g => Vertex g -> Vertex g -> g
edge x y = vertex x `connect` vertex y

vertices :: Graph g => [Vertex g] -> g
vertices = overlays . map vertex

clique :: Graph g => [Vertex g] -> g
clique = connects . map vertex

fromEdgeList :: Graph g => [(Vertex g, Vertex g)] -> g
fromEdgeList = overlays . map (uncurry edge)

isSubgraphOf :: (Graph g, Eq g) => g -> g -> Bool
isSubgraphOf x y = overlay x y == y

path :: Graph g => [Vertex g] -> g
path []  = empty
path [x] = vertex x
path xs  = fromEdgeList $ zip xs (tail xs)

circuit :: Graph g => [Vertex g] -> g
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

tree :: Graph g => Tree (Vertex g) -> g
tree (Node x f) = vertex x `connect` vertices (map rootLabel f) `overlay` forest f

forest :: Graph g => Forest (Vertex g) -> g
forest = overlays . map tree

arbitraryGraph :: (Graph g, Arbitrary (Vertex g)) => Gen g
arbitraryGraph = sized graph
  where
    graph 0 = return empty
    graph 1 = vertex <$> arbitrary
    graph n = do
        left <- choose (0, n)
        oneof [ overlay <$> (graph left) <*> (graph $ n - left)
              , connect <$> (graph left) <*> (graph $ n - left) ]

-- 'foldr f empty' adds a redundant empty to the result; foldg avoids this
foldg :: Graph g => (g -> g -> g) -> [g] -> g
foldg _ [] = empty
foldg f gs = foldr1 f gs

overlays :: Graph g => [g] -> g
overlays = foldg overlay

connects :: Graph g => [g] -> g
connects = foldg connect
