{-# LANGUAGE TypeFamilies #-}
module Algebra.Graph (
    Graph (..), vertices, clique, fromEdgeList, path, loop, isSubgraphOf,
    foldg, overlays, connects
    ) where

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
path xs = fromEdgeList $ zip xs (tail xs)

loop :: Graph g => [Vertex g] -> g
loop xs = path $ xs ++ take 1 xs

-- 'foldr f empty' adds a redundant empty to the result; foldg avoids this
foldg :: Graph g => (g -> g -> g) -> [g] -> g
foldg _ [] = empty
foldg f gs = foldr1 f gs

overlays :: Graph g => [g] -> g
overlays = foldg overlay

connects :: Graph g => [g] -> g
connects = foldg connect
