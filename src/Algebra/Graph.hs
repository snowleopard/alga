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

instance Graph () where
    type Vertex () = ()
    empty          = ()
    vertex  _      = ()
    overlay _ _    = ()
    connect _ _    = ()

-- Note: Maybe g and (a -> g) instances are identical and use the Applicative's
-- pure and <*>. We do not provide a general instance for all Applicative
-- functors because that would lead to overlapping instances.
instance Graph g => Graph (Maybe g) where
    type Vertex (Maybe g) = Vertex g
    empty       = pure empty
    vertex      = pure . vertex
    overlay x y = overlay <$> x <*> y
    connect x y = connect <$> x <*> y

instance Graph g => Graph (a -> g) where
    type Vertex (a -> g) = Vertex g
    empty       = pure empty
    vertex      = pure . vertex
    overlay x y = overlay <$> x <*> y
    connect x y = connect <$> x <*> y

instance (Graph g, Graph h) => Graph (g, h) where
    type Vertex (g, h)        = (Vertex g     , Vertex h     )
    empty                     = (empty        , empty        )
    vertex  (x,  y )          = (vertex  x    , vertex  y    )
    overlay (x1, y1) (x2, y2) = (overlay x1 x2, overlay y1 y2)
    connect (x1, y1) (x2, y2) = (connect x1 x2, connect y1 y2)
