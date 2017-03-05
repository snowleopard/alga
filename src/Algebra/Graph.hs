module Algebra.Graph (
    Graph (..), vertices, overlays, connects, edge, edges, graph, isSubgraphOf,
    path, circuit, clique, star, tree, forest, biclique
    ) where

import Data.Tree

class Graph g where
    type Vertex g
    empty   :: g
    vertex  :: Vertex g -> g
    overlay :: g -> g -> g
    connect :: g -> g -> g

vertices :: Graph g => [Vertex g] -> g
vertices = overlays . map vertex

overlays :: Graph g => [g] -> g
overlays = foldr overlay empty

connects :: Graph g => [g] -> g
connects = foldr connect empty

edge :: Graph g => Vertex g -> Vertex g -> g
edge x y = vertex x `connect` vertex y

edges :: Graph g => [(Vertex g, Vertex g)] -> g
edges = overlays . map (uncurry edge)

graph :: Graph g => [Vertex g] -> [(Vertex g, Vertex g)] -> g
graph vs es = overlay (vertices vs) (edges es)

isSubgraphOf :: (Graph g, Eq g) => g -> g -> Bool
isSubgraphOf x y = overlay x y == y

path :: Graph g => [Vertex g] -> g
path []  = empty
path [x] = vertex x
path xs  = edges $ zip xs (tail xs)

circuit :: Graph g => [Vertex g] -> g
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

clique :: Graph g => [Vertex g] -> g
clique = connects . map vertex

biclique :: Graph g => [Vertex g] -> [Vertex g] -> g
biclique xs ys = connect (vertices xs) (vertices ys)

star :: Graph g => Vertex g -> [Vertex g] -> g
star x ys = connect (vertex x) (vertices ys)

tree :: Graph g => Tree (Vertex g) -> g
tree (Node x f) = overlay (star x $ map rootLabel f) (forest f)

forest :: Graph g => Forest (Vertex g) -> g
forest = overlays . map tree

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
