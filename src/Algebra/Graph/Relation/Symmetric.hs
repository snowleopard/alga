-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Symmetric
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of symmetric binary relations. To avoid name
-- clashes with "Algebra.Graph.Relation", this module can be imported qualified:
--
-- @
-- import qualified Algebra.Graph.Relation.Symmetric as Symmetric
-- @
--
-- 'Relation' is an instance of the 'Algebra.Graph.Class.Graph' type
-- class, which can be used for polymorphic graph construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Symmetric (
    -- * Data structure
    Relation, toSymmetric, fromSymmetric,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexSet, edgeSet, neighbours,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, gmap, induce,
  ) where

import Algebra.Graph.Relation.Symmetric.Internal
import Data.Coerce
import Data.Set (Set)
import Data.Tree
import Data.Tuple

import qualified Data.Set as Set

import qualified Algebra.Graph.Relation          as R
import qualified Algebra.Graph.Relation.Internal as RI

-- | Construct a symmetric relation from a given "Algebra.Graph.Relation".
-- Complexity: /O(m*log(m))/ time.
--
-- @
-- toSymmetric ('Algebra.Graph.Relation.edge' 1 2)         == 'edge' 1 2
-- toSymmetric . 'fromSymmetric'    == id
-- 'fromSymmetric'    . toSymmetric == 'Algebra.Graph.Relation.symmetricClosure'
-- 'vertexCount'      . toSymmetric == 'Algebra.Graph.Relation.vertexCount'
-- (*2) . 'edgeCount' . toSymmetric >= 'Algebra.Graph.Relation.edgeCount'
-- @
toSymmetric :: Ord a => R.Relation a -> Relation a
toSymmetric = SR . R.symmetricClosure

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- edge x y               == 'edge' y x
-- edge x y               == 'edges' [(x,y), (y,x)]
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Ord a => a -> a -> Relation a
edge x y = SR $ RI.Relation (Set.fromList [x, y]) (Set.fromList [(x,y), (y,x)])

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Ord a => [a] -> Relation a
vertices = coerce R.vertices

-- TODO: Optimise by avoiding multiple list traversal.
-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []             == 'empty'
-- edges [(x,y)]        == 'edge' x y
-- edges [(x,y), (y,x)] == 'edge' x y
-- @
edges :: Ord a => [(a, a)] -> Relation a
edges es = SR $ RI.Relation
    (Set.fromList $ uncurry (++) $ unzip es) (Set.fromList (es ++ map swap es))

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: Ord a => [Relation a] -> Relation a
overlays = coerce R.overlays

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- connects           == connects . 'reverse'
-- @
connects :: Ord a => [Relation a] -> Relation a
connects = foldr connect empty

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- isSubgraphOf 'empty'         x             ==  True
-- isSubgraphOf ('vertex' x)    'empty'         ==  False
-- isSubgraphOf x             ('overlay' x y) ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  ==  True
-- isSubgraphOf ('edge' x y)    ('edge' y x)    ==  True
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: Ord a => Relation a -> Relation a -> Bool
isSubgraphOf = coerce R.isSubgraphOf

-- | Check if a relation is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: Relation a -> Bool
isEmpty = coerce R.isEmpty

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Ord a => a -> Relation a -> Bool
hasVertex = coerce R.hasVertex

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y ('edge' y x)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                  == 'elem' (min x y, max x y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> Relation a -> Bool
hasEdge = coerce R.hasEdge

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: Relation a -> Int
vertexCount = coerce R.vertexCount

-- | The number of edges in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => Relation a -> Int
edgeCount = length . edgeList

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Relation a -> [a]
vertexList = coerce R.vertexList

-- | The sorted list of edges of a graph, where edge vertices appear in the
-- non-decreasing order.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- Note: If you need the sorted list of edges where an edge appears in both
-- directions, use @'Algebra.Graph.Relation.edgeList' . 'fromSymmetric'@.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(min x y, max y x)]
-- edgeList ('star' 2 [3,1]) == [(1,2), (2,3)]
-- @
edgeList :: Ord a => Relation a -> [(a, a)]
edgeList = Set.toAscList . edgeSet

-- | The set of vertices of a given graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: Relation a -> Set a
vertexSet = coerce R.vertexSet

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'          == []
-- adjacencyList ('vertex' x)     == [(x, [])]
-- adjacencyList ('edge' 1 2)     == [(1, [2]), (2, [1])]
-- adjacencyList ('star' 2 [3,1]) == [(1, [2]), (2, [1,3]), (3, [2])]
-- 'stars' . adjacencyList        == id
-- @
adjacencyList :: Eq a => Relation a -> [(a, [a])]
adjacencyList = coerce R.adjacencyList

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'vertex' x
-- path [x,y] == 'edge' x y
-- path       == path . 'reverse'
-- @
path :: Ord a => [a] -> Relation a
path xs = case xs of []     -> empty
                     [x]    -> vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'edge' x x
-- circuit [x,y] == 'edge' x y
-- circuit       == circuit . 'reverse'
-- @
circuit :: Ord a => [a] -> Relation a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- TODO: Optimise by avoiding the call to 'R.symmetricClosure'.
-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time + /O(m*log(m)) time from computing the symmetricClosure and /O(n + m)/ memory.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique            == clique . 'reverse'
-- @
clique :: Ord a => [a] -> Relation a
clique = SR . R.symmetricClosure . R.clique

-- TODO: Optimise by avoiding the call to 'R.symmetricClosure'.
-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time + /O(m*log(m)) time from computing the symmetricClosure and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,x2), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: Ord a => [a] -> [a] -> Relation a
biclique xs = SR . R.symmetricClosure . R.biclique xs

-- TODO: Optimise.
-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: Ord a => a -> [a] -> Relation a
star x [] = vertex x
star x ys = connect (vertex x) (vertices ys)

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L * log(n))/ time, memory and size, where /L/ is the total
-- size of the input.
--
-- @
-- stars []                      == 'empty'
-- stars [(x, [])]               == 'vertex' x
-- stars [(x, [y])]              == 'edge' x y
-- stars [(x, ys)]               == 'star' x ys
-- stars                         == 'overlays' . 'map' ('uncurry' 'star')
-- stars . 'adjacencyList'         == id
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: Ord a => [(a, [a])] -> Relation a
stars as = SR $ RI.Relation (Set.fromList vs) (Set.fromList es)
  where
    vs = concat [ x : ys           | (x, ys) <- as          ]
    es = concat [ [(x, y), (y, x)] | (x, ys) <- as, y <- ys ]

-- | The /tree graph/ constructed from a given 'Tree.Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Ord a => Tree a -> Relation a
tree (Node x []) = vertex x
tree (Node x f ) = star x (map rootLabel f)
    `overlay` forest (filter (not . null . subForest) f)

-- | The /forest graph/ constructed from a given 'Tree.Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . 'map' 'tree'
-- @
forest :: Ord a => Forest a -> Relation a
forest = overlays . map tree

-- | Remove a vertex from a given graph.
-- Complexity: /O(n + m)/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> Relation a -> Relation a
removeVertex = coerce R.removeVertex

-- | Remove an edge from a given graph.
-- Complexity: /O(log(m))/ time.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y                  == removeEdge y x
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Ord a => a -> a -> Relation a -> Relation a
removeEdge x y r = SR $ RI.Relation d (Set.delete (y, x) $ Set.delete (x, y) rr)
  where
    RI.Relation d rr = fromSymmetric r

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Relation'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Ord a => a -> a -> Relation a -> Relation a
replaceVertex = coerce R.replaceVertex

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: Ord a => (a -> Bool) -> a -> Relation a -> Relation a
mergeVertices = coerce R.mergeVertices

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'Relation'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: Ord b => (a -> b) -> Relation a -> Relation b
gmap = coerce R.gmap

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> Relation a -> Relation a
induce = coerce R.induce

-- | The set of /neighbours/ of an element @x@ is the set of elements that are
-- related to it, i.e. @neighbours x == { a | aRx }@. In the context of undirected
-- graphs, this corresponds to the set of /adjacent/ vertices of vertex @x@.
--
-- @
-- neighbours x 'Algebra.Graph.Class.empty'      == Set.'Set.empty'
-- neighbours x ('Algebra.Graph.Class.vertex' x) == Set.'Set.empty'
-- neighbours x ('Algebra.Graph.Class.edge' x y) == Set.'Set.fromList' [y]
-- neighbours y ('Algebra.Graph.Class.edge' x y) == Set.'Set.fromList' [x]
-- @
neighbours :: Ord a => a -> Relation a -> Set a
neighbours = coerce R.postSet
