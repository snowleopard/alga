-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Symmetric
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- An abstract implementation of symmetric binary relations. Use
-- "Algebra.Graph.Class" for polymorphic construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Symmetric (
    -- * Data structure
    SI.Relation, toSymmetric, fromSymmetric,

    -- * Basic graph construction primitives
    SI.empty, SI.vertex, edge, SI.overlay, SI.connect, vertices, edges, overlays, connects,

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

import qualified Algebra.Graph.Relation as R
import qualified Algebra.Graph.Relation.Internal as RI
import qualified Algebra.Graph.Relation.Symmetric.Internal as SI

import qualified Data.Set as Set
import Data.Tree 
import qualified Data.Tree as Tree
import Data.Tuple (swap)

-- | Construct a symmetric relation from a 'Relation'.
-- Complexity: /O(m*log(m))/ time.
--
-- @
-- toSymmetric ('Algebra.Graph.Relation.edge' 1 2) == 'edge 1 2'
-- toSymmetric . 'fromSymmetric'                   == id
-- 'vertexCount' . toSymmetric                     == 'Algebra.Graph.Relation.vertexCount'
-- (* 2) . 'edgeCount' . toSymmetric               >= 'Algebra.Graph.Relation.edgeCount'
-- @
toSymmetric :: Ord a => R.Relation a -> SI.Relation a
toSymmetric = SI.SR . R.symmetricClosure

-- | Extract the underlying relation.
-- Complexity: /O(1)/ time.
--
-- @
-- fromSymmetric ('edge' 1 2)                           == 'Algebra.Graph.Relation.edges' [(1,2), (2,1)]
-- 'Algebra.Graph.Relation.vertexCount' . fromSymmetric == 'vertexCount'
-- 'Algebra.Graph.Relation.edgeCount' . fromSymmetric   <= (* 2) . 'edgeCount'
-- @
fromSymmetric :: SI.Relation a -> R.Relation a
fromSymmetric = SI.fromSymmetric

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y                 == edges [(x,y), (y,x)]
-- edge x y                 == connect (vertex x) (vertex y)
-- edge x y                 == edge y x
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Ord a => a -> a -> SI.Relation a
edge x y = SI.SR $ RI.Relation (Set.fromList [x, y]) (Set.fromList [(x, y), (y,x)]) 

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L * log(L))/ time and /O(L)/ memory, where /L/ is the length
-- of the given list.
--
-- @
-- vertices []              == 'empty'
-- vertices [x]             == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Ord a => [a] -> SI.Relation a
vertices = SI.SR . R.vertices

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []             == 'empty'
-- edges [(x,y)]        == 'edge' x y
-- edges [(x,y), (y,x)] == 'edge' x y
-- @
edges :: Ord a => [(a, a)] -> SI.Relation a
edges es = SI.SR $ RI.Relation (Set.fromList $ uncurry (++) $ unzip es) (Set.fromList es `Set.union` Set.map swap (Set.fromList es))

-- | Overlay a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- overlays []          == 'empty'
-- overlays [x]         == x
-- overlays [x,y]       == 'overlay' x y
-- overlays             == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: Ord a => [SI.Relation a] -> SI.Relation a
overlays = SI.SR . R.overlays . map fromSymmetric

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []          == 'empty'
-- connects [x]         == x
-- connects [x,y]       == 'connect' x y
-- connects             == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- connects             == connects . 'reverse'
-- @
connects :: Ord a => [SI.Relation a] -> SI.Relation a
connects = foldr SI.connect SI.empty

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- isSubgraphOf 'empty'         x               ==  True
-- isSubgraphOf ('vertex' x)    'empty'         ==  False
-- isSubgraphOf x             ('overlay' x y)   ==  True
-- isSubgraphOf ('overlay' x y) ('connect' x y) ==  True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  ==  True
-- isSubgraphOf x y                             ==> x <= y
-- isSubgraphOf (edge x y) (edge y x)           == True
-- @
isSubgraphOf :: Ord a => SI.Relation a -> SI.Relation a -> Bool
isSubgraphOf x y = R.isSubgraphOf (fromSymmetric x) (fromSymmetric y)

-- | Check if a relation is empty.
-- Complexity: /O(1)/ time.
--
-- @
-- isEmpty 'empty'                         == True
-- isEmpty ('overlay' 'empty' 'empty')     == True
-- isEmpty ('vertex' x)                    == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- isEmpty ('removeEdge' x y $ 'edge' x y) == False
-- @
isEmpty :: SI.Relation a -> Bool
isEmpty = R.isEmpty . fromSymmetric

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == 'const' False
-- @
hasVertex :: Ord a => a -> SI.Relation a -> Bool
hasVertex x = R.hasVertex x . fromSymmetric

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge y x ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == 'const' False
-- hasEdge x y                    == 'elem' (min x y, max x y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> SI.Relation a -> Bool
hasEdge x y = R.hasEdge x y . fromSymmetric

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                     ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y  ==> x \< y
-- @
vertexCount :: SI.Relation a -> Int
vertexCount = R.vertexCount . fromSymmetric

-- | The number of edges in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount              == 'length' . 'edgeList'
-- @
edgeCount :: Ord a => SI.Relation a -> Int
edgeCount = length . edgeList

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: SI.Relation a -> [a]
vertexList = R.vertexList . fromSymmetric

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList (edge x y)       == [(min x y, max y x)]
-- edgeList ('star' 2 [3,1]) == [(1,2), (2,3)]
-- @
edgeList :: Ord a => SI.Relation a -> [(a, a)]
edgeList = SI.deduplicate . R.edgeList . fromSymmetric

-- | The set of vertices of a given graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: SI.Relation a -> Set.Set a
vertexSet = R.vertexSet . fromSymmetric

-- | The set of edges of a given graph.
-- Complexity: /O(m)/ time.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (min x y, max x y)
-- @
edgeSet :: Ord a => SI.Relation a -> Set.Set (a, a)
edgeSet = SI.deduplicateSet . R.edgeSet . fromSymmetric

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
adjacencyList :: Eq a => SI.Relation a -> [(a, [a])]
adjacencyList = R.adjacencyList . fromSymmetric

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'vertex' x
-- path [x,y] == 'edge' x y
-- path       == path . 'reverse'
-- @
path :: Ord a => [a] -> SI.Relation a
path xs = case xs of []     -> SI.empty
                     [x]    -> SI.vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit []      == 'empty'
-- circuit [x]     == 'edge' x x
-- circuit [x,y]   == 'edge' x y
-- circuit [x,y,z] == 'edges' [(x,y),(x,z),(y,z)]
-- circuit         == circuit . 'reverse'
-- @
circuit :: Ord a => [a] -> SI.Relation a
circuit []     = SI.empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

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
clique :: Ord a => [a] -> SI.Relation a
clique = SI.SR . R.symmetricClosure . R.clique

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time + /O(m*log(m)) time from computing the symmetricClosure and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1),(x1,y2),(x2,x2),(x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: Ord a => [a] -> [a] -> SI.Relation a
biclique xs = SI.SR . R.symmetricClosure . R.biclique xs

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
star :: Ord a => a -> [a] -> SI.Relation a
star x [] = SI.vertex x
star x ys = SI.connect (SI.vertex x) (vertices ys)

-- | The /stars/ formed by overlaying a list of 'star's. An inverse of
-- 'adjacencyList'.
-- Complexity: /O(L * log(n))/ time, memory and size, where /L/ is the total
-- size of the input.
--
-- @
-- stars []                        == 'empty'
-- stars [(x, [])]                 == 'vertex' x
-- stars [(x, [y])]                == 'edge' x y
-- stars [(x, ys)]                 == 'star' x ys
-- stars                           == 'overlays' . 'map' ('uncurry' 'star')
-- stars . 'adjacencyList'         == id
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: Ord a => [(a, [a])] -> SI.Relation a
stars as = SI.SR $ RI.Relation (Set.fromList vs) (Set.fromList es)
  where
    vs = concatMap (uncurry (:)) as
    es = [ (x, y) | (x, ys) <- as, y <- ys ] ++ [ (y, x) | (x, ys) <- as, y <- ys ]

-- | The /tree graph/ constructed from a given 'Tree.Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Ord a => Tree.Tree a -> SI.Relation a
tree (Node x []) = SI.vertex x
tree (Node x f ) = star x (map rootLabel f)
    `SI.overlay` forest (filter (not . null . subForest) f)

-- | The /forest graph/ constructed from a given 'Tree.Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . 'map' 'tree'
-- @
forest :: Ord a => Tree.Forest a -> SI.Relation a
forest = overlays . map tree

-- | Remove a vertex from a given graph.
-- Complexity: /O(n + m)/ time.
--
-- @
-- removeVertex x ('vertex' x)     == 'empty'
-- removeVertex 1 ('vertex' 2)     == 'vertex' 2
-- removeVertex x ('edge' x x)     == 'empty'
-- removeVertex 1 ('edge' 1 2)     == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: Ord a => a -> SI.Relation a -> SI.Relation a
removeVertex x = SI.SR . R.removeVertex x . fromSymmetric

-- | Remove an edge from a given graph.
-- Complexity: /O(log(m))/ time.
--
-- @
-- removeEdge x y ('edge' x y)       == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y   == removeEdge x y
-- removeEdge x y                    == removeEdge y x
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)    == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)    == 1 * 1 + 2 * 2
-- @
removeEdge :: Ord a => a -> a -> SI.Relation a -> SI.Relation a
removeEdge x y r = let (RI.Relation d rr) = fromSymmetric r
                       in SI.SR $ RI.Relation d (Set.delete (y, x) . Set.delete (x, y) $ rr)

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x              == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y              == 'mergeVertices' (== x) y
-- @
replaceVertex :: Ord a => a -> a -> SI.Relation a -> SI.Relation a
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices ('const' False) x    == id
-- mergeVertices (== x) y             == 'replaceVertex' x y
-- mergeVertices 'even' 1 (0 * 2)     == 1 * 1
-- mergeVertices 'odd'  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: Ord a => (a -> Bool) -> a -> SI.Relation a -> SI.Relation a
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transform a graph by applying a function to each of its vertices. This is
-- similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'Relation'.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('edge' x y) == 'edge' (f x) (f y)
-- gmap id             == id
-- gmap f . gmap g     == gmap (f . g)
-- @
gmap :: Ord b => (a -> b) -> SI.Relation a -> SI.Relation b
gmap f = SI.SR . R.gmap f . fromSymmetric

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce ('const' True ) x      == x
-- induce ('const' False) x      == 'empty'
-- induce (/= x)                 == 'removeVertex' x
-- induce p . induce q           == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> SI.Relation a -> SI.Relation a
induce p = SI.SR . R.induce p . fromSymmetric

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
neighbours :: Ord a => a -> SI.Relation a -> Set.Set a
neighbours x = R.postSet x . fromSymmetric
