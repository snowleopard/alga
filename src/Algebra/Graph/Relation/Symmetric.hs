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
    SymmetricRelation(..), fromRelation, toRelation,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexSet, edgeSet, preSet, postSet, neighbours,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, gmap, induce,

    -- * Relational operations
    compose, -- closure, reflexiveClosure, symmetricClosure, transitiveClosure
  ) where

import qualified Algebra.Graph.Relation as R
import qualified Algebra.Graph.Relation.Internal as RI hiding (empty, vertex, overlay, connect, referredToVertexSet)
import Algebra.Graph.Relation.InternalDerived

import qualified Data.Set as Set
import Data.Tuple
import Data.Tree 
import qualified Data.Tree as Tree

-- | Construct a symmetric relation from a 'Relation'.
-- Complexity: /O(1)/ time.
fromRelation :: R.Relation a -> SymmetricRelation a
fromRelation = SymmetricRelation

-- | Extract the underlying relation.
-- Complexity: /O(m*log(m))/ time.
toRelation :: Ord a => SymmetricRelation a -> R.Relation a
toRelation = R.symmetricClosure . fromSymmetric

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y                 == edges [(1,2),(2,1)]
-- y)) ('connect' ('vertex' y) ('vertex' x))
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) <= 2
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Ord a => a -> a -> SymmetricRelation a
edge x y = SymmetricRelation $ RI.Relation (Set.fromList [x, y]) (Set.fromList [(x, y), (y,x)]) 

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
vertices :: Ord a => [a] -> SymmetricRelation a
vertices = SymmetricRelation . R.vertices

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'overlay' ('edge' x y) ('edge' y x)
-- 'edgeCount' . edges == 'length' . 'Data.List.nub' . 'Data.List.union'
-- . '<map swap, id>'
-- @
edges :: Ord a => [(a, a)] -> SymmetricRelation a
edges es = SymmetricRelation $ RI.Relation (Set.fromList $ uncurry (++) $ unzip es) (Set.fromList es `Set.union` Set.map swap (Set.fromList es))

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
overlays :: Ord a => [SymmetricRelation a] -> SymmetricRelation a
overlays = SymmetricRelation . R.overlays . map fromSymmetric

-- | Connect a given list of graphs.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: Ord a => [SymmetricRelation a] -> SymmetricRelation a
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
-- isSubgraphOf x y                         ==> x <= y
-- @
isSubgraphOf :: Ord a => SymmetricRelation a -> SymmetricRelation a -> Bool
isSubgraphOf x y = R.isSubgraphOf (fromSymmetric x) (fromSymmetric y)

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
isEmpty :: SymmetricRelation a -> Bool
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
hasVertex :: Ord a => a -> SymmetricRelation a -> Bool
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
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> SymmetricRelation a -> Bool
hasEdge x y = R.hasEdge x y . fromSymmetric

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'             ==  0
-- vertexCount ('vertex' x)        ==  1
-- vertexCount                   ==  'length' . 'vertexList'
-- vertexCount x \< vertexCount y ==> x \< y
-- @
vertexCount :: SymmetricRelation a -> Int
vertexCount = R.vertexCount . fromSymmetric

-- | The number of edges in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 2
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: SymmetricRelation a -> Int
edgeCount = R.edgeCount . fromSymmetric

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: SymmetricRelation a -> [a]
vertexList = R.vertexList . fromSymmetric

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y), (y,x)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (1,2), (2,3), (3,2)]
-- @
edgeList :: SymmetricRelation a -> [(a, a)]
edgeList = R.edgeList . fromSymmetric

-- | The set of vertices of a given graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- @
vertexSet :: SymmetricRelation a -> Set.Set a
vertexSet = R.vertexSet . fromSymmetric

-- | The set of edges of a given graph.
-- Complexity: /O(1)/ time.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.fromList' [(x,y), (y,x)]
-- @
edgeSet :: SymmetricRelation a -> Set.Set (a, a)
edgeSet = R.edgeSet . fromSymmetric

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
adjacencyList :: Eq a => SymmetricRelation a -> [(a, [a])]
adjacencyList = R.adjacencyList . fromSymmetric

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preSet x == { a | aRx }@. In the context of directed
-- graphs, this corresponds to the set of /direct predecessors/ of vertex @x@.
-- Complexity: /O(n + m)/ time and /O(n)/ memory.
--
-- @
-- preSet x 'empty'      == Set.'Set.empty'
-- preSet x ('vertex' x) == Set.'Set.empty'
-- preSet 1 ('edge' 1 2) == Set.'Set.singleton' 2
-- preSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preSet :: Ord a => a -> SymmetricRelation a -> Set.Set a
preSet x = R.preSet x . fromSymmetric

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postSet x == { a | xRa }@. In the context of directed
-- graphs, this corresponds to the set of /direct successors/ of vertex @x@.
-- Complexity: /O(n + m)/ time and /O(n)/ memory.
--
-- @
-- postSet x 'empty'      == Set.'Set.empty'
-- postSet x ('vertex' x) == Set.'Set.empty'
-- postSet x ('edge' x y) == Set.'Set.fromList' [y]
-- postSet 2 ('edge' 1 2) == Set.'Set.singleton' 1
-- @
postSet :: Ord a => a -> SymmetricRelation a -> Set.Set a
postSet x = R.postSet x . fromSymmetric

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edges' [(x,y), (y,x)]
-- path           == path . 'reverse'
-- @
path :: Ord a => [a] -> SymmetricRelation a
path xs = case xs of []     -> empty
                     [x]    -> vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- circuit []        == 'empty'
-- circuit [x]       == 'edge' x x
-- circuit [x,y]     == 'edges' [(x,y), (y,x)]
-- circuit           == circuit . 'reverse'
-- @
circuit :: Ord a => [a] -> SymmetricRelation a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time + /O(m*log(m)) time from computing the symmetricClosure and /O(n + m)/ memory.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edges' [(x,y), (y,x)]
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,x), (y,z), (z,x), (z,y)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique            == clique . 'reverse'
-- @
clique :: Ord a => [a] -> SymmetricRelation a
clique = SymmetricRelation . R.symmetricClosure . R.clique

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time + /O(m*log(m)) time from computing the symmetricClosure and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (y1,x1), (y1,x2), (x2,y1), (x2,y2), (y2,x1), (y2,x2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: Ord a => [a] -> [a] -> SymmetricRelation a
biclique xs = SymmetricRelation . R.symmetricClosure . R.biclique xs

-- TODO: Optimise.
-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edges' [(x,y), (y,x)]
-- star x [y,z] == 'edges' [(x,y), (x,z), (y,x), (y,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: Ord a => a -> [a] -> SymmetricRelation a
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
-- stars [(x, [y])]              == 'edges' [(x,y), (y,x)]
-- stars [(x, ys)]               == 'star' x ys
-- stars                         == 'overlays' . 'map' ('uncurry' 'star')
-- stars . 'adjacencyList'         == id
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: Ord a => [(a, [a])] -> SymmetricRelation a
stars as = SymmetricRelation $ RI.Relation (Set.fromList vs) (Set.fromList es)
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
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (2,1), (3,1), (3,4), (3,5), (4,3), (5,3)]
-- @
tree :: Ord a => Tree.Tree a -> SymmetricRelation a
tree (Node x []) = vertex x
tree (Node x f ) = star x (map rootLabel f)
    `overlay` forest (filter (not . null . subForest) f)

-- | The /forest graph/ constructed from a given 'Tree.Forest' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (2,1), (3,1), (4,5), (5,4)]
-- forest                                                     == 'overlays' . 'map' 'tree'
-- @
forest :: Ord a => Tree.Forest a -> SymmetricRelation a
forest = overlays . map tree

-- | Remove a vertex from a given graph.
-- Complexity: /O(n + m)/ time.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x   == removeVertex x
-- @
removeVertex :: Ord a => a -> SymmetricRelation a -> SymmetricRelation a
removeVertex x = SymmetricRelation . R.removeVertex x . fromSymmetric

-- | Remove an edge from a given graph.
-- Complexity: /O(log(m))/ time.
--
-- @
-- removeEdge x y ('AdjacencyMap.edge' x y)       == 'vertices' [x,y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y == removeEdge y x
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Ord a => a -> a -> SymmetricRelation a -> SymmetricRelation a
removeEdge x y r = let (RI.Relation d rr) = fromSymmetric r
                       in SymmetricRelation $ RI.Relation d (Set.delete (y, x) . Set.delete (x, y) $ rr)

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Ord a => a -> a -> SymmetricRelation a -> SymmetricRelation a
replaceVertex u v = gmap $ \w -> if w == u then v else w

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
mergeVertices :: Ord a => (a -> Bool) -> a -> SymmetricRelation a -> SymmetricRelation a
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
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: Ord b => (a -> b) -> SymmetricRelation a -> SymmetricRelation b
gmap f = SymmetricRelation . R.gmap f . fromSymmetric

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
induce :: (a -> Bool) -> SymmetricRelation a -> SymmetricRelation a
induce p = SymmetricRelation . R.induce p . fromSymmetric

-- | Left-to-right /relational composition/ of graphs: vertices @x@ and @z@ are
-- connected in the resulting graph if there is a vertex @y@, such that @x@ is
-- connected to @y@ in the first graph, and @y@ is connected to @z@ in the
-- second graph. There are no isolated vertices in the result. This operation is
-- associative, has 'empty' and single-'vertex' graphs as /annihilating zeroes/,
-- and distributes over 'overlay'.
-- Complexity: /O(n * m * log(m))/ time and /O(n + m)/ memory.
--
-- @
-- compose 'empty'            x                == 'empty'
-- compose x                'empty'            == 'empty'
-- compose ('vertex' x)       y                == 'empty'
-- compose x                ('vertex' y)       == 'empty'
-- compose x                (compose y z)    == compose (compose x y) z
-- compose x                ('overlay' y z)    == 'overlay' (compose x y) (compose x z)
-- compose ('overlay' x y)    z                == 'overlay' (compose x z) (compose y z)
-- compose ('edge' x y)       ('edge' y z)       == 'edge' x z
-- compose ('path'    [1..5]) ('path'    [1..5]) == 'edges' [(1,3), (2,4), (3,5)]
-- compose ('circuit' [1..5]) ('circuit' [1..5]) == 'circuit' [1,3,5,2,4]
-- @
compose :: Ord a => SymmetricRelation a -> SymmetricRelation a -> SymmetricRelation a
compose x = SymmetricRelation . R.compose (fromSymmetric x) . fromSymmetric
        
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
neighbours :: Ord a => a -> SymmetricRelation a -> Set.Set a
neighbours x = R.postSet x . toRelation
