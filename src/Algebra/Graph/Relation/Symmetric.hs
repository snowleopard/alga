-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Symmetric
-- Copyright  : (c) Andrey Mokhov 2016-2020
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
    removeVertex, removeEdge, replaceVertex, mergeVertices, gmap, induce, induceJust,

    -- * Miscellaneous
    consistent

    ) where

import Control.DeepSeq
import Data.Coerce
import Data.Set (Set)
import Data.String
import Data.Tree

import qualified Data.Set as Set

import qualified Algebra.Graph.Relation as R

{-| This data type represents a /symmetric binary relation/ over a set of
elements of type @a@. Symmetric relations satisfy all laws of the
'Algebra.Graph.Class.Undirected' type class, including the commutativity of
'connect':

@'connect' x y == 'connect' y x@

The 'Show' instance lists edge vertices in non-decreasing order:

@show (empty     :: Relation Int) == "empty"
show (1         :: Relation Int) == "vertex 1"
show (1 + 2     :: Relation Int) == "vertices [1,2]"
show (1 * 2     :: Relation Int) == "edge 1 2"
show (2 * 1     :: Relation Int) == "edge 1 2"
show (1 * 2 * 1 :: Relation Int) == "edges [(1,1),(1,2)]"
show (3 * 2 * 1 :: Relation Int) == "edges [(1,2),(1,3),(2,3)]"
show (1 * 2 + 3 :: Relation Int) == "overlay (vertex 3) (edge 1 2)"@

The total order on graphs is defined using /size-lexicographic/ comparison:

* Compare the number of vertices. In case of a tie, continue.
* Compare the sets of vertices. In case of a tie, continue.
* Compare the number of edges. In case of a tie, continue.
* Compare the sets of edges.

Here are a few examples:

@'vertex' 1 < 'vertex' 2
'vertex' 3 < 'edge' 1 2
'vertex' 1 < 'edge' 1 1
'edge' 1 1 < 'edge' 1 2
'edge' 1 2 < 'edge' 1 1 + 'edge' 2 2
'edge' 2 1 < 'edge' 1 3@

@'edge' 1 2 == 'edge' 2 1@

Note that the resulting order refines the 'isSubgraphOf' relation and is
compatible with 'overlay' and 'connect' operations:

@'isSubgraphOf' x y ==> x <= y@

@'empty' <= x
x     <= x + y
x + y <= x * y@
-}
newtype Relation a = SR {
    -- | Extract the underlying symmetric "Algebra.Graph.Relation".
    -- Complexity: /O(1)/ time and memory.
    --
    -- @
    -- fromSymmetric ('edge' 1 2)    == 'R.edges' [(1,2), (2,1)]
    -- 'R.vertexCount' . fromSymmetric == 'vertexCount'
    -- 'R.edgeCount'   . fromSymmetric <= (*2) . 'edgeCount'
    -- @
    fromSymmetric :: R.Relation a
    } deriving (Eq, IsString, NFData)

instance (Ord a, Show a) => Show (Relation a) where
    show = show . toRelation
      where
        toRelation r = R.vertices (vertexList r) `R.overlay` R.edges (edgeList r)

instance Ord a => Ord (Relation a) where
    compare x y = mconcat
        [ compare (vertexCount x) (vertexCount  y)
        , compare (vertexSet   x) (vertexSet    y)
        , compare (edgeCount   x) (edgeCount    y)
        , compare (edgeSet     x) (edgeSet      y) ]

-- | __Note:__ this does not satisfy the usual ring laws; see 'Relation' for
-- more details.
instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

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

-- | Construct the /empty graph/.
--
-- @
-- 'isEmpty'     empty == True
-- 'hasVertex' x empty == False
-- 'vertexCount' empty == 0
-- 'edgeCount'   empty == 0
-- @
empty :: Relation a
empty = coerce R.empty

-- | Construct the graph comprising /a single isolated vertex/.
--
-- @
-- 'isEmpty'     (vertex x) == False
-- 'hasVertex' x (vertex y) == (x == y)
-- 'vertexCount' (vertex x) == 1
-- 'edgeCount'   (vertex x) == 0
-- @
vertex :: a -> Relation a
vertex = coerce R.vertex

-- | Construct the graph comprising /a single edge/.
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
edge x y = SR $ R.edges [(x,y), (y,x)]

-- | /Overlay/ two graphs. This is a commutative, associative and idempotent
-- operation with the identity 'empty'.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- 'isEmpty'     (overlay x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (overlay x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (overlay x y) >= 'vertexCount' x
-- 'vertexCount' (overlay x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (overlay x y) >= 'edgeCount' x
-- 'edgeCount'   (overlay x y) <= 'edgeCount' x   + 'edgeCount' y
-- 'vertexCount' (overlay 1 2) == 2
-- 'edgeCount'   (overlay 1 2) == 0
-- @
overlay :: Ord a => Relation a -> Relation a -> Relation a
overlay = coerce R.overlay

-- | /Connect/ two graphs. This is a commutative and associative operation with
-- the identity 'empty', which distributes over 'overlay' and obeys the
-- decomposition axiom.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory. Note that the
-- number of edges in the resulting graph is quadratic with respect to the number
-- of vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
--
-- @
-- connect x y               == connect y x
-- 'isEmpty'     (connect x y) == 'isEmpty'   x   && 'isEmpty'   y
-- 'hasVertex' z (connect x y) == 'hasVertex' z x || 'hasVertex' z y
-- 'vertexCount' (connect x y) >= 'vertexCount' x
-- 'vertexCount' (connect x y) <= 'vertexCount' x + 'vertexCount' y
-- 'edgeCount'   (connect x y) >= 'edgeCount' x
-- 'edgeCount'   (connect x y) >= 'edgeCount' y
-- 'edgeCount'   (connect x y) >= 'vertexCount' x * 'vertexCount' y \`div\` 2
-- 'vertexCount' (connect 1 2) == 2
-- 'edgeCount'   (connect 1 2) == 1
-- @
connect :: Ord a => Relation a -> Relation a -> Relation a
connect x y = coerce R.connect x y `overlay` biclique (vertexList y) (vertexList x)

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
edges = toSymmetric . R.edges

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
-- hasVertex x ('vertex' y)       == (x == y)
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
-- hasEdge x y                  == 'elem' ('min' x y, 'max' x y) . 'edgeList'
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
edgeCount = Set.size . edgeSet

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
-- edgeList ('edge' x y)     == [('min' x y, 'max' y x)]
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

-- | The set of edges of a given graph, where edge vertices appear in the
-- non-decreasing order.
-- Complexity: /O(m)/ time.
--
-- Note: If you need the set of edges where an edge appears in both directions,
-- use @'R.relation' . 'fromSymmetric'@. The latter is much
-- faster than this function, and takes only /O(1)/ time and memory.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' ('min' x y, 'max' x y)
-- @
edgeSet :: Ord a => Relation a -> Set (a, a)
edgeSet = Set.filter (uncurry (<=)) . R.edgeSet . fromSymmetric

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and memory.
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
path = toSymmetric . R.path

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
circuit = toSymmetric . R.circuit

-- TODO: Optimise by avoiding the call to 'R.symmetricClosure'.
-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
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
clique = toSymmetric . R.clique

-- TODO: Optimise by avoiding the call to 'R.symmetricClosure'.
-- | The /biclique/ on two lists of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,x2), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: Ord a => [a] -> [a] -> Relation a
biclique xs ys = toSymmetric (R.biclique xs ys)

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
star x = toSymmetric . R.star x

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
stars = toSymmetric . R.stars

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
tree = toSymmetric . R.tree

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
forest = toSymmetric . R.forest

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
removeEdge x y = SR . R.removeEdge x y . R.removeEdge y x . fromSymmetric

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
-- constant time.
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
-- Complexity: /O(n + m)/ time, assuming that the predicate takes constant time.
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

-- | Construct the /induced subgraph/ of a given graph by removing the vertices
-- that are 'Nothing'.
-- Complexity: /O(n + m)/ time.
--
-- @
-- induceJust ('vertex' 'Nothing')                               == 'empty'
-- induceJust ('edge' ('Just' x) 'Nothing')                        == 'vertex' x
-- induceJust . 'gmap' 'Just'                                    == 'id'
-- induceJust . 'gmap' (\\x -> if p x then 'Just' x else 'Nothing') == 'induce' p
-- @
induceJust :: Ord a => Relation (Maybe a) -> Relation a
induceJust = coerce R.induceJust

-- | The set of /neighbours/ of an element @x@ is the set of elements that are
-- related to it, i.e. @neighbours x == { a | aRx }@. In the context of undirected
-- graphs, this corresponds to the set of /adjacent/ vertices of vertex @x@.
--
-- @
-- neighbours x 'empty'      == Set.'Set.empty'
-- neighbours x ('vertex' x) == Set.'Set.empty'
-- neighbours x ('edge' x y) == Set.'Set.fromList' [y]
-- neighbours y ('edge' x y) == Set.'Set.fromList' [x]
-- @
neighbours :: Ord a => a -> Relation a -> Set a
neighbours = coerce R.postSet

-- | Check that the internal representation of a symmetric relation is
-- consistent, i.e. that (i) that all edges refer to existing vertices, and (ii)
-- all edges have their symmetric counterparts. It should be impossible to
-- create an inconsistent 'Relation', and we use this function in testing.
--
-- @
-- consistent 'empty'         == True
-- consistent ('vertex' x)    == True
-- consistent ('overlay' x y) == True
-- consistent ('connect' x y) == True
-- consistent ('edge' x y)    == True
-- consistent ('edges' xs)    == True
-- consistent ('stars' xs)    == True
-- @
consistent :: Ord a => Relation a -> Bool
consistent (SR r) = R.consistent r && r == R.transpose r
