-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the 'Relation' data type, as well as associated
-- operations and algorithms. 'Relation' is an instance of the 'C.Graph' type
-- class, which can be used for polymorphic graph construction and manipulation.
-----------------------------------------------------------------------------
module Algebra.Graph.Relation (
    -- * Data structure
    Relation, domain, relation,

    -- * Basic graph construction primitives
    empty, vertex, edge, overlay, connect, vertices, edges, overlays, connects,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, edgeCount, vertexList, edgeList,
    adjacencyList, vertexSet, vertexIntSet, edgeSet, preSet, postSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, stars, starTranspose, tree, forest,

    -- * Graph transformation
    removeVertex, removeEdge, replaceVertex, mergeVertices, transpose, gmap, induce,

    -- * Operations on binary relations
    compose, reflexiveClosure, symmetricClosure, transitiveClosure, preorderClosure
  ) where

import Prelude ()
import Prelude.Compat

import Data.Tree
import Data.Tuple

import Algebra.Graph.Relation.Internal

import qualified Data.IntSet as IntSet
import qualified Data.Set    as Set
import qualified Data.Tree   as Tree

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'hasEdge' x y (edge x y) == True
-- 'edgeCount'   (edge x y) == 1
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Ord a => a -> a -> Relation a
edge x y = Relation (Set.fromList [x, y]) (Set.singleton (x, y))

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
vertices xs = Relation (Set.fromList xs) Set.empty

-- | Construct the graph from a list of edges.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- edges []          == 'empty'
-- edges [(x,y)]     == 'edge' x y
-- 'edgeCount' . edges == 'length' . 'Data.List.nub'
-- @
edges :: Ord a => [(a, a)] -> Relation a
edges es = Relation (Set.fromList $ uncurry (++) $ unzip es) (Set.fromList es)

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
overlays xs = Relation (Set.unions $ map domain xs) (Set.unions $ map relation xs)

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
connects :: Ord a => [Relation a] -> Relation a
connects = foldr connect empty

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf :: Ord a => Relation a -> Relation a -> Bool
isSubgraphOf x y = domain x `Set.isSubsetOf` domain y && relation x `Set.isSubsetOf` relation y

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
isEmpty = null . domain

-- | Check if a graph contains a given vertex.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: Ord a => a -> Relation a -> Bool
hasVertex x = Set.member x . domain

-- | Check if a graph contains a given edge.
-- Complexity: /O(log(n))/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y . 'removeEdge' x y == const False
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: Ord a => a -> a -> Relation a -> Bool
hasEdge x y = Set.member (x, y) . relation

-- | The number of vertices in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: Relation a -> Int
vertexCount = Set.size . domain

-- | The number of edges in a graph.
-- Complexity: /O(1)/ time.
--
-- @
-- edgeCount 'empty'      == 0
-- edgeCount ('vertex' x) == 0
-- edgeCount ('edge' x y) == 1
-- edgeCount            == 'length' . 'edgeList'
-- @
edgeCount :: Relation a -> Int
edgeCount = Set.size . relation

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(n)/ time and memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: Relation a -> [a]
vertexList = Set.toAscList . domain

-- | The sorted list of edges of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('edge' x y)     == [(x,y)]
-- edgeList ('star' 2 [3,1]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- edgeList . 'transpose'    == 'Data.List.sort' . map 'Data.Tuple.swap' . edgeList
-- @
edgeList :: Relation a -> [(a, a)]
edgeList = Set.toAscList . relation

-- | The set of vertices of a given graph.
-- Complexity: /O(1)/ time.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- vertexSet . 'clique'   == Set.'Set.fromList'
-- @
vertexSet :: Relation a -> Set.Set a
vertexSet = domain

-- | The set of vertices of a given graph. Like 'vertexSet' but specialised for
-- graphs with vertices of type 'Int'.
-- Complexity: /O(n)/ time.
--
-- @
-- vertexIntSet 'empty'      == IntSet.'IntSet.empty'
-- vertexIntSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexIntSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexIntSet :: Relation Int -> IntSet.IntSet
vertexIntSet = IntSet.fromAscList . vertexList

-- | The set of edges of a given graph.
-- Complexity: /O(1)/ time.
--
-- @
-- edgeSet 'empty'      == Set.'Set.empty'
-- edgeSet ('vertex' x) == Set.'Set.empty'
-- edgeSet ('edge' x y) == Set.'Set.singleton' (x,y)
-- edgeSet . 'edges'    == Set.'Set.fromList'
-- @
edgeSet :: Relation a -> Set.Set (a, a)
edgeSet = relation

-- | The sorted /adjacency list/ of a graph.
-- Complexity: /O(n + m)/ time and /O(m)/ memory.
--
-- @
-- adjacencyList 'empty'          == []
-- adjacencyList ('vertex' x)     == [(x, [])]
-- adjacencyList ('edge' 1 2)     == [(1, [2]), (2, [])]
-- adjacencyList ('star' 2 [3,1]) == [(1, []), (2, [1,3]), (3, [])]
-- 'stars' . adjacencyList        == id
-- @
adjacencyList :: Eq a => Relation a -> [(a, [a])]
adjacencyList r = go (Set.toAscList $ domain r) (Set.toAscList $ relation r)
  where
    go [] _      = []
    go vs []     = map ((,[])) vs
    go (x:vs) es = let (ys, zs) = span ((==x) . fst) es in (x, map snd ys) : go vs zs

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preSet x == { a | aRx }@. In the context of directed
-- graphs, this corresponds to the set of /direct predecessors/ of vertex @x@.
-- Complexity: /O(n + m)/ time and /O(n)/ memory.
--
-- @
-- preSet x 'empty'      == Set.'Set.empty'
-- preSet x ('vertex' x) == Set.'Set.empty'
-- preSet 1 ('edge' 1 2) == Set.'Set.empty'
-- preSet y ('edge' x y) == Set.'Set.fromList' [x]
-- @
preSet :: Ord a => a -> Relation a -> Set.Set a
preSet x = Set.mapMonotonic fst . Set.filter ((== x) . snd) . relation

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postSet x == { a | xRa }@. In the context of directed
-- graphs, this corresponds to the set of /direct successors/ of vertex @x@.
-- Complexity: /O(n + m)/ time and /O(n)/ memory.
--
-- @
-- postSet x 'empty'      == Set.'Set.empty'
-- postSet x ('vertex' x) == Set.'Set.empty'
-- postSet x ('edge' x y) == Set.'Set.fromList' [y]
-- postSet 2 ('edge' 1 2) == Set.'Set.empty'
-- @
postSet :: Ord a => a -> Relation a -> Set.Set a
postSet x = Set.mapMonotonic snd . Set.filter ((== x) . fst) . relation

-- | The /path/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- path []        == 'empty'
-- path [x]       == 'vertex' x
-- path [x,y]     == 'edge' x y
-- path . 'reverse' == 'transpose' . path
-- @
path :: Ord a => [a] -> Relation a
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
-- circuit . 'reverse' == 'transpose' . circuit
-- @
circuit :: Ord a => [a] -> Relation a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- clique . 'reverse'  == 'transpose' . clique
-- @
clique :: Ord a => [a] -> Relation a
clique xs = Relation (Set.fromList xs) (fst $ go xs)
  where
    go []     = (Set.empty, Set.empty)
    go (x:xs) = (Set.union res (Set.map (x,) set), Set.insert x set)
      where
        (res, set) = go xs

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(n * log(n) + m)/ time and /O(n + m)/ memory.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: Ord a => [a] -> [a] -> Relation a
biclique xs ys = Relation (x `Set.union` y) (x `setProduct` y)
  where
    x = Set.fromList xs
    y = Set.fromList ys

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
-- stars                         == 'overlays' . map (uncurry 'star')
-- stars . 'adjacencyList'         == id
-- 'overlay' (stars xs) (stars ys) == stars (xs ++ ys)
-- @
stars :: Ord a => [(a, [a])] -> Relation a
stars as = Relation (Set.fromList vs) (Set.fromList es)
  where
    vs = concatMap (uncurry (:)) as
    es = [ (x, y) | (x, ys) <- as, y <- ys ]

-- | The /star transpose/ formed by a list of leaves connected to a centre vertex.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- starTranspose x []    == 'vertex' x
-- starTranspose x [y]   == 'edge' y x
-- starTranspose x [y,z] == 'edges' [(y,x), (z,x)]
-- starTranspose x ys    == 'connect' ('vertices' ys) ('vertex' x)
-- starTranspose x ys    == 'transpose' ('star' x ys)
-- @
starTranspose :: Ord a => a -> [a] -> Relation a
starTranspose x [] = vertex x
starTranspose x ys = connect (vertices ys) (vertex x)

-- | The /tree graph/ constructed from a given 'Tree.Tree' data structure.
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Ord a => Tree.Tree a -> Relation a
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
-- forest                                                     == 'overlays' . map 'tree'
-- @
forest :: Ord a => Tree.Forest a -> Relation a
forest = overlays. map tree

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
removeVertex x (Relation d r) = Relation (Set.delete x d) (Set.filter notx r)
  where
    notx (a, b) = a /= x && b /= x

-- | Remove an edge from a given graph.
-- Complexity: /O(log(m))/ time.
--
-- @
-- removeEdge x y ('AdjacencyMap.edge' x y)       == 'vertices' [x, y]
-- removeEdge x y . removeEdge x y == removeEdge x y
-- removeEdge x y . 'removeVertex' x == 'removeVertex' x
-- removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2
-- removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2
-- @
removeEdge :: Ord a => a -> a -> Relation a -> Relation a
removeEdge x y (Relation d r) = Relation d (Set.delete (x, y) r)

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'AdjacencyMap'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O((n + m) * log(n))/ time.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: Ord a => a -> a -> Relation a -> Relation a
replaceVertex u v = gmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O((n + m) * log(n))/ time, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: Ord a => (a -> Bool) -> a -> Relation a -> Relation a
mergeVertices p v = gmap $ \u -> if p u then v else u

-- | Transpose a given graph.
-- Complexity: /O(m * log(m))/ time.
--
-- @
-- transpose 'empty'       == 'empty'
-- transpose ('vertex' x)  == 'vertex' x
-- transpose ('edge' x y)  == 'edge' y x
-- transpose . transpose == id
-- 'edgeList' . transpose  == 'Data.List.sort' . map 'Data.Tuple.swap' . 'edgeList'
-- @
transpose :: Ord a => Relation a -> Relation a
transpose (Relation d r) = Relation d (Set.map swap r)

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
gmap f (Relation d r) = Relation (Set.map f d) (Set.map (\(x, y) -> (f x, f y)) r)

-- | Construct the /induced subgraph/ of a given graph by removing the
-- vertices that do not satisfy a given predicate.
-- Complexity: /O(m)/ time, assuming that the predicate takes /O(1)/ to
-- be evaluated.
--
-- @
-- induce (const True ) x      == x
-- induce (const False) x      == 'empty'
-- induce (/= x)               == 'removeVertex' x
-- induce p . induce q         == induce (\\x -> p x && q x)
-- 'isSubgraphOf' (induce p x) x == True
-- @
induce :: (a -> Bool) -> Relation a -> Relation a
induce p (Relation d r) = Relation (Set.filter p d) (Set.filter pp r)
  where
    pp (x, y) = p x && p y

-- | /Compose/ two relations: @R = 'compose' Q P@. Two elements @x@ and @y@ are
-- related in the resulting relation, i.e. @xRy@, if there exists an element @z@,
-- such that @xPz@ and @zQy@. This is an associative operation which has 'empty'
-- as the /annihilating zero/.
-- Complexity: /O(n * m * log(m))/ time and /O(n + m)/ memory.
--
-- @
-- compose 'empty'            x                == 'empty'
-- compose x                'empty'            == 'empty'
-- compose x                (compose y z)    == compose (compose x y) z
-- compose ('edge' y z)       ('edge' x y)       == 'edge' x z
-- compose ('path'    [1..5]) ('path'    [1..5]) == 'edges' [(1,3),(2,4),(3,5)]
-- compose ('circuit' [1..5]) ('circuit' [1..5]) == 'circuit' [1,3,5,2,4]
-- @
compose :: Ord a => Relation a -> Relation a -> Relation a
compose x y = Relation (referredToVertexSet r) r
  where
    d = domain x `Set.union` domain y
    r = Set.unions [ preSet z y `setProduct` postSet z x | z <- Set.toAscList d ]

-- | Compute the /reflexive closure/ of a 'Relation'.
-- Complexity: /O(n * log(m))/ time.
--
-- @
-- reflexiveClosure 'empty'      == 'empty'
-- reflexiveClosure ('vertex' x) == 'edge' x x
-- @
reflexiveClosure :: Ord a => Relation a -> Relation a
reflexiveClosure (Relation d r) =
    Relation d $ r `Set.union` Set.fromDistinctAscList [ (a, a) | a <- Set.toAscList d ]

-- | Compute the /symmetric closure/ of a 'Relation'.
-- Complexity: /O(m * log(m))/ time.
--
-- @
-- symmetricClosure 'empty'      == 'empty'
-- symmetricClosure ('vertex' x) == 'vertex' x
-- symmetricClosure ('edge' x y) == 'edges' [(x, y), (y, x)]
-- @
symmetricClosure :: Ord a => Relation a -> Relation a
symmetricClosure (Relation d r) = Relation d $ r `Set.union` Set.map swap r

-- | Compute the /transitive closure/ of a 'Relation'.
-- Complexity: /O(n * m * log(n) * log(m))/ time.
--
-- @
-- transitiveClosure 'empty'           == 'empty'
-- transitiveClosure ('vertex' x)      == 'vertex' x
-- transitiveClosure ('path' $ 'Data.List.nub' xs) == 'clique' ('Data.List.nub' xs)
-- @
transitiveClosure :: Ord a => Relation a -> Relation a
transitiveClosure old
    | old == new = old
    | otherwise  = transitiveClosure new
  where
    new = overlay old (old `compose` old)

-- | Compute the /preorder closure/ of a 'Relation'.
-- Complexity: /O(n * m * log(m))/ time.
--
-- @
-- preorderClosure 'empty'           == 'empty'
-- preorderClosure ('vertex' x)      == 'edge' x x
-- preorderClosure ('path' $ 'Data.List.nub' xs) == 'reflexiveClosure' ('clique' $ 'Data.List.nub' xs)
-- @
preorderClosure :: Ord a => Relation a -> Relation a
preorderClosure = reflexiveClosure . transitiveClosure
