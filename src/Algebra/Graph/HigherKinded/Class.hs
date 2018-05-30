{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.HigherKinded.Class
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines the core type class 'Graph', a few graph subclasses, and
-- basic polymorphic graph construction primitives. Functions that cannot be
-- implemented fully polymorphically and require the use of an intermediate data
-- type are not included. For example, to compute the size of a 'Graph'
-- expression you will need to use a concrete data type, such as "Algebra.Graph".
--
-- See "Algebra.Graph.Class" for alternative definitions where the core type
-- class is not higher-kinded and permits more instances.
-----------------------------------------------------------------------------
module Algebra.Graph.HigherKinded.Class (
    -- * The core type class
    Graph (..), empty, vertex, overlay,

    -- * Undirected graphs
    Undirected,

    -- * Reflexive graphs
    Reflexive,

    -- * Transitive graphs
    Transitive,

    -- * Preorders
    Preorder,

    -- * Basic graph construction primitives
    edge, vertices, edges, overlays, connects,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Graph properties
    isEmpty, hasVertex, hasEdge, vertexCount, vertexList, vertexSet, vertexIntSet,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, starTranspose, tree, forest, mesh,
    torus, deBruijn,

    -- * Graph transformation
    removeVertex, replaceVertex, mergeVertices, splitVertex,

    -- * Graph composition
    box,

    -- * Conversion between graph data types
    ToGraph (..)

  ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Monad.Compat (MonadPlus, msum, mfilter)
import Data.Foldable (toList)
import Data.Tree

import qualified Data.IntSet as IntSet
import qualified Data.Set    as Set

{-|
The core type class for constructing algebraic graphs is defined by introducing
the 'connect' method to the standard 'MonadPlus' class and reusing the following
existing methods:

* The 'empty' method comes from the 'Control.Applicative.Alternative' class and
corresponds to the /empty graph/. This module simply re-exports it.

* The 'vertex' graph construction primitive is an alias for 'pure' of the
'Applicative' type class.

* Graph 'overlay' is an alias for 'mplus' of the 'MonadPlus' type class.

The 'Graph' type class is characterised by the following minimal set of axioms.
In equations we use @+@ and @*@ as convenient shortcuts for 'overlay' and
'connect', respectively.

    * 'overlay' is commutative and associative:

        >       x + y == y + x
        > x + (y + z) == (x + y) + z

    * 'connect' is associative and has 'empty' as the identity:

        >   x * empty == x
        >   empty * x == x
        > x * (y * z) == (x * y) * z

    * 'connect' distributes over 'overlay':

        > x * (y + z) == x * y + x * z
        > (x + y) * z == x * z + y * z

    * 'connect' can be decomposed:

        > x * y * z == x * y + x * z + y * z

The following useful theorems can be proved from the above set of axioms.

    * 'overlay' has 'empty' as the identity and is idempotent:

        >   x + empty == x
        >   empty + x == x
        >       x + x == x

    * Absorption and saturation of 'connect':

        > x * y + x + y == x * y
        >     x * x * x == x * x

The core type class 'Graph' corresponds to unlabelled directed graphs.
'Undirected', 'Reflexive', 'Transitive' and 'Preorder' graphs can be obtained
by extending the minimal set of axioms.

When specifying the time and memory complexity of graph algorithms, /n/ will
denote the number of vertices in the graph, /m/ will denote the number of
edges in the graph, and /s/ will denote the /size/ of the corresponding
'Graph' expression.
-}
class (Traversable g,
#if !MIN_VERSION_base(4,8,0)
  Alternative g,
#endif
  MonadPlus g) => Graph g where
    -- | Connect two graphs.
    connect :: g a -> g a -> g a

    -- | Construct the /induced subgraph/ of a given graph by removing the
    -- vertices that do not satisfy a given predicate.
    -- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
    -- /O(1)/ to be evaluated.
    --
    -- A default version is implemented using the 'MonadPlus' instance, but it can
    -- be improved by the implementation
    --
    -- @
    -- induce (const True ) x      == x
    -- induce (const False) x      == 'empty'
    -- induce (/= x)               == 'removeVertex' x
    -- induce p . induce q         == induce (\\x -> p x && q x)
    -- 'isSubgraphOf' (induce p x) x == True
    -- @
    induce :: (a -> Bool) -> g a -> g a
    induce = mfilter

-- | Construct the graph comprising a single isolated vertex. An alias for 'pure'.
vertex :: Graph g => a -> g a
vertex = pure

-- | Overlay two graphs. An alias for '<|>'.
overlay :: Graph g => g a -> g a -> g a
overlay = (<|>)

{-|
The class of /undirected graphs/ that satisfy the following additional axiom.

    * 'connect' is commutative:

        > x * y == y * x
-}
class Graph g => Undirected g

{-|
The class of /reflexive graphs/ that satisfy the following additional axiom.

    * Each vertex has a /self-loop/:

        > vertex x == vertex x * vertex x

    Or, alternatively, if we remember that 'vertex' is an alias for 'pure':

        > pure x == pure x * pure x

Note that by applying the axiom in the reverse direction, one can always remove
all self-loops resulting in an /irreflexive graph/. This type class can
therefore be also used in the context of irreflexive graphs.
-}
class Graph g => Reflexive g

{-|
The class of /transitive graphs/ that satisfy the following additional axiom.

    * The /closure/ axiom: graphs with equal transitive closures are equal.

        > y /= empty ==> x * y + x * z + y * z == x * y + y * z

By repeated application of the axiom one can turn any graph into its transitive
closure or transitive reduction.
-}
class Graph g => Transitive g

{-|
The class of /preorder graphs/ that are both reflexive and transitive.
-}
class (Reflexive g, Transitive g) => Preorder g

-- | Construct the graph comprising a single edge.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y               == 'connect' ('vertex' x) ('vertex' y)
-- 'vertexCount' (edge 1 1) == 1
-- 'vertexCount' (edge 1 2) == 2
-- @
edge :: Graph g => a -> a -> g a
edge x y = connect (vertex x) (vertex y)

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices []            == 'empty'
-- vertices [x]           == 'vertex' x
-- 'hasVertex' x . vertices == 'elem' x
-- 'vertexCount' . vertices == 'length' . 'Data.List.nub'
-- 'vertexSet'   . vertices == Set.'Set.fromList'
-- @
vertices :: Graph g => [a] -> g a
vertices = overlays . map vertex

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []      == 'empty'
-- edges [(x,y)] == 'edge' x y
-- @
edges :: Graph g => [(a, a)] -> g a
edges = overlays . map (uncurry edge)

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []        == 'empty'
-- overlays [x]       == x
-- overlays [x,y]     == 'overlay' x y
-- overlays           == 'foldr' 'overlay' 'empty'
-- 'isEmpty' . overlays == 'all' 'isEmpty'
-- @
overlays :: Graph g => [g a] -> g a
overlays []     = empty
overlays [x]    = x
overlays (x:xs) = x `overlay` overlays xs

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- connects []        == 'empty'
-- connects [x]       == x
-- connects [x,y]     == 'connect' x y
-- connects           == 'foldr' 'connect' 'empty'
-- 'isEmpty' . connects == 'all' 'isEmpty'
-- @
connects :: Graph g => [g a] -> g a
connects []     = empty
connects [x]    = x
connects (x:xs) = x `connect` connects xs

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second. Here is the current implementation:
--
-- @
-- isSubgraphOf x y = 'overlay' x y == y
-- @
-- The complexity therefore depends on the complexity of equality testing of
-- the specific graph instance.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf :: (Graph g, Eq (g a)) => g a -> g a -> Bool
isSubgraphOf x y = overlay x y == y

-- | Check if a graph is empty. A convenient alias for 'null'.
-- Complexity: /O(s)/ time.
--
-- @
-- isEmpty 'empty'                       == True
-- isEmpty ('overlay' 'empty' 'empty')       == True
-- isEmpty ('vertex' x)                  == False
-- isEmpty ('removeVertex' x $ 'vertex' x) == True
-- @
isEmpty :: Graph g => g a -> Bool
isEmpty = null

-- | Check if a graph contains a given vertex. A convenient alias for `elem`.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- hasVertex x . 'removeVertex' x == const False
-- @
hasVertex :: (Eq a, Graph g) => a -> g a -> Bool
hasVertex = elem

-- | Check if a graph contains a given edge.
-- Complexity: /O(s)/ time.
--
-- @
-- hasEdge x y 'empty'            == False
-- hasEdge x y ('vertex' z)       == False
-- hasEdge x y ('edge' x y)       == True
-- hasEdge x y                  == 'elem' (x,y) . 'edgeList'
-- @
hasEdge :: (Eq (g a), Graph g, Ord a) => a -> a -> g a -> Bool
hasEdge u v = (edge u v `isSubgraphOf`) . induce (`elem` [u, v])

-- | The number of vertices in a graph.
-- Complexity: /O(s * log(n))/ time.
--
-- @
-- vertexCount 'empty'      == 0
-- vertexCount ('vertex' x) == 1
-- vertexCount            == 'length' . 'vertexList'
-- @
vertexCount :: (Ord a, Graph g) => g a -> Int
vertexCount = length . vertexList

-- | The sorted list of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexList 'empty'      == []
-- vertexList ('vertex' x) == [x]
-- vertexList . 'vertices' == 'Data.List.nub' . 'Data.List.sort'
-- @
vertexList :: (Ord a, Graph g) => g a -> [a]
vertexList = Set.toAscList . vertexSet

-- | The set of vertices of a given graph.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexSet 'empty'      == Set.'Set.empty'
-- vertexSet . 'vertex'   == Set.'Set.singleton'
-- vertexSet . 'vertices' == Set.'Set.fromList'
-- vertexSet . 'clique'   == Set.'Set.fromList'
-- @
vertexSet :: (Ord a, Graph g) => g a -> Set.Set a
vertexSet = foldr Set.insert Set.empty

-- | The set of vertices of a given graph. Like 'vertexSet' but specialised for
-- graphs with vertices of type 'Int'.
-- Complexity: /O(s * log(n))/ time and /O(n)/ memory.
--
-- @
-- vertexIntSet 'empty'      == IntSet.'IntSet.empty'
-- vertexIntSet . 'vertex'   == IntSet.'IntSet.singleton'
-- vertexIntSet . 'vertices' == IntSet.'IntSet.fromList'
-- vertexIntSet . 'clique'   == IntSet.'IntSet.fromList'
-- @
vertexIntSet :: Graph g => g Int -> IntSet.IntSet
vertexIntSet = foldr IntSet.insert IntSet.empty

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'vertex' x
-- path [x,y] == 'edge' x y
-- @
path :: Graph g => [a] -> g a
path xs = case xs of []     -> empty
                     [x]    -> vertex x
                     (_:ys) -> edges (zip xs ys)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'edge' x x
-- circuit [x,y] == 'edges' [(x,y), (y,x)]
-- @
circuit :: Graph g => [a] -> g a
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique []         == 'empty'
-- clique [x]        == 'vertex' x
-- clique [x,y]      == 'edge' x y
-- clique [x,y,z]    == 'edges' [(x,y), (x,z), (y,z)]
-- clique (xs ++ ys) == 'connect' (clique xs) (clique ys)
-- @
clique :: Graph g => [a] -> g a
clique = connects . map vertex

-- | The /biclique/ on two lists of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- biclique xs      ys      == 'connect' ('vertices' xs) ('vertices' ys)
-- @
biclique :: Graph g => [a] -> [a] -> g a
biclique xs [] = vertices xs
biclique [] ys = vertices ys
biclique xs ys = connect (vertices xs) (vertices ys)

-- | The /star/ formed by a centre vertex connected to a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- star x ys    == 'connect' ('vertex' x) ('vertices' ys)
-- @
star :: Graph g => a -> [a] -> g a
star x [] = vertex x
star x ys = connect (vertex x) (vertices ys)

-- | The /star transpose/ formed by a list of leaves connected to a centre vertex.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- starTranspose x []    == 'vertex' x
-- starTranspose x [y]   == 'edge' y x
-- starTranspose x [y,z] == 'edges' [(y,x), (z,x)]
-- starTranspose x ys    == 'connect' ('vertices' ys) ('vertex' x)
-- starTranspose x ys    == transpose ('star' x ys)
-- @
starTranspose :: Graph g => a -> [a] -> g a
starTranspose x [] = vertex x
starTranspose x ys = connect (vertices ys) (vertex x)

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree (i.e. the number of vertices in the tree).
--
-- @
-- tree (Node x [])                                         == 'vertex' x
-- tree (Node x [Node y [Node z []]])                       == 'path' [x,y,z]
-- tree (Node x [Node y [], Node z []])                     == 'star' x [y,z]
-- tree (Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]) == 'edges' [(1,2), (1,3), (3,4), (3,5)]
-- @
tree :: Graph g => Tree a -> g a
tree (Node x []) = vertex x
tree (Node x f ) = star x (map rootLabel f)
         `overlay` forest (filter (not . null . subForest) f)

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest (i.e. the number of vertices in the forest).
--
-- @
-- forest []                                                  == 'empty'
-- forest [x]                                                 == 'tree' x
-- forest [Node 1 [Node 2 [], Node 3 []], Node 4 [Node 5 []]] == 'edges' [(1,2), (1,3), (4,5)]
-- forest                                                     == 'overlays' . map 'tree'
-- @
forest :: Graph g => Forest a -> g a
forest = overlays . map tree

-- | Construct a /mesh graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- mesh xs     []   == 'empty'
-- mesh []     ys   == 'empty'
-- mesh [x]    [y]  == 'vertex' (x, y)
-- mesh xs     ys   == 'box' ('path' xs) ('path' ys)
-- mesh [1..3] "ab" == 'edges' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\')), ((1,\'b\'),(2,\'b\')), ((2,\'a\'),(2,\'b\'))
--                           , ((2,\'a\'),(3,\'a\')), ((2,\'b\'),(3,\'b\')), ((3,\'a\'),(3,\'b\')) ]
-- @
mesh :: Graph g => [a] -> [b] -> g (a, b)
mesh xs ys = path xs `box` path ys

-- | Construct a /torus graph/ from two lists of vertices.
-- Complexity: /O(L1 * L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- torus xs    []   == 'empty'
-- torus []    ys   == 'empty'
-- torus [x]   [y]  == 'edge' (x, y) (x, y)
-- torus xs    ys   == 'box' ('circuit' xs) ('circuit' ys)
-- torus [1,2] "ab" == 'edges' [ ((1,\'a\'),(1,\'b\')), ((1,\'a\'),(2,\'a\')), ((1,\'b\'),(1,\'a\')), ((1,\'b\'),(2,\'b\'))
--                           , ((2,\'a\'),(1,\'a\')), ((2,\'a\'),(2,\'b\')), ((2,\'b\'),(1,\'b\')), ((2,\'b\'),(2,\'a\')) ]
-- @
torus :: Graph g => [a] -> [b] -> g (a, b)
torus xs ys = circuit xs `box` circuit ys

-- | Construct a /De Bruijn graph/ of a given non-negative dimension using symbols
-- from a given alphabet.
-- Complexity: /O(A^(D + 1))/ time, memory and size, where /A/ is the size of the
-- alphabet and /D/ is the dimension of the graph.
--
-- @
--           deBruijn 0 xs               == 'edge' [] []
-- n > 0 ==> deBruijn n []               == 'empty'
--           deBruijn 1 [0,1]            == 'edges' [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]
--           deBruijn 2 "0"              == 'edge' "00" "00"
--           deBruijn 2 "01"             == 'edges' [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
--                                                , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]
--           transpose   (deBruijn n xs) == 'fmap' 'reverse' $ deBruijn n xs
--           'vertexCount' (deBruijn n xs) == ('length' $ 'Data.List.nub' xs)^n
-- n > 0 ==> 'edgeCount'   (deBruijn n xs) == ('length' $ 'Data.List.nub' xs)^(n + 1)
-- @
deBruijn :: Graph g => Int -> [a] -> g [a]
deBruijn 0   _        = edge [] []
deBruijn len alphabet = skeleton >>= expand
  where
    overlaps = mapM (const alphabet) [2..len]
    skeleton = edges    [        (Left s, Right s)   | s <- overlaps ]
    expand v = vertices [ either ([a] ++) (++ [a]) v | a <- alphabet ]

-- | Remove a vertex from a given graph.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- removeVertex x ('vertex' x)       == 'empty'
-- removeVertex 1 ('vertex' 2)       == 'vertex' 2
-- removeVertex x ('edge' x x)       == 'empty'
-- removeVertex 1 ('edge' 1 2)       == 'vertex' 2
-- removeVertex x . removeVertex x == removeVertex x
-- @
removeVertex :: (Eq a, Graph g) => a -> g a -> g a
removeVertex v = induce (/= v)

-- | The function @'replaceVertex' x y@ replaces vertex @x@ with vertex @y@ in a
-- given 'Graph'. If @y@ already exists, @x@ and @y@ will be merged.
-- Complexity: /O(s)/ time, memory and size.
--
-- @
-- replaceVertex x x            == id
-- replaceVertex x y ('vertex' x) == 'vertex' y
-- replaceVertex x y            == 'mergeVertices' (== x) y
-- @
replaceVertex :: (Eq a, Graph g) => a -> a -> g a -> g a
replaceVertex u v = fmap $ \w -> if w == u then v else w

-- | Merge vertices satisfying a given predicate into a given vertex.
-- Complexity: /O(s)/ time, memory and size, assuming that the predicate takes
-- /O(1)/ to be evaluated.
--
-- @
-- mergeVertices (const False) x    == id
-- mergeVertices (== x) y           == 'replaceVertex' x y
-- mergeVertices even 1 (0 * 2)     == 1 * 1
-- mergeVertices odd  1 (3 + 4 * 5) == 4 * 1
-- @
mergeVertices :: Graph g => (a -> Bool) -> a -> g a -> g a
mergeVertices p v = fmap $ \w -> if p w then v else w

-- | Split a vertex into a list of vertices with the same connectivity.
-- Complexity: /O(s + k * L)/ time, memory and size, where /k/ is the number of
-- occurrences of the vertex in the expression and /L/ is the length of the
-- given list.
--
-- @
-- splitVertex x []                  == 'removeVertex' x
-- splitVertex x [x]                 == id
-- splitVertex x [y]                 == 'replaceVertex' x y
-- splitVertex 1 [0,1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)
-- @
splitVertex :: (Eq a, Graph g) => a -> [a] -> g a -> g a
splitVertex v us g = g >>= \w -> if w == v then vertices us else vertex w

-- | Compute the /Cartesian product/ of graphs.
-- Complexity: /O(s1 * s2)/ time, memory and size, where /s1/ and /s2/ are the
-- sizes of the given graphs.
--
-- @
-- box ('path' [0,1]) ('path' "ab") == 'edges' [ ((0,\'a\'), (0,\'b\'))
--                                       , ((0,\'a\'), (1,\'a\'))
--                                       , ((0,\'b\'), (1,\'b\'))
--                                       , ((1,\'a\'), (1,\'b\')) ]
-- @
-- Up to an isomorphism between the resulting vertex types, this operation
-- is /commutative/, /associative/, /distributes/ over 'overlay', has singleton
-- graphs as /identities/ and 'empty' as the /annihilating zero/. Below @~~@
-- stands for the equality up to an isomorphism, e.g. @(x, ()) ~~ x@.
--
-- @
-- box x y               ~~ box y x
-- box x (box y z)       ~~ box (box x y) z
-- box x ('overlay' y z)   == 'overlay' (box x y) (box x z)
-- box x ('vertex' ())     ~~ x
-- box x 'empty'           ~~ 'empty'
-- 'vertexCount' (box x y) == 'vertexCount' x * 'vertexCount' y
-- 'edgeCount'   (box x y) <= 'vertexCount' x * 'edgeCount' y + 'edgeCount' x * 'vertexCount' y
-- @
box :: Graph g => g a -> g b -> g (a, b)
box x y = msum $ xs ++ ys
  where
    xs = map (\b -> fmap (,b) x) $ toList y
    ys = map (\a -> fmap (a,) y) $ toList x

-- | The 'ToGraph' type class captures data types that can be converted to
-- polymorphic graph expressions. The conversion method 'toGraph' semantically
-- acts as the identity on graph data structures, but allows to convert graphs
-- between different data representations.
--
-- @
--       toGraph (g     :: 'Algebra.Graph.Graph' a  ) :: 'Algebra.Graph.Graph' a   == g
-- 'show' (toGraph (1 * 2 :: 'Algebra.Graph.Graph' Int) :: 'Algebra.Graph.Fold' Int) == "edge 1 2"
-- @
class ToGraph t where
    toGraph :: Graph g => t a -> g a
