-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Class
-- Copyright  : (c) Andrey Mokhov 2016-2017
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
-- type are not included. For example, to compute the number of vertices in a
-- 'Graph' expression you will need to use a concrete data type, such as
-- "Algebra.Graph.Fold". Other useful 'Graph' instances are defined in
-- "Algebra.Graph", "Algebra.Graph.AdjacencyMap" and "Algebra.Graph.Relation".
--
-- See "Algebra.Graph.HigherKinded.Class" for the higher-kinded version of the
-- core graph type class.
-----------------------------------------------------------------------------
module Algebra.Graph.Class (
    -- * The core type class
    Graph (..),

    -- * Undirected graphs
    Undirected,

    -- * Reflexive graphs
    Reflexive,

    -- * Transitive graphs
    Transitive,

    -- * Preorders
    Preorder,

    -- * Basic graph construction primitives
    edge, vertices, overlays, connects, edges, graph,

    -- * Relations on graphs
    isSubgraphOf,

    -- * Standard families of graphs
    path, circuit, clique, biclique, star, tree, forest,

    -- * Conversion between graph data types
    ToGraph (..)
  ) where

import Data.Tree

{-|
The core type class for constructing algebraic graphs, characterised by the
following minimal set of axioms. In equations we use @+@ and @*@ as convenient
shortcuts for 'overlay' and 'connect', respectively.

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
class Graph g where
    -- | The type of graph vertices.
    type Vertex g
    -- | Construct the empty graph.
    empty :: g
    -- | Construct the graph with a single vertex.
    vertex :: Vertex g -> g
    -- | Overlay two graphs.
    overlay :: g -> g -> g
    -- | Connect two graphs.
    connect :: g -> g -> g

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

instance Graph () where
    type Vertex () = ()
    empty          = ()
    vertex  _      = ()
    overlay _ _    = ()
    connect _ _    = ()

instance Undirected ()
instance Reflexive  ()
instance Transitive ()
instance Preorder   ()

-- Note: Maybe g and (a -> g) instances are identical and use the Applicative's
-- pure and <*>. We do not provide a general instance for all Applicative
-- functors because that would lead to overlapping instances.
instance Graph g => Graph (Maybe g) where
    type Vertex (Maybe g) = Vertex g
    empty       = pure empty
    vertex      = pure . vertex
    overlay x y = overlay <$> x <*> y
    connect x y = connect <$> x <*> y

instance Undirected g => Undirected (Maybe g)
instance Reflexive  g => Reflexive  (Maybe g)
instance Transitive g => Transitive (Maybe g)
instance Preorder   g => Preorder   (Maybe g)

instance Graph g => Graph (a -> g) where
    type Vertex (a -> g) = Vertex g
    empty       = pure empty
    vertex      = pure . vertex
    overlay x y = overlay <$> x <*> y
    connect x y = connect <$> x <*> y

instance Undirected g => Undirected (a -> g)
instance Reflexive  g => Reflexive  (a -> g)
instance Transitive g => Transitive (a -> g)
instance Preorder   g => Preorder   (a -> g)

instance (Graph g, Graph h) => Graph (g, h) where
    type Vertex (g, h)        = (Vertex g     , Vertex h     )
    empty                     = (empty        , empty        )
    vertex  (x,  y )          = (vertex  x    , vertex  y    )
    overlay (x1, y1) (x2, y2) = (overlay x1 x2, overlay y1 y2)
    connect (x1, y1) (x2, y2) = (connect x1 x2, connect y1 y2)

instance (Undirected g, Undirected h) => Undirected (g, h)
instance (Reflexive  g, Reflexive  h) => Reflexive  (g, h)
instance (Transitive g, Transitive h) => Transitive (g, h)
instance (Preorder   g, Preorder   h) => Preorder   (g, h)

instance (Graph g, Graph h, Graph i) => Graph (g, h, i) where
    type Vertex (g, h, i)             = (Vertex g     , Vertex h     , Vertex i     )
    empty                             = (empty        , empty        , empty        )
    vertex  (x,  y , z )              = (vertex  x    , vertex  y    , vertex  z    )
    overlay (x1, y1, z1) (x2, y2, z2) = (overlay x1 x2, overlay y1 y2, overlay z1 z2)
    connect (x1, y1, z1) (x2, y2, z2) = (connect x1 x2, connect y1 y2, connect z1 z2)

instance (Undirected g, Undirected h, Undirected i) => Undirected (g, h, i)
instance (Reflexive  g, Reflexive  h, Reflexive  i) => Reflexive  (g, h, i)
instance (Transitive g, Transitive h, Transitive i) => Transitive (g, h, i)
instance (Preorder   g, Preorder   h, Preorder   i) => Preorder   (g, h, i)

-- | Construct the graph comprising a single edge.
-- Complexity: /O(1)/ time, memory and size.
--
-- @
-- edge x y == 'connect' ('vertex' x) ('vertex' y)
-- @
edge :: Graph g => Vertex g -> Vertex g -> g
edge x y = connect (vertex x) (vertex y)

-- | Construct the graph comprising a given list of isolated vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- vertices []  == 'empty'
-- vertices [x] == 'vertex' x
-- @
vertices :: Graph g => [Vertex g] -> g
vertices = overlays . map vertex

-- | Construct the graph from a list of edges.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- edges []      == 'empty'
-- edges [(x,y)] == 'edge' x y
-- @
edges :: Graph g => [(Vertex g, Vertex g)] -> g
edges = overlays . map (uncurry edge)

-- | Overlay a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- overlays []    == 'empty'
-- overlays [x]   == x
-- overlays [x,y] == 'overlay' x y
-- @
overlays :: Graph g => [g] -> g
overlays = foldr overlay empty

-- | Connect a given list of graphs.
-- Complexity: /O(L)/ time and memory, and /O(S)/ size, where /L/ is the length
-- of the given list, and /S/ is the sum of sizes of the graphs in the list.
--
-- @
-- connects []    == 'empty'
-- connects [x]   == x
-- connects [x,y] == 'connect' x y
-- @
connects :: Graph g => [g] -> g
connects = foldr connect empty

-- | Construct the graph from given lists of vertices /V/ and edges /E/.
-- The resulting graph contains the vertices /V/ as well as all the vertices
-- referred to by the edges /E/.
-- Complexity: /O(|V| + |E|)/ time, memory and size.
--
-- @
-- graph []  []      == 'empty'
-- graph [x] []      == 'vertex' x
-- graph []  [(x,y)] == 'edge' x y
-- graph vs  es      == 'overlay' ('vertices' vs) ('edges' es)
-- @
graph :: Graph g => [Vertex g] -> [(Vertex g, Vertex g)] -> g
graph vs es = overlay (vertices vs) (edges es)

-- | The 'isSubgraphOf' function takes two graphs and returns 'True' if the
-- first graph is a /subgraph/ of the second. Here is the current implementation:
--
-- @
-- isSubgraphOf x y = 'overlay' x y == y
-- @
-- The complexity therefore depends on the complexity of equality testing of
-- a particular graph instance.
--
-- @
-- isSubgraphOf 'empty'         x             == True
-- isSubgraphOf ('vertex' x)    'empty'         == False
-- isSubgraphOf x             ('overlay' x y) == True
-- isSubgraphOf ('overlay' x y) ('connect' x y) == True
-- isSubgraphOf ('path' xs)     ('circuit' xs)  == True
-- @
isSubgraphOf :: (Graph g, Eq g) => g -> g -> Bool
isSubgraphOf x y = overlay x y == y

-- | The /path/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- path []    == 'empty'
-- path [x]   == 'vertex' x
-- path [x,y] == 'edge' x y
-- @
path :: Graph g => [Vertex g] -> g
path []  = empty
path [x] = vertex x
path xs  = edges $ zip xs (tail xs)

-- | The /circuit/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- circuit []    == 'empty'
-- circuit [x]   == 'edge' x x
-- circuit [x,y] == 'edges' [(x,y), (y,x)]
-- @
circuit :: Graph g => [Vertex g] -> g
circuit []     = empty
circuit (x:xs) = path $ [x] ++ xs ++ [x]

-- | The /clique/ on a list of vertices.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- clique []      == 'empty'
-- clique [x]     == 'vertex' x
-- clique [x,y]   == 'edge' x y
-- clique [x,y,z] == 'edges' [(x,y), (x,z), (y,z)]
-- @
clique :: Graph g => [Vertex g] -> g
clique = connects . map vertex

-- | The /biclique/ on a list of vertices.
-- Complexity: /O(L1 + L2)/ time, memory and size, where /L1/ and /L2/ are the
-- lengths of the given lists.
--
-- @
-- biclique []      []      == 'empty'
-- biclique [x]     []      == 'vertex' x
-- biclique []      [y]     == 'vertex' y
-- biclique [x1,x2] [y1,y2] == 'edges' [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]
-- @
biclique :: Graph g => [Vertex g] -> [Vertex g] -> g
biclique xs ys = connect (vertices xs) (vertices ys)

-- | The /star/ formed by a centre vertex and a list of leaves.
-- Complexity: /O(L)/ time, memory and size, where /L/ is the length of the
-- given list.
--
-- @
-- star x []    == 'vertex' x
-- star x [y]   == 'edge' x y
-- star x [y,z] == 'edges' [(x,y), (x,z)]
-- @
star :: Graph g => Vertex g -> [Vertex g] -> g
star x ys = connect (vertex x) (vertices ys)

-- | The /tree graph/ constructed from a given 'Tree' data structure.
-- Complexity: /O(T)/ time, memory and size, where /T/ is the size of the
-- given tree (i.e. the number of vertices in the tree).
tree :: Graph g => Tree (Vertex g) -> g
tree (Node x f) = overlay (star x $ map rootLabel f) (forest f)

-- | The /forest graph/ constructed from a given 'Forest' data structure.
-- Complexity: /O(F)/ time, memory and size, where /F/ is the size of the
-- given forest (i.e. the number of vertices in the forest).
forest :: Graph g => Forest (Vertex g) -> g
forest = overlays . map tree

-- | The 'ToGraph' type class captures data types that can be converted to
-- polymorphic graph expressions. The conversion method 'toGraph' semantically
-- acts as the identity on graph data structures, but allows to convert graphs
-- between different data representations.
--
-- @
--       toGraph (g     :: 'Algebra.Graph.Graph' a  ) :: 'Algebra.Graph.Graph' a       == g
-- 'show' (toGraph (1 * 2 :: 'Algebra.Graph.Graph' Int) :: 'Algebra.Graph.Relation' Int) == "edge 1 2"
-- @
class ToGraph t where
    type ToVertex t
    toGraph :: (Graph g, Vertex g ~ ToVertex t) => t -> g
