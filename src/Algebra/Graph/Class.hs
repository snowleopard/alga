-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Class
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Common classes of algebraic graphs.
--
-- See "Algebra.Graph.HigherKinded.Class" for alternative definitions where
-- the type classes are higher-kinded.
-----------------------------------------------------------------------------
module Algebra.Graph.Class (
    -- * The core type class
    Graph (..), ToGraph (..),

    -- * Undirected graphs
    Undirected,

    -- * Reflexive graphs
    Reflexive,

    -- * Transitive graphs
    Transitive,

    -- * Preorders
    Preorder
  ) where

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
'Undirected', reflexive and transitive graphs can be obtained by extending the
minimal set of axioms.
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
all self-loops resulting in an /irreflexive graph/. We therefore also use this
type class to represent irreflexive graphs.
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
