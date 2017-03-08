-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.HigherKinded.Classes
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Common classes of algebraic graphs. This module defines higher-kinded
-- equivalents for the classes defined in "Algebra.Graph.Classes".
--
-----------------------------------------------------------------------------
module Algebra.Graph.HigherKinded.Classes (
    -- * The core type class
    Graph (..), vertex,

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
class Monad g => Graph g where
    -- | Construct the empty graph.
    empty :: g a
    -- | Overlay two graphs.
    overlay :: g a -> g a -> g a
    -- | Connect two graphs.
    connect :: g a -> g a -> g a

-- | Construct the graph comprising a single isolated vertex. An alias for 'pure'.
vertex :: Graph g => a -> g a
vertex = pure

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
