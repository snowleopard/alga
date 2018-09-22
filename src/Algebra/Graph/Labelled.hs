{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module provides a minimal and experimental implementation of algebraic
-- graphs with edge labels. The API will be expanded in the next release.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled (
    -- * Algebraic data type for edge-labeleld graphs
    Graph (..), UnlabelledGraph, empty, vertex, edge, overlay, connect,
    connectBy, (-<), (>-),

    -- * Operations
    edgeLabel
  ) where

import Prelude ()
import Prelude.Compat

import Algebra.Graph.Label
import qualified Algebra.Graph.Class as C

-- | Edge-labelled graphs, where the type variable @e@ stands for edge labels.
-- For example, @Graph Bool a@ is isomorphic to unlabelled graphs defined in
-- the top-level module "Algebra.Graph.Graph", where @False@ and @True@ denote
-- the lack of and the existence of an unlabelled edge, respectively.
data Graph e a = Empty
               | Vertex a
               | Connect e (Graph e a) (Graph e a)
               deriving (Functor, Show)

-- | A type synonym for unlabelled graphs.
type UnlabelledGraph a = Graph Bool a

-- | Construct the /empty graph/. An alias for the constructor 'Empty'.
-- Complexity: /O(1)/ time, memory and size.
empty :: Graph e a
empty = Empty

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
-- Complexity: /O(1)/ time, memory and size.
vertex :: a -> Graph e a
vertex = Vertex

-- | Construct the graph comprising /a single edge/ with the label 'one'.
-- Complexity: /O(1)/ time, memory and size.
edge :: Dioid e => a -> a -> Graph e a
edge = C.edge

-- | /Overlay/ two graphs. An alias for 'Connect' 'zero'. This is a commutative,
-- associative and idempotent operation with the identity 'empty'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size.
overlay :: Semilattice e => Graph e a -> Graph e a -> Graph e a
overlay = Connect zero

-- | /Connect/ two graphs. An alias for 'Connect' 'one'. This is an associative
-- operation with the identity 'empty', which distributes over 'overlay' and
-- obeys the decomposition axiom. See the full list of laws in "Algebra.Graph".
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
connect :: Dioid e => Graph e a -> Graph e a -> Graph e a
connect = Connect one

-- | /Connect/ two graphs with edges labelled by a given label. An alias for
-- 'Connect'.
-- Complexity: /O(1)/ time and memory, /O(s1 + s2)/ size. Note that the number
-- of edges in the resulting graph is quadratic with respect to the number of
-- vertices of the arguments: /m = O(m1 + m2 + n1 * n2)/.
connectBy :: e -> Graph e a -> Graph e a -> Graph e a
connectBy = Connect

-- | The left-hand part of a convenient ternary-ish operator @x -\<e\>- y@ for
-- connecting graphs with labelled edges. For example:
--
-- @
-- x = 'vertex' "x"
-- y = 'vertex' "y"
-- z = x -\<2\>- y
-- @
(-<) :: Graph e a -> e -> (Graph e a, e)
g -< e = (g, e)

-- | The right-hand part of a convenient ternary-ish operator @x -\<e\>- y@ for
-- connecting graphs with labelled edges. For example:
--
-- @
-- x = 'vertex' "x"
-- y = 'vertex' "y"
-- z = x -\<2\>- y
-- @
(>-) :: (Graph e a, e) -> Graph e a -> Graph e a
(g, e) >- h = Connect e g h

infixl 5 -<
infixl 5 >-

instance Dioid e => C.Graph (Graph e a) where
    type Vertex (Graph e a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = overlay
    connect = connect

foldgUnDiff :: b -> (a -> b) -> (b -> b -> b) ->  Graph e a -> b
foldgUnDiff e v o = go
  where
    go Empty = e
    go (Vertex a) = v a
    go (Connect _ g h) = o (go g) (go h)

-- | Check if a graph contains a given vertex. A convenient alias for `elem`.
-- Complexity: /O(s)/ time.
--
-- @
-- hasVertex x 'empty'            == False
-- hasVertex x ('vertex' x)       == True
-- hasVertex 1 ('vertex' 2)       == False
-- @
hasVertex :: Eq a => a -> Graph e a -> Bool
hasVertex v = foldgUnDiff False ((==) v) (||)

-- | Extract the label of a specified edge from a graph.
edgeLabel :: (Eq a, Semilattice e) => a -> a -> Graph e a -> e
edgeLabel _ _ Empty           = zero
edgeLabel _ _ (Vertex _)      = zero
edgeLabel x y (Connect e g h) = edgeLabel x y g \/ edgeLabel x y h \/ new
  where
    new | x `hasVertex` g && y `hasVertex` h = e
        | otherwise                = zero
