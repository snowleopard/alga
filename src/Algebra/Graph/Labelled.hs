{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TypeFamilies #-}
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
-- This module defines edge-labelled graphs.
--
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
data Graph e a = Empty
               | Vertex a
               | Connect e (Graph e a) (Graph e a)
               deriving (Foldable, Functor, Show, Traversable)

-- | Construct the /empty graph/. An alias for the constructor 'Empty'.
-- Complexity: /O(1)/ time, memory and size.
empty :: Graph e a
empty = Empty

-- | Construct the graph comprising /a single isolated vertex/. An alias for the
-- constructor 'Vertex'.
vertex :: a -> Graph e a
vertex = Vertex

-- | Construct the graph comprising /a single edge/.
-- Complexity: /O(1)/ time, memory and size.
edge :: Dioid e => a -> a -> Graph e a
edge = C.edge

overlay :: Semilattice e => Graph e a -> Graph e a -> Graph e a
overlay = Connect zero

connect :: Dioid e => Graph e a -> Graph e a -> Graph e a
connect = Connect one

connectBy :: e -> Graph e a -> Graph e a -> Graph e a
connectBy = Connect

-- | A convenient ternary-ish operator x -<e>- y, for example:
-- x = Vertex "x"
-- y = Vertex "y"
-- z = x -<1>- y
(-<) :: Graph e a -> e -> (Graph e a, e)
g -< e = (g, e)

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

edgeLabel :: (Eq a, Semilattice e) => a -> a -> Graph e a -> e
edgeLabel _ _ Empty           = zero
edgeLabel _ _ (Vertex _)      = zero
edgeLabel x y (Connect e g h) = edgeLabel x y g \/ edgeLabel x y h \/ new
  where
    new | x `elem` g && y `elem` h = e
        | otherwise                = zero

type UnlabelledGraph a = Graph Bool a
