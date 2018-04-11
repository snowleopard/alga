{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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
    Semiring (..), Graph (..),

    -- * Operations
    edgeWeight
  ) where

import qualified Algebra.Graph.Class as C

-- This class has usual semiring laws:
--
--            x |+| y == x |+| y
--    x |+| (y |+| z) == (x |+| y) |+| z
--         x |+| zero == x
--
--    x |*| (y |*| z) == (x |*| y) |*| z
--         x |*| zero == zero
--         zero |*| x == zero
--          x |*| one == x
--          one |*| x == x
--
--    x |*| (y |+| z) == x |*| y |+| x |*| z
--    (x |+| y) |*| z == x |*| z |+| y |*| z
--
class Semiring a where
    zero  :: a
    one   :: a
    (|+|) :: a -> a -> a
    (|*|) :: a -> a -> a

infixl 6 |+|
infixl 7 |*|

-- This adds the idempotence law for |+|: x |+| x == x
class Semiring a => Dioid a where

-- Type variable e stands for edge labels
data Graph e a = Empty
               | Vertex a
               | LabelledConnect e (Graph e a) (Graph e a)
               deriving (Foldable, Functor, Show, Traversable)

overlay :: Semiring e => Graph e a -> Graph e a -> Graph e a
overlay = LabelledConnect zero

connect :: Semiring e => Graph e a -> Graph e a -> Graph e a
connect = LabelledConnect one

-- TODO: Test the C.Graph laws
instance Dioid e => C.Graph (Graph e a) where
    type Vertex (Graph e a) = a
    empty   = Empty
    vertex  = Vertex
    overlay = overlay
    connect = connect

edgeWeight :: (Eq a, Dioid e) => a -> a -> Graph e a -> e
edgeWeight _ _ Empty                   = zero
edgeWeight _ _ (Vertex _)              = zero
edgeWeight x y (LabelledConnect e g h) = edgeWeight x y g |+| edgeWeight x y h |+| new
  where
    new | x `elem` g && y `elem` h = e
        | otherwise                = zero
