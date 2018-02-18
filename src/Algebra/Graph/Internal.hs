{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines various internal utilities and data structures used
-- throughout the library, such as lists with fast concatenation. The API
-- is unstable and unsafe.
-----------------------------------------------------------------------------
module Algebra.Graph.Internal (
    -- * General data structures
    List (..),

    -- * Data structures for graph traversal
    Focus, focus, Context (..), context, filterContext
  ) where

import Control.Applicative (Applicative (..))
import Data.Foldable
import Data.Semigroup

import Algebra.Graph.Class (ToGraph(..))

import qualified Algebra.Graph.HigherKinded.Class as H
import qualified GHC.Exts                         as Exts

-- | An abstract list data type with /O(1)/ time concatenation (the current
-- implementation uses difference lists). Here @a@ is the type of list elements.
-- 'List' @a@ is a 'Monoid': 'mempty' corresponds to the empty list and two lists
-- can be concatenated with 'mappend' (or operator 'Data.Monoid.<>'). Singleton
-- lists can be constructed using the function 'pure' from the 'Applicative'
-- instance. 'List' @a@ is also an instance of 'IsList', therefore you can use
-- list literals, e.g. @[1,4]@ @::@ 'List' @Int@ is the same as 'pure' @1@
-- 'Data.Monoid.<>' 'pure' @4@; note that this requires the @OverloadedLists@
-- GHC extension. To extract plain Haskell lists you can use the 'toList'
-- function from the 'Foldable' instance.
newtype List a = List (Endo [a]) deriving (Monoid, Semigroup)

instance Show a => Show (List a) where
    show = show . toList

instance Eq a => Eq (List a) where
    x == y = toList x == toList y

instance Ord a => Ord (List a) where
    compare x y = compare (toList x) (toList y)

-- TODO: Add rewrite rules? fromList . toList == toList . fromList == id
instance Exts.IsList (List a) where
    type Item (List a) = a
    fromList        = List . Endo . (<>)
    toList (List x) = appEndo x []

instance Foldable List where
    foldMap f = foldMap f . Exts.toList
#if MIN_VERSION_base(4,8,0)
    toList    = Exts.toList
#endif

instance Functor List where
    fmap f = Exts.fromList . map f . toList

instance Applicative List where
    pure    = List . Endo . (:)
    f <*> x = Exts.fromList (toList f <*> toList x)

instance Monad List where
    return  = pure
    x >>= f = Exts.fromList (toList x >>= toList . f)

-- | Focus on the empty graph.
emptyFocus :: Focus a
emptyFocus = Focus False mempty mempty mempty

-- | Focus on the graph with a single vertex, given a predicate indicating
-- whether the vertex is of interest.
vertexFocus :: (a -> Bool) -> a -> Focus a
vertexFocus f x = Focus (f x) mempty mempty (pure x)

-- | Overlay two foci.
overlayFoci :: Focus a -> Focus a -> Focus a
overlayFoci x y = Focus (ok x || ok y) (is x <> is y) (os x <> os y) (vs x <> vs y)

-- | Connect two foci.
connectFoci :: Focus a -> Focus a -> Focus a
connectFoci x y = Focus (ok x || ok y) (xs <> is y) (os x <> ys) (vs x <> vs y)
  where
    xs = if ok y then vs x else is x
    ys = if ok x then vs y else os y

-- | The context of a subgraph comprises the input and output vertices outside
-- the subgraph that are connected to the vertices inside the subgraph.
data Context a = Context { inputs :: [a], outputs :: [a] }

-- | Extract the context from a graph 'Focus'. Returns @Nothing@ if the focus
-- could not be obtained.
context :: ToGraph g => (ToVertex g -> Bool) -> g -> Maybe (Context (ToVertex g))
context p g | ok f      = Just $ Context (toList $ is f) (toList $ os f)
            | otherwise = Nothing
  where
    f = focus p g

-- | The /focus/ of a graph expression is a flattened represenentation of the
-- subgraph under focus, its interface, as well as the list of all encountered
-- vertices. See 'Algebra.Graph.removeEdge' for a use-case example.
data Focus a = Focus
    { ok :: Bool     -- ^ True if focus on the specified subgraph is obtained.
    , is :: List a   -- ^ Inputs into the focused subgraph.
    , os :: List a   -- ^ Outputs out of the focused subgraph.
    , vs :: List a } -- ^ All vertices (leaves) of the graph expression.

-- | 'Focus' on a specified subgraph.
focus :: ToGraph g => (ToVertex g -> Bool) -> g -> Focus (ToVertex g)
focus f = foldg emptyFocus (vertexFocus f) overlayFoci connectFoci

-- TODO: This can be generalised to support non-empty graphs.
-- | Filter vertices in a subgraph context.
filterContext :: (Eq a, H.Graph g, ToGraph (g a), ToVertex (g a) ~ a)
              => a -> (a -> Bool) -> (a -> Bool) -> g a -> g a
filterContext s i o g = maybe g go $ context (==s) g
  where
    go (Context is os) = H.overlays [ H.induce (/=s) g
                                    , H.starTranspose s (filter i is)
                                    , H.star          s (filter o os) ]
