{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2019
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
-- is unstable and unsafe, and is exposed only for documentation.
-----------------------------------------------------------------------------
module Algebra.Graph.Internal (
    -- * Data structures
    List (..),

    -- * Graph traversal
    Focus (..), emptyFocus, vertexFocus, overlayFoci, connectFoci, Hit (..),
    foldr1Safe, maybeF,

    -- * Utilities
    setProduct, setProductWith
    ) where

import Data.Foldable
import Data.Semigroup
import Data.Set (Set)

import qualified Data.Set as Set
import qualified GHC.Exts as Exts

-- | An abstract list data type with /O(1)/ time concatenation (the current
-- implementation uses difference lists). Here @a@ is the type of list elements.
-- 'List' @a@ is a 'Monoid': 'mempty' corresponds to the empty list and two lists
-- can be concatenated with 'mappend' (or operator 'Data.Semigroup.<>'). Singleton
-- lists can be constructed using the function 'pure' from the 'Applicative'
-- instance. 'List' @a@ is also an instance of 'IsList', therefore you can use
-- list literals, e.g. @[1,4]@ @::@ 'List' @Int@ is the same as 'pure' @1@
-- 'Data.Semigroup.<>' 'pure' @4@; note that this requires the @OverloadedLists@
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
    toList    = Exts.toList

instance Functor List where
    fmap f = Exts.fromList . map f . toList

instance Applicative List where
    pure    = List . Endo . (:)
    f <*> x = Exts.fromList (toList f <*> toList x)

instance Monad List where
    return  = pure
    x >>= f = Exts.fromList (toList x >>= toList . f)

-- | The /focus/ of a graph expression is a flattened represenentation of the
-- subgraph under focus, its context, as well as the list of all encountered
-- vertices. See 'Algebra.Graph.removeEdge' for a use-case example.
data Focus a = Focus
    { ok :: Bool     -- ^ True if focus on the specified subgraph is obtained.
    , is :: List a   -- ^ Inputs into the focused subgraph.
    , os :: List a   -- ^ Outputs out of the focused subgraph.
    , vs :: List a } -- ^ All vertices (leaves) of the graph expression.

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

-- | An auxiliary data type for 'hasEdge': when searching for an edge, we can hit
-- its 'Tail', i.e. the source vertex, the whole 'Edge', or 'Miss' it entirely.
data Hit = Miss | Tail | Edge deriving (Eq, Ord)

-- | A safe version of 'foldr1'.
foldr1Safe :: (a -> a -> a) -> [a] -> Maybe a
foldr1Safe f = foldr (maybeF f) Nothing
{-# INLINE foldr1Safe #-}

-- | Auxiliary function that try to apply a function to a base case and a 'Maybe'
-- value and return 'Just' the result or 'Just' the base case.
maybeF :: (a -> b -> a) -> a -> Maybe b -> Maybe a
maybeF f x = \y ->
  case y of
    Nothing -> Just x
    Just y  -> Just (f x y)
{-# INLINE maybeF #-}

-- | Compute the Cartesian product of two sets.
setProduct :: Set a -> Set b -> Set (a, b)
#if MIN_VERSION_containers(0,5,11)
setProduct = Set.cartesianProduct
#else
setProduct x y = Set.fromDistinctAscList [ (a, b) | a <- Set.toAscList x, b <- Set.toAscList y ]
#endif

-- | Compute the Cartesian product of two sets, applying a function to each
-- resulting pair.
setProductWith :: Ord c => (a -> b -> c) -> Set a -> Set b -> Set c
setProductWith f x y = Set.fromList [ f a b | a <- Set.toAscList x, b <- Set.toAscList y ]
