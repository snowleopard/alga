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
    Focus (..)
  ) where

import Control.Applicative (Applicative (..))
import Data.Foldable (Foldable (foldMap))
import Data.Semigroup
import GHC.Exts

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
instance IsList (List a) where
    type Item (List a) = a
    fromList        = List . Endo . (<>)
    toList (List x) = appEndo x []

instance Foldable List where
    foldMap f (List x) = foldMap f (appEndo x [])

instance Functor List where
    fmap f = fromList . map f . toList

instance Applicative List where
    pure    = List . Endo . (:)
    f <*> x = fromList (toList f <*> toList x)

instance Monad List where
    return  = pure
    x >>= f = fromList (toList x >>= toList . f)

data Focus a = Focus
    { ok :: Bool    -- True if focus on the specified subgraph is obtained.
    , is :: List a   -- Inputs into the focused subgraph.
    , os :: List a   -- Outputs out of the focused subgraph.
    , vs :: List a } -- All vertices (leaves) of the graph expression.

