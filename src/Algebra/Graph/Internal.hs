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
-- This module defines various utilities used throughout the library, such as
-- lists with fast concatenation.
-----------------------------------------------------------------------------
module Algebra.Graph.Internal (
    -- * Data structures
    List (..)
  ) where

import Prelude ()
import Prelude.Compat

import Data.Semigroup
import GHC.Exts

-- | An abstract document data type with /O(1)/ time concatenation (the current
-- implementation uses difference lists). Here @a@ is the type of abstract
-- symbols or words -- for example, text strings or binary blocks. 'Doc' @a@
-- is a 'Monoid': 'mempty' corresponds to the empty document and two documents
-- can be concatenated with 'mappend' (or operator 'Data.Monoid.<>'). Documents
-- comprising a single symbol or word can be constructed using the function
-- 'literal'. 'Doc' @a@ is also an instance of 'IsList', therefore you can use
-- list literals to construct documents, e.g. @["al", "ga"]@ @::@ 'Doc' @String@
-- is the same as 'literal' @"al"@ 'Data.Monoid.<>' 'literal' @"ga"@; note that
-- this requires the @OverloadedLists@ GHC extension. Finally, by using the
-- @OverloadedStrings@ GHC extension you can construct documents as string
-- literals, e.g. simply as @"alga"@. See some examples below.
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
