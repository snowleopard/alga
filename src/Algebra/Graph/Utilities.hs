-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Utilities
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
-- documents with fast concatenation implemented via difference lists.
-----------------------------------------------------------------------------
module Algebra.Graph.Utilities (
    -- * Documents
    Doc, literal, render
  ) where

import Data.Semigroup
import Data.String
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
newtype Doc a = Doc (Endo [a]) deriving (Monoid, Semigroup)

instance (Monoid a, Show a) => Show (Doc a) where
    show = show . render

instance (Monoid a, Eq a) => Eq (Doc a) where
    x == y = render x == render y

instance (Monoid a, Ord a) => Ord (Doc a) where
    compare x y = compare (render x) (render y)

instance IsString a => IsString (Doc a) where
    fromString = literal . fromString

instance IsList (Doc a) where
  type Item (Doc a) = a
  fromList       = Doc . Endo . (<>)
  toList (Doc x) = appEndo x []

-- | Construct a document comprising a single symbol or word. If @a@ is an
-- instance of class 'IsString', then documents of type 'Doc' @a@ can be
-- constructed directly from string literals (see the second example below).
--
-- @
-- literal "Hello, " 'Data.Monoid.<>' literal "World!" == literal "Hello, World!"
-- literal "I am just a string literal"  == "I am just a string literal"
-- literal 'mempty'                        == 'mempty'
-- literal "al" 'Data.Monoid.<>' literal "ga"          == ["al", "ga"]
-- 'render' . literal                      == 'id'
-- literal . 'render'                      == 'id'
-- @
literal :: a -> Doc a
literal = Doc . Endo . (:)

-- | Render a document as a single string or word. An inverse of the function
-- 'literal'.
--
-- @
-- render ('literal' "al" 'Data.Monoid.<>' 'literal' "ga") :: ('IsString' s, 'Monoid' s) => s
-- render ('literal' "al" 'Data.Monoid.<>' 'literal' "ga") == "alga"
-- render 'mempty'                         == 'mempty'
-- render . 'literal'                      == 'id'
-- 'literal' . render                      == 'id'
-- @
render :: Monoid a => Doc a -> a
render = mconcat . toList
