{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Export
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines basic data types and functions for exporting graphs in
-- textual and binary formats. "Algebra.Graph.Export.Dot" provides DOT-specific
-- functionality.
-----------------------------------------------------------------------------
module Algebra.Graph.Export (
    -- * Constructing and exporting documents
    Doc, literal, export,

    -- * Common combinators for text documents
    newLine, brackets, doubleQuotes, indent, unlines
  ) where

import Prelude hiding (unlines)
import Data.Monoid
import Data.String hiding (unlines)

-- | An abstract document type, where @s@ is the type of symbols or words (text
-- or binary). Note that most operations on 'Doc' @s@ require the 'Monoid' @s@
-- instance.
newtype Doc s = Doc (Endo [s]) deriving Monoid

instance IsString s => IsString (Doc s) where
    fromString = literal . fromString

-- | Construct a document comprising a single symbol or word. An inverse of the
-- function 'export'. If @s@ is an instance of class 'IsString', then documents
-- of type 'Doc' @s@ can be constructed directly from string literals (see the
-- second example below).
--
-- @
-- literal "Hello, " <> literal "World!" == literal "Hello, World!"
-- literal "I am just a string literal"  == "I am just a string literal"
-- literal 'mempty'                        == 'mempty'
-- 'export' . literal                      == 'id'
-- literal . 'export'                      == 'id'
-- @
literal :: s -> Doc s
literal = Doc . Endo . (:)

-- | Export a document by concatenating the contents. An inverse of the function
-- 'literal'.
--
-- @
-- export ('literal' "al" <> 'literal' "ga") == "alga"
-- export 'mempty'                         == 'mempty'
-- export . 'literal'                      == 'id'
-- 'literal' . export                      == 'id'
-- @
export :: Monoid s => Doc s -> s
export (Doc x) = mconcat $ appEndo x []

-- | A document comprising a single newline symbol.
newLine :: IsString s => Doc s
newLine = "\n"

-- | Wrap a document into square brackets.
brackets :: IsString s => Doc s -> Doc s
brackets x = "[" <> x <> "]"

-- | Wrap a document into double quotes.
doubleQuotes :: IsString s => Doc s -> Doc s
doubleQuotes x = "\"" <> x <> "\""

-- | Prepend a given number of spaces to a document.
indent :: IsString s => Int -> Doc s -> Doc s
indent spaces x = fromString (replicate spaces ' ') <> x

-- | Concatenate documents after appending a terminating 'newLine' to each.
unlines :: IsString s => [Doc s] -> Doc s
unlines []     = mempty
unlines (x:xs) = x <> newLine <> unlines xs
