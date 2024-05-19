{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Export
-- Copyright  : (c) Andrey Mokhov 2016-2024
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines basic functionality for exporting graphs in textual and
-- binary formats. "Algebra.Graph.Export.Dot" provides DOT-specific functions.
-----------------------------------------------------------------------------
module Algebra.Graph.Export (
    -- * Constructing and exporting documents
    Doc, isEmpty, literal, render,

    -- * Common combinators for text documents
    (<+>), brackets, doubleQuotes, indent, unlines,

    -- * Generic graph export
    export
    ) where

import Data.Foldable (fold)
import Data.String hiding (unlines)
import Prelude hiding (unlines)

import Algebra.Graph.ToGraph (ToGraph, ToVertex, toAdjacencyMap)
import Algebra.Graph.AdjacencyMap (vertexList, edgeList)
import Algebra.Graph.Internal

-- | An abstract document data type with /O(1)/ time concatenation (the current
-- implementation uses difference lists). Here @s@ is the type of abstract
-- symbols or strings (text or binary). 'Doc' @s@ is a 'Monoid', therefore
-- 'mempty' corresponds to the /empty document/ and two documents can be
-- concatenated with 'mappend' (or operator 'Data.Semigroup.<>'). Documents
-- comprising a single symbol or string can be constructed using the function
-- 'literal'. Alternatively, you can construct documents as string literals,
-- e.g. simply as @"alga"@, by using the @OverloadedStrings@ GHC extension. To
-- extract the document contents use the function 'render'.
--
-- Note that the document comprising a single empty string is considered to be
-- different from the empty document. This design choice is motivated by the
-- desire to support string types @s@ that have no 'Eq' instance, such as
-- "Data.ByteString.Builder", for which there is no way to check whether a
-- string is empty or not. As a consequence, the 'Eq' and 'Ord' instances are
-- defined as follows:
--
-- @
-- 'mempty' /= 'literal' ""
-- 'mempty' <  'literal' ""
-- @
newtype Doc s = Doc (List s) deriving (Monoid, Semigroup)

instance (Monoid s, Show s) => Show (Doc s) where
    show = show . render

instance (Monoid s, Eq s) => Eq (Doc s) where
    x == y | isEmpty x = isEmpty y
           | isEmpty y = False
           | otherwise = render x == render y

-- | The empty document is smallest.
instance (Monoid s, Ord s) => Ord (Doc s) where
    compare x y | isEmpty x = if isEmpty y then EQ else LT
                | isEmpty y = GT
                | otherwise = compare (render x) (render y)

instance IsString s => IsString (Doc s) where
    fromString = literal . fromString

-- | Check if a document is empty. The result is the same as when comparing the
-- given document to 'mempty', but this function does not require the 'Eq' @s@
-- constraint. Note that the document comprising a single empty string is
-- considered to be different from the empty document.
--
-- @
-- isEmpty 'mempty'       == True
-- isEmpty ('literal' \"\") == False
-- isEmpty x            == (x == 'mempty')
-- @
isEmpty :: Doc s -> Bool
isEmpty (Doc xs) = null xs

-- | Construct a document comprising a single symbol or string. If @s@ is an
-- instance of class 'IsString', then documents of type 'Doc' @s@ can be
-- constructed directly from string literals (see the second example below).
--
-- @
-- literal "Hello, " 'Data.Semigroup.<>' literal "World!" == literal "Hello, World!"
-- literal "I am just a string literal"  == "I am just a string literal"
-- 'render' . literal                      == 'id'
-- @
literal :: s -> Doc s
literal = Doc . pure

-- | Render the document as a single string. An inverse of the function 'literal'.
--
-- @
-- render ('literal' "al" 'Data.Semigroup.<>' 'literal' "ga") :: ('IsString' s, 'Monoid' s) => s
-- render ('literal' "al" 'Data.Semigroup.<>' 'literal' "ga") == "alga"
-- render 'mempty'                         == 'mempty'
-- render . 'literal'                      == 'id'
-- @
render :: Monoid s => Doc s -> s
render (Doc x) = fold x

-- | Concatenate two documents, separated by a single space, unless one of the
-- documents is empty. The operator \<+\> is associative with identity 'mempty'.
--
-- @
-- x \<+\> 'mempty'         == x
-- 'mempty' \<+\> x         == x
-- x \<+\> (y \<+\> z)      == (x \<+\> y) \<+\> z
-- "name" \<+\> "surname" == "name surname"
-- @
(<+>) :: IsString s => Doc s -> Doc s -> Doc s
x <+> y | isEmpty x = y
        | isEmpty y = x
        | otherwise = x <> " " <> y

infixl 7 <+>

-- | Wrap a document in square brackets.
--
-- @
-- brackets "i"    == "[i]"
-- brackets 'mempty' == "[]"
-- @
brackets :: IsString s => Doc s -> Doc s
brackets x = "[" <> x <> "]"

-- | Wrap a document into double quotes.
--
-- @
-- doubleQuotes "\/path\/with spaces"   == "\\"\/path\/with spaces\\""
-- doubleQuotes (doubleQuotes 'mempty') == "\\"\\"\\"\\""
-- @
doubleQuotes :: IsString s => Doc s -> Doc s
doubleQuotes x = "\"" <> x <> "\""

-- | Prepend a given number of spaces to a document.
--
-- @
-- indent 0        == 'id'
-- indent 1 'mempty' == " "
-- @
indent :: IsString s => Int -> Doc s -> Doc s
indent spaces x = fromString (replicate spaces ' ') <> x

-- | Concatenate documents after appending a terminating newline symbol to each.
--
-- @
-- unlines []                    == 'mempty'
-- unlines ['mempty']              == "\\n"
-- unlines ["title", "subtitle"] == "title\\nsubtitle\\n"
-- @
unlines :: IsString s => [Doc s] -> Doc s
unlines []     = mempty
unlines (x:xs) = x <> "\n" <> unlines xs

-- TODO: Avoid round-trip graph conversion if g :: AdjacencyMap a.
-- | Export a graph into a document given two functions that construct documents
-- for individual vertices and edges. The order of export is: vertices, sorted
-- by 'Ord' @a@, and then edges, sorted by 'Ord' @(a, a)@.
--
-- For example:
--
-- @
-- vDoc x   = 'literal' ('show' x) <> "\\n"
-- eDoc x y = 'literal' ('show' x) <> " -> " <> 'literal' ('show' y) <> "\\n"
-- > putStrLn $ 'render' $ export vDoc eDoc (1 + 2 * (3 + 4) :: 'Algebra.Graph.Graph' Int)
--
-- 1
-- 2
-- 3
-- 4
-- 2 -> 3
-- 2 -> 4
-- @
export :: (Ord a, ToGraph g, ToVertex g ~ a) => (a -> Doc s) -> (a -> a -> Doc s) -> g -> Doc s
export v e g = vDoc <> eDoc
  where
    vDoc   = mconcat $ map  v          (vertexList adjMap)
    eDoc   = mconcat $ map (uncurry e) (edgeList   adjMap)
    adjMap = toAdjacencyMap g
