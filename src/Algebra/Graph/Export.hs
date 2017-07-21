{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
-- This module defines functions for exporting algebraic graphs into textual and
-- binary formats.
-----------------------------------------------------------------------------
module Algebra.Graph.Export (
    -- * Constructing and exporting documents
    Doc, literal, export, Attribute (..),

    -- * Common combinators for text documents
    newLine, brackets, doubleQuotes, indent, unlines,

    -- * Exporting graphs as DOT documents
    DotStyle (..), defaultDotStyle, defaultDotStyleViaShow, dotExportWith,
    dotExport, dotExportViaShow
  ) where

import Prelude hiding (unlines)
import Data.List hiding (unlines)
import Data.Monoid
import Data.String hiding (unlines)

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.Class (ToGraph (..))

-- | An abstract document type, where @s@ is the type of symbols or words (text
-- or binary). Note that most operations on 'Doc' @s@ require the 'Monoid' @s@
-- instance.
newtype Doc s = Doc (Endo [s]) deriving Monoid

-- | Construct a document comprising a single symbol.
literal :: s -> Doc s
literal = Doc . Endo . (:)

instance IsString s => IsString (Doc s) where
    fromString = literal . fromString

-- | Export a document by concatenating the contents.
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

-- | An attribute is just a key-value pair, for example @"shape" := "box"@.
-- Attributes are used to specify the style of graph elements during export.
data Attribute s = (:=) s s

-- | Example: @dotAttributes ["label" := "A label", "shape" := "box"]@
-- corresponds to document: @ [label="A label" shape="box"]@.
dotAttributes :: IsString s => [Attribute s] -> Doc s
dotAttributes [] = mempty
dotAttributes as = indent 1 . brackets . mconcat . intersperse " " $ map dot as
  where
    dot (k := v) = mconcat [literal k <> "=" <> doubleQuotes (literal v)]

-- | Everything we need to know to export a graph in the DOT format.
data DotStyle a s = DotStyle
    { graphName :: Doc s
    -- ^ The name of the graph.
    , preamble :: Doc s
    -- ^ Preamble is added at the beginning of the DOT file body, after the
    -- opening @{@ bracket.
    , graphAttributes :: [Attribute s]
    -- ^ Graph style, e.g. @["bgcolor" := "azure"]@.
    , defaultVertexAttributes :: [Attribute s]
    -- ^ Default vertex style, e.g. @["shape" := "diamond"]@.
    , defaultEdgeAttributes :: [Attribute s]
    -- ^ Default edge style, e.g. @["style" := "dashed"]@.
    , vertexName :: a -> Doc s
    -- ^ Compute a vertex name.
    , vertexAttributes :: a -> [Attribute s]
    -- ^ Attributes of a specific vertex.
    , edgeAttributes   :: a -> a -> [Attribute s]
    -- ^ Attributes of a specific edge.
    }

-- | Default style for exporting graphs.
defaultDotStyle :: (a -> Doc s) -> DotStyle a s
defaultDotStyle v = DotStyle mempty mempty [] [] [] v (\_ -> []) (\_ _ -> [])

-- | Default style for exporting graphs whose vertices are 'Show'-able.
defaultDotStyleViaShow :: (Show a, IsString s) => DotStyle a s
defaultDotStyleViaShow = defaultDotStyle (fromString . show)

-- TODO: Avoid round-trip graph conversion if g :: AdjacencyMap a.
-- | Export a graph using a given DOT style.
dotExportWith :: (IsString s, Monoid s, Ord a, ToGraph g, ToVertex g ~ a) => DotStyle a s -> g -> s
dotExportWith DotStyle {..} g = export $ unlines $ header ++ map (indent 2) body ++ ["}"]
  where
    header    = ["digraph " <> graphName, "{", preamble]
             ++ ("graph" `with` graphAttributes)
             ++ ("node"  `with` defaultVertexAttributes)
             ++ ("edge"  `with` defaultEdgeAttributes)
    with x as = if null as then [] else [x <> dotAttributes as]
    body      = map v (vertexList adj) ++ map e (edgeList adj)
    label     = doubleQuotes . vertexName
    v x       = label x <> dotAttributes (vertexAttributes x)
    e (x, y)  = label x <> " -> " <> label y <> dotAttributes (edgeAttributes x y)
    adj       = toGraph g

-- | Export a graph using the 'defaultDotStyle'.
dotExport :: (IsString s, Monoid s, Ord s, ToGraph g, ToVertex g ~ s) => g -> s
dotExport = dotExportWith (defaultDotStyle literal)

-- | Export a graph using the 'defaultDotStyleViaShow'.
dotExportViaShow :: (IsString s, Monoid s, ToGraph g, Ord (ToVertex g), Show (ToVertex g)) => g -> s
dotExportViaShow = dotExportWith defaultDotStyleViaShow
