{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Export.Dot
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module defines functions for exporting graphs in DOT file format.
-----------------------------------------------------------------------------
module Algebra.Graph.Export.Dot (
    -- * Exporting graphs as DOT documents
    Attribute (..), Style (..), defaultStyle, defaultStyleViaShow, exportWith,
    export, exportViaShow
  ) where

import Prelude hiding (unlines)
import Data.List hiding (unlines)
import Data.Monoid
import Data.String hiding (unlines)

import Algebra.Graph.AdjacencyMap
import qualified Algebra.Graph.Export as E
import Algebra.Graph.Export hiding (export)
import Algebra.Graph.Class (ToGraph (..))

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
data Style a s = Style
    { graphName :: s
    -- ^ The name of the graph.
    , preamble :: s
    -- ^ Preamble is added at the beginning of the DOT file body, after the
    -- opening @{@ bracket.
    , graphAttributes :: [Attribute s]
    -- ^ Graph style, e.g. @["bgcolor" := "azure"]@.
    , defaultVertexAttributes :: [Attribute s]
    -- ^ Default vertex style, e.g. @["shape" := "diamond"]@.
    , defaultEdgeAttributes :: [Attribute s]
    -- ^ Default edge style, e.g. @["style" := "dashed"]@.
    , vertexName :: a -> s
    -- ^ Compute a vertex name.
    , vertexAttributes :: a -> [Attribute s]
    -- ^ Attributes of a specific vertex.
    , edgeAttributes   :: a -> a -> [Attribute s]
    -- ^ Attributes of a specific edge.
    }

-- | Default style for exporting graphs.
defaultStyle :: Monoid s => (a -> s) -> Style a s
defaultStyle v = Style mempty mempty [] [] [] v (\_ -> []) (\_ _ -> [])

-- | Default style for exporting graphs whose vertices are 'Show'-able.
defaultStyleViaShow :: (Show a, IsString s, Monoid s) => Style a s
defaultStyleViaShow = defaultStyle (fromString . show)

-- TODO: Avoid round-trip graph conversion if g :: AdjacencyMap a.
-- | Export a graph using a given DOT style.
exportWith :: (IsString s, Monoid s, Ord a, ToGraph g, ToVertex g ~ a) => Style a s -> g -> s
exportWith Style {..} g = E.export $ unlines $ header ++ map (indent 2) body ++ ["}"]
  where
    header    = ["digraph " <> literal graphName, "{", literal preamble]
             ++ ("graph" `with` graphAttributes)
             ++ ("node"  `with` defaultVertexAttributes)
             ++ ("edge"  `with` defaultEdgeAttributes)
    with x as = if null as then [] else [x <> dotAttributes as]
    body      = map v (vertexList adj) ++ map e (edgeList adj)
    label     = doubleQuotes . literal . vertexName
    v x       = label x <> dotAttributes (vertexAttributes x)
    e (x, y)  = label x <> " -> " <> label y <> dotAttributes (edgeAttributes x y)
    adj       = toGraph g

-- | Export a graph using the 'defaultStyle'.
export :: (IsString s, Monoid s, Ord s, ToGraph g, ToVertex g ~ s) => g -> s
export = exportWith (defaultStyle id)

-- | Export a graph using the 'defaultStyleViaShow'.
exportViaShow :: (IsString s, Monoid s, ToGraph g, Ord (ToVertex g), Show (ToVertex g)) => g -> s
exportViaShow = exportWith defaultStyleViaShow
