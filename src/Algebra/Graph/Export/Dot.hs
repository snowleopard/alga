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
-- This module defines functions for exporting graphs in the DOT file format.
-----------------------------------------------------------------------------
module Algebra.Graph.Export.Dot (
    -- * Graph attributes and style
    Attribute (..), Style (..), defaultStyle, defaultStyleViaShow,

    -- * Export functions
    export, exportAsIs, exportViaShow
  ) where

import Prelude hiding (unlines)
import Data.List hiding (unlines)
import Data.Monoid
import Data.String hiding (unlines)

import Algebra.Graph.AdjacencyMap
import Algebra.Graph.Class (ToGraph (..))
import Algebra.Graph.Export

-- | An attribute is just a key-value pair, for example @"shape" := "box"@.
-- Attributes are used to specify the style of graph elements during export.
data Attribute s = (:=) s s

-- | The record 'Style' @a@ @s@ specifies the style to use when exporting a
-- graph in the DOT format. Here @a@ is the type of the graph vertices, and @s@
-- is the type of string to represent the resulting DOT document (e.g. String,
-- Text, etc.). Most fields can be empty. The only field that has no obvious
-- default value is 'vertexName', which holds a function of type @a -> s@ to
-- compute vertex names. See the example for the function 'export'.
data Style a s = Style
    { graphName :: s
    -- ^ Name of the graph.
    , preamble :: s
    -- ^ Preamble is added at the beginning of the DOT file body.
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

-- | Default style for exporting graphs. All style settings are empty except for
-- 'vertexName', which is provided as the only argument.
defaultStyle :: Monoid s => (a -> s) -> Style a s
defaultStyle v = Style mempty mempty [] [] [] v (\_ -> []) (\_ _ -> [])

-- | Default style for exporting graphs whose vertices are 'Show'-able. All
-- style settings are empty except for 'vertexName', which is computed from
-- 'show'.
--
-- @
-- defaultStyleViaShow = 'defaultStyle' ('fromString' . 'show')
-- @
defaultStyleViaShow :: (Show a, IsString s, Monoid s) => Style a s
defaultStyleViaShow = defaultStyle (fromString . show)

-- TODO: Avoid round-trip graph conversion if g :: AdjacencyMap a.
-- | Export a graph with a given style.
--
-- For example:
--
-- @
-- style :: 'Style' Int String
-- style = 'Style'
--     { 'graphName'               = \"Example\"
--     , 'preamble'                = "  // This is an example\\n"
--     , 'graphAttributes'         = ["label" := \"Example\", "labelloc" := "top"]
--     , 'defaultVertexAttributes' = ["shape" := "circle"]
--     , 'defaultEdgeAttributes'   = 'mempty'
--     , 'vertexName'              = \x   -> "v" ++ 'show' x
--     , 'vertexAttributes'        = \x   -> ["color" := "blue"   | 'odd' x      ]
--     , 'edgeAttributes'          = \x y -> ["style" := "dashed" | 'odd' (x * y)] }
--
-- > putStrLn $ export style (1 * 2 + 3 * 4 * 5 :: 'Graph' Int)
--
-- digraph Example
-- {
--   // This is an example
--
--   graph [label=\"Example\" labelloc="top"]
--   node [shape="circle"]
--   "v1" [color="blue"]
--   "v2"
--   "v3" [color="blue"]
--   "v4"
--   "v5" [color="blue"]
--   "v1" -> "v2"
--   "v3" -> "v4"
--   "v3" -> "v5" [style="dashed"]
--   "v4" -> "v5"
-- }
-- @
export :: (IsString s, Monoid s, Eq s, Ord a, ToGraph g, ToVertex g ~ a) => Style a s -> g -> s
export Style {..} g = render $ unlines $ header ++ map (indent 2) body ++ ["}"]
  where
    header    = ["digraph " <> literal graphName, "{"]
             ++ [ literal preamble | preamble /= mempty ]
    with x as = if null as then [] else [x <> attributes as]
    body      = ("graph" `with` graphAttributes)
             ++ ("node"  `with` defaultVertexAttributes)
             ++ ("edge"  `with` defaultEdgeAttributes)
             ++ map v (vertexList adj) ++ map e (edgeList adj)
    label     = doubleQuotes . literal . vertexName
    v x       = label x <> attributes (vertexAttributes x)
    e (x, y)  = label x <> " -> " <> label y <> attributes (edgeAttributes x y)
    adj       = toGraph g

-- A list of attributes formatted as a DOT document.
-- Example: @attributes ["label" := "A label", "shape" := "box"]@
-- corresponds to document: @ [label="A label" shape="box"]@.
attributes :: IsString s => [Attribute s] -> Doc s
attributes [] = mempty
attributes as = indent 1 . brackets . mconcat . intersperse " " $ map dot as
  where
    dot (k := v) = mconcat [literal k <> "=" <> doubleQuotes (literal v)]

-- | Export a graph whose vertices are represented simply by their names.
--
-- For example:
--
-- @
-- > Text.putStrLn $ exportAsIs ('circuit' ["a", "b", "c"] :: 'AdjacencyMap' Text)
--
-- digraph
-- {
--   "a"
--   "b"
--   "c"
--   "a" -> "b"
--   "b" -> "c"
--   "c" -> "a"
-- }
-- @
exportAsIs :: (IsString s, Monoid s, Ord s, ToGraph g, ToVertex g ~ s) => g -> s
exportAsIs = export (defaultStyle id)

-- | Export a graph using the 'defaultStyleViaShow'.
--
-- For example:
--
-- @
-- > putStrLn $ exportViaShow (1 + 2 * (3 + 4) :: 'Graph' Int)
--
-- digraph
-- {
--   "1"
--   "2"
--   "3"
--   "4"
--   "2" -> "3"
--   "2" -> "4"
-- }
-- @
exportViaShow :: (IsString s, Monoid s, Eq s, ToGraph g, Ord (ToVertex g), Show (ToVertex g)) => g -> s
exportViaShow = export defaultStyleViaShow
