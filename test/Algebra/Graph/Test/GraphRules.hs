{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Graph
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph" rewrite-rules
-----------------------------------------------------------------------------
module Algebra.Graph.Test.GraphRules where

import Data.Maybe (fromMaybe)

import Algebra.Graph
import Algebra.Graph.Internal

import qualified Test.Inspection as I

-- overlays tests
vertices', overlaysDotMapVertex :: [a] -> Graph a
vertices'            = fromMaybe Empty . foldr (mf Overlay . Vertex) Nothing
overlaysDotMapVertex = overlays . map vertex

I.inspect $ 'vertices' I.=== 'overlaysDotMapVertex

-- transpose tests
--- transpose . star
starTranspose, transposeDotStar :: a -> [a] -> Graph a
starTranspose a [] = vertex a
starTranspose a xs = connect (vertices xs) (vertex a)

transposeDotStar x = transpose . star x

I.inspect $ 'starTranspose I.=== 'transposeDotStar

--- transpose . overlays
overlays', transposeDotOverlays :: [Graph a] -> Graph a
overlays'            = overlays . map transpose

transposeDotOverlays = transpose . overlays

I.inspect $ 'overlays' I.=== 'transposeDotOverlays

--- transpose . vertices
verticesTransposed, transposeDotVertices :: [a] -> Graph a
verticesTransposed   = overlays . map vertex

transposeDotVertices = transpose . overlays . map vertex

I.inspect $ 'verticesTransposed I.=== 'transposeDotVertices
