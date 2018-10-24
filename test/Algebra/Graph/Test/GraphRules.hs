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

-- overlays/connects tests
--- overlays . map vertex
vertices', overlaysDotMapVertex :: [a] -> Graph a
vertices'            = fromMaybe Empty . foldr (mf Overlay . Vertex) Nothing
overlaysDotMapVertex = overlays . map vertex

I.inspect $ 'vertices' I.=== 'overlaysDotMapVertex

--- connects . map vertex
clique', connectsDotMapVertex :: [a] -> Graph a
clique'              = fromMaybe Empty . foldr (mf Connect . Vertex) Nothing
connectsDotMapVertex = connects . map vertex

I.inspect $ 'clique' I.=== 'connectsDotMapVertex

-- transpose tests
--- transpose empty
empty', transposeEmpty :: Graph a
empty'         = Empty
transposeEmpty = transpose Empty

I.inspect $ 'empty' I.=== 'transposeEmpty

--- transpose . vertex
vertex', transposeDotVertex :: a -> Graph a
vertex'            = Vertex
transposeDotVertex = transpose . vertex

I.inspect $ 'vertex' I.=== 'transposeDotVertex

--- transpose . overlay
overlayTransposed, transposeDotOverlay :: Graph a -> Graph a -> Graph a
overlayTransposed   g1 g2 = Overlay (transpose g1) (transpose g2)
transposeDotOverlay g1 g2 = transpose $ Overlay g1 g2

I.inspect $ 'overlayTransposed I.=== 'transposeDotOverlay

--- transpose . connect
connectTransposed, transposeDotConnect :: Graph a -> Graph a -> Graph a
connectTransposed   g1 g2 = Connect (transpose g2) (transpose g1)
transposeDotConnect g1 g2 = transpose $ Connect g1 g2

I.inspect $ 'connectTransposed I.=== 'transposeDotConnect

--- transpose . overlays
overlaysTransposed, transposeDotOverlays :: [Graph a] -> Graph a
overlaysTransposed   = overlays . map transpose
transposeDotOverlays = transpose . overlays

I.inspect $ 'overlaysTransposed I.=== 'transposeDotOverlays

--- transpose . connects
connectsTransposed, transposeDotConnects :: [Graph a] -> Graph a
connectsTransposed   = connects . reverse . map transpose
transposeDotConnects = transpose . connects

I.inspect $ 'connectsTransposed I.=== 'transposeDotConnects

--- transpose . vertices
verticesTransposed, transposeDotVertices :: [a] -> Graph a
verticesTransposed   = overlays . map vertex
transposeDotVertices = transpose . overlays . map vertex

I.inspect $ 'verticesTransposed I.=== 'transposeDotVertices

--- transpose . clique
cliqueTransposed, transposeDotClique :: [a] -> Graph a
cliqueTransposed   = connects . reverse . map vertex
transposeDotClique = transpose . connects . map vertex

I.inspect $ 'cliqueTransposed I.=== 'transposeDotClique

--- transpose . star
starTranspose, transposeDotStar :: a -> [a] -> Graph a
starTranspose a [] = vertex a
starTranspose a xs = connect (vertices xs) (vertex a)
transposeDotStar x = transpose . star x

I.inspect $ 'starTranspose I.=== 'transposeDotStar
