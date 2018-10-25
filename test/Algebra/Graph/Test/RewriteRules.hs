{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.RewriteRules
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph" rewrite-rules
-----------------------------------------------------------------------------
module Algebra.Graph.Test.RewriteRules where

import Data.Maybe (fromMaybe)

import Algebra.Graph hiding ((===))
import Algebra.Graph.Internal

import Test.Inspection

-- overlays/connects tests
--- overlays . map vertex
vertices', overlaysDotMapVertex :: [a] -> Graph a
vertices'            = fromMaybe Empty . foldr (mf Overlay . Vertex) Nothing
overlaysDotMapVertex = overlays . map vertex

inspect $ 'vertices' === 'overlaysDotMapVertex

--- connects . map vertex
clique', connectsDotMapVertex :: [a] -> Graph a
clique'              = fromMaybe Empty . foldr (mf Connect . Vertex) Nothing
connectsDotMapVertex = connects . map vertex

inspect $ 'clique' === 'connectsDotMapVertex

-- transpose tests
--- transpose empty
empty', transposeEmpty :: Graph a
empty'         = Empty
transposeEmpty = transpose Empty

inspect $ 'empty' === 'transposeEmpty

--- transpose . vertex
vertex', transposeDotVertex :: a -> Graph a
vertex'            = Vertex
transposeDotVertex = transpose . vertex

inspect $ 'vertex' === 'transposeDotVertex

--- transpose . overlay
overlayTransposed, transposeDotOverlay :: Graph a -> Graph a -> Graph a
overlayTransposed   g1 g2 = Overlay (transpose g1) (transpose g2)
transposeDotOverlay g1 g2 = transpose $ Overlay g1 g2

inspect $ 'overlayTransposed === 'transposeDotOverlay

--- transpose . connect
connectTransposed, transposeDotConnect :: Graph a -> Graph a -> Graph a
connectTransposed   g1 g2 = Connect (transpose g2) (transpose g1)
transposeDotConnect g1 g2 = transpose $ Connect g1 g2

inspect $ 'connectTransposed === 'transposeDotConnect

--- transpose . overlays
overlaysTransposed, transposeDotOverlays :: [Graph a] -> Graph a
overlaysTransposed   = overlays . map transpose
transposeDotOverlays = transpose . overlays

inspect $ 'overlaysTransposed === 'transposeDotOverlays

--- transpose . connects
connectsTransposed, transposeDotConnects :: [Graph a] -> Graph a
connectsTransposed   = connects . reverse . map transpose
transposeDotConnects = transpose . connects

inspect $ 'connectsTransposed === 'transposeDotConnects

--- transpose . vertices
verticesTransposed, transposeDotVertices :: [a] -> Graph a
verticesTransposed   = overlays . map vertex
transposeDotVertices = transpose . overlays . map vertex

inspect $ 'verticesTransposed === 'transposeDotVertices

--- transpose . clique
cliqueTransposed, transposeDotClique :: [a] -> Graph a
cliqueTransposed   = connects . reverse . map vertex
transposeDotClique = transpose . connects . map vertex

inspect $ 'cliqueTransposed === 'transposeDotClique

--- transpose . star
starTranspose, transposeDotStar :: a -> [a] -> Graph a
starTranspose a [] = vertex a
starTranspose a xs = connect (vertices xs) (vertex a)
transposeDotStar x = transpose . star x

inspect $ 'starTranspose === 'transposeDotStar
