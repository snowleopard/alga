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

import Algebra.Graph

import qualified Test.Inspection as I

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
vertices', transposeDotVertices :: [a] -> Graph a
vertices'            = overlays . map vertex

transposeDotVertices = transpose . vertices

I.inspect $ 'vertices' I.=== 'transposeDotVertices

-- buildG tests
--- foldg . fmap fusion
foldgFmap, foldgDotFmap :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (a -> a) -> Graph a -> b
foldgFmap    e v o c f = foldg e (v . f) o c

foldgDotFmap e v o c f = foldg e v o c . fmap f

I.inspect $ 'foldgFmap I.=== 'foldgDotFmap

--- foldg . transpose fusion
foldgTranspose, foldgDotTranspose :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
foldgTranspose    e v o c = foldg e v o (flip c)

foldgDotTranspose e v o c = foldg e v o c . transpose

I.inspect $ 'foldgTranspose I.=== 'foldgDotTranspose

--- fmap . fmap fusion
fmap', fmapDotFmap :: (a -> b) -> (b -> c) -> Graph a -> Graph c
fmap' f g       = foldg Empty (Vertex . (g . f)) Overlay Connect

fmapDotFmap f g = fmap g . fmap f

I.inspect $ 'fmap' I.=== 'fmapDotFmap
