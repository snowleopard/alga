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

-- transpose . star
starTranspose :: Int -> [Int] -> Graph Int
starTranspose a [] = vertex a
starTranspose a xs = connect (vertices xs) (vertex a)

transposeDotStar :: Int -> [Int] -> Graph Int
transposeDotStar x = transpose . star x

I.inspect $ 'starTranspose I.=== 'transposeDotStar

-- transpose . overlays
overlays' :: [Graph Int] -> Graph Int
overlays' = overlays . map transpose

transposeDotOverlays :: [Graph Int] -> Graph Int
transposeDotOverlays = transpose . overlays

I.inspect $ 'overlays' I.=== 'transposeDotOverlays

-- transpose . vertices
vertices' :: [Int] -> Graph Int
vertices' = overlays . map vertex

transposeDotVertices :: [Int] -> Graph Int
transposeDotVertices = transpose . vertices

I.inspect $ 'vertices' I.=== 'transposeDotVertices
