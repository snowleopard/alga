{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.RewriteRules
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph" rewrite rules.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.RewriteRules where

import Data.Maybe (fromMaybe)

import Algebra.Graph hiding ((===))
import Algebra.Graph.Internal

import Test.Inspection

-- Naming convention: we use the suffix "R" to indicate the desired outcome of
-- rewrite rules, and suffices "1", "2", etc. to indicate initial expressions.

-- Testsuite for 'overlays' and 'connects'.
vertices1, verticesR :: [a] -> Graph a
vertices1 = overlays . map vertex
verticesR = fromMaybe Empty . foldr (maybeF Overlay . Vertex) Nothing

inspect $ 'vertices1 === 'verticesR

clique1, cliqueR :: [a] -> Graph a
clique1 = connects . map vertex
cliqueR = fromMaybe Empty . foldr (maybeF Connect . Vertex) Nothing

inspect $ 'clique1 === 'cliqueR

-- Testsuite for 'transpose'.
empty1, emptyR :: Graph a
empty1 = transpose Empty
emptyR = Empty

inspect $ 'empty1 === 'emptyR

vertex1, vertexR :: a -> Graph a
vertex1 = transpose . vertex
vertexR = Vertex

inspect $ 'vertex1 === 'vertexR

overlay1, overlayR :: Graph a -> Graph a -> Graph a
overlay1 x y = transpose (Overlay x y)
overlayR x y = Overlay (transpose x) (transpose y)

inspect $ 'overlay1 === 'overlayR

connect1, connectR :: Graph a -> Graph a -> Graph a
connect1 x y = transpose (Connect x y)
connectR x y = Connect (transpose y) (transpose x)

inspect $ 'connect1 === 'connectR

overlays1, overlaysR :: [Graph a] -> Graph a
overlays1 = transpose . overlays
overlaysR = overlays . map transpose

inspect $ 'overlays1 === 'overlaysR

connects1, connectsR :: [Graph a] -> Graph a
connects1 = transpose . connects
connectsR = fromMaybe Empty . foldr (maybeF (flip Connect) . transpose) Nothing

inspect $ 'connects1 === 'connectsR

vertices2 :: [a] -> Graph a
vertices2 = transpose . overlays . map vertex

inspect $ 'vertices2 === 'vertices1

-- Note that we currently have these three tests:
-- * vertices2 === vertices1
-- * vertices1 === verticesR
-- * vertices2 =/= verticesR
-- This non-transitivity is awkward, and feels like a bug in the inspection
-- testing library. See https://github.com/nomeata/inspection-testing/issues/23.
inspect $ 'vertices2 =/= 'verticesR

cliqueT1, cliqueTR :: [a] -> Graph a
cliqueT1 = transpose . connects . map vertex
cliqueTR = fromMaybe Empty . foldr (maybeF (flip Connect) . Vertex) Nothing

inspect $ 'cliqueT1 === 'cliqueTR

starT1, starTR :: a -> [a] -> Graph a
starT1 x = transpose . star x
starTR a [] = vertex a
starTR a xs = connect (vertices xs) (vertex a)

inspect $ 'starT1 === 'starTR

fmapFmap1, fmapFmapR :: Graph a -> (a -> b) -> (b -> c) -> Graph c
fmapFmap1 g f h = fmap h (fmap f g)
fmapFmapR g f h = fmap (h . f) g

inspect $ 'fmapFmap1 === 'fmapFmapR

bind2, bind2R :: (a -> Graph b) -> (b -> Graph c) -> Graph a -> Graph c
bind2 f g x = x >>= f >>= g
bind2R f g x = x >>= (\x -> f x >>= g)

inspect $ 'bind2 === 'bind2R

-- Ideally, we want this test to pass.
-- Strangely, '<*>' in 'ovApR' does not inline and makes the test fail.
--
-- This is corrected below, where '<*>' was inlined "by hand"
ovAp, ovApR :: Graph (a -> b) -> Graph (a -> b) -> Graph a -> Graph b
ovAp  x y z = overlay x y <*> z
ovApR x y z = overlay (x <*> z) (y <*> z)

inspect $ 'ovAp =/= 'ovApR

ovAp', ovApR' :: Graph (a -> b) -> Graph (a -> b) -> Graph a -> Graph b
ovAp'  x y z = overlay x y <*> z
ovApR' x y z = overlay (x >>= (<$> z)) (y >>= (<$> z))

inspect $ 'ovAp' === 'ovApR'
