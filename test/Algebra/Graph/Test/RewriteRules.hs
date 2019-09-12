{-# LANGUAGE TemplateHaskell, RankNTypes #-}
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

import GHC.Base (build)

import Test.Inspection

type Build  a = forall b. (a -> b -> b) -> b -> b
type Buildg a = forall b. b -> (a -> b) -> (b -> b ->b ) -> (b -> b-> b) -> b

-- Naming convention
--- We use:
--- * the suffix "R" to indicate the desired outcome of rewrite rules.
--- * the suffix "C" when testing the "good consumer" property.
--- * the suffix "P" when testing the "good producer" property.
--- * the suffix "I" when testing inlining.

-- 'foldg'.
emptyI, emptyIR :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> b
emptyI  e v o c = foldg e v o c Empty
emptyIR e _ _ _ = e

inspect $ 'emptyI === 'emptyIR

vertexI, vertexIR :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> a -> b
vertexI  e v o c x = foldg e v o c (Vertex x)
vertexIR _ v _ _ x = v x

inspect $ 'vertexI === 'vertexIR

overlayI, overlayIR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> Graph a -> b
overlayI  e v o c x y = foldg e v o c (Overlay x y)
overlayIR e v o c x y = o (foldg e v o c x) (foldg e v o c y)

inspect $ 'overlayI === 'overlayIR

connectI, connectIR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> Graph a -> b
connectI  e v o c x y = foldg e v o c (Connect x y)
connectIR e v o c x y = c (foldg e v o c x) (foldg e v o c y)

inspect $ 'connectI === 'connectIR

-- overlays
overlaysC, overlaysCR :: Build (Graph a) -> Graph a
overlaysC  xs = overlays (build xs)
overlaysCR xs = fromMaybe Empty (xs (maybeF Overlay) Nothing)

inspect $ 'overlaysC === 'overlaysCR

overlaysP, overlaysPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [Graph a] -> b
overlaysP  e v o c xs = foldg e v o c (overlays xs)
overlaysPR e v o c xs = fromMaybe e (foldr (maybeF o . foldg e v o c) Nothing xs)

inspect $ 'overlaysP === 'overlaysPR

-- connects
connectsC, connectsCR :: Build (Graph a) -> Graph a
connectsC  xs = connects (build xs)
connectsCR xs = fromMaybe Empty (xs (maybeF Connect) Nothing)

inspect $ 'connectsC === 'connectsCR

connectsP, connectsPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [Graph a] -> b
connectsP  e v o c xs = foldg e v o c (connects xs)
connectsPR e v o c xs = fromMaybe e (foldr (maybeF c . foldg e v o c) Nothing xs)

inspect $ 'connectsP === 'connectsPR

-- star
starC, starCR :: a -> Build a -> Graph a
starC  x xs = star x (build xs)
starCR x xs =
  case xs (maybeF Overlay . Vertex) Nothing of
    Nothing -> Vertex x
    Just vertices -> Connect (Vertex x) vertices

inspect $ 'starC === 'starCR

starP, starPR :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> a -> [a] -> b
starP  e v o c x xs = foldg e v o c (star x xs)
starPR _ v o c x xs =
  case foldr (maybeF o . v) Nothing xs of
    Nothing -> v x
    Just vertices -> c (v x) vertices

inspect $ 'starP === 'starPR

-- fmap
fmapC, fmapCR :: (a -> b) -> Buildg a -> Graph b
fmapC  f g = fmap f (buildg g)
fmapCR f g = g Empty (Vertex . f) Overlay Connect

inspect $ 'fmapC === 'fmapCR

fmapP, fmapPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (c -> a) -> Graph c -> b
fmapP  e v o c f g = foldg e v o c (fmap f g)
fmapPR e v o c f g = foldg e (v . f) o c g

inspect $ 'fmapP === 'fmapPR

-- bind
bindC, bindCR :: (a -> Graph b) -> Buildg a -> Graph b
bindC  f g = (buildg g) >>= f
bindCR f g = g Empty (\x -> f x) Overlay Connect

inspect $ 'bindC === 'bindCR

bindP, bindPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (c -> Graph a) -> Graph c -> b
bindP  e v o c f g = foldg e v o c (g >>= f)
bindPR e v o c f g = foldg e (foldg e v o c . f) o c g

inspect $ 'fmapP === 'fmapPR

-- ap
ovC, ovCR :: Buildg (a -> b) -> Graph a -> Graph b
ovC  f x = buildg f <*> x
ovCR f x = f Empty (\v -> foldg Empty (Vertex . v) Overlay Connect x) Overlay Connect

inspect $ 'ovC === 'ovCR

ovP, ovPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph (c -> a) -> Graph c -> b
ovP  e v o c f x = foldg e v o c (f <*> x)
ovPR e v o c f x =
  foldg e (\w -> foldg e (v . w) o c x) o c f

inspect $ 'ovP === 'ovPR

-- hasVertex
hasVertexC, hasVertexCR :: Eq a => a -> Buildg a -> Bool
hasVertexC  x g = hasVertex x (buildg g)
hasVertexCR x g = g False (==x) (||) (||)

inspect $ 'hasVertexC === 'hasVertexCR
