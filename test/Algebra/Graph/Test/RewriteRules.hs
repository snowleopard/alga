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
import qualified Data.Set as Set

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

-- 'foldg'
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

-- vertices
verticesC, verticesCR :: Build a -> Graph a
verticesC  xs = vertices (build xs)
verticesCR xs = fromMaybe Empty (xs (maybeF Overlay . Vertex) Nothing)

inspect $ 'verticesC === 'verticesCR

verticesP, verticesPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
verticesP  e v o c xs = foldg e v o c (vertices xs)
verticesPR e v o _ xs =
  fromMaybe e (foldr (maybeF o . v) Nothing xs)

inspect $ 'verticesP === 'verticesPR

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

-- clique
cliqueC, cliqueCR :: Build a -> Graph a
cliqueC  xs = clique (build xs)
cliqueCR xs = fromMaybe Empty (xs (maybeF Connect . Vertex) Nothing)

inspect $ 'cliqueC === 'cliqueCR

cliqueP, cliquePR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
cliqueP  e v o c xs = foldg e v o c (clique xs)
cliquePR e v _ c xs =
  fromMaybe e (foldr (maybeF c . v) Nothing xs)

inspect $ 'cliqueP === 'cliquePR

-- edges
edgesC, edgesCR :: Build (a,a) -> Graph a
edgesC  xs = edges (build xs)
edgesCR xs =
  fromMaybe Empty
    (xs (\e -> maybeF Overlay (Connect (Vertex (fst e)) (Vertex (snd e)))) Nothing)

inspect $ 'edgesC === 'edgesCR

edgesP, edgesPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [(a,a)] -> b
edgesP  e v o c xs = foldg e v o c (edges xs)
edgesPR e v o c xs =
  fromMaybe e (foldr (\e -> maybeF o (c (v (fst e)) (v (snd e)))) Nothing xs)

inspect $ 'edgesP === 'edgesPR

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

-- isEmpty
isEmptyC, isEmptyCR :: Buildg a -> Bool
isEmptyC  g = isEmpty (buildg g)
isEmptyCR g = g True (const False) (&&) (&&)

inspect $ 'isEmptyC === 'isEmptyCR

-- size
sizeC, sizeCR :: Buildg a -> Int
sizeC  g = size (buildg g)
sizeCR g = g 1 (const 1) (+) (+)

inspect $ 'sizeC === 'sizeCR

-- vertexSet
vertexSetC, vertexSetCR :: Ord a => Buildg a -> Set.Set a
vertexSetC  g = vertexSet (buildg g)
vertexSetCR g =
  g Set.empty Set.singleton Set.union Set.union

inspect $ 'vertexSetC === 'vertexSetCR

-- vertexCount
vertexCountC, vertexCountCR :: Ord a => Buildg a -> Int
vertexCountC  g = vertexCount (buildg g)
vertexCountCR g =
  Set.size (g Set.empty Set.singleton Set.union Set.union)

inspect $ 'vertexSetC === 'vertexSetCR

-- hasVertex
hasVertexC, hasVertexCR :: Eq a => a -> Buildg a -> Bool
hasVertexC  x g = hasVertex x (buildg g)
hasVertexCR x g = g False (==x) (||) (||)

inspect $ 'hasVertexC === 'hasVertexCR

-- path
pathP, pathPR :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
pathP  e v o c xs = foldg e v o c (path xs)
pathPR e v o c xs =
  case xs of
    []     -> e
    [x]    -> v x
    (_:ys) -> foldg e v o c $ edges (zip xs ys)
    -- edges is a good producer and consumer so this is optimized

inspect $ 'pathP === 'pathPR

-- circuit
circuitP, circuitPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
circuitP  e v o c xs = foldg e v o c (circuit xs)
circuitPR e v o c xs =
  case xs of
    [] -> e
    (x:xs) -> foldg e v o c $ path $ [x] ++ xs ++ [x]
    -- path is a good producer and consumer so this is optimized

inspect $ 'circuitP === 'circuitPR

-- biclique
bicliqueC, bicliqueCR :: Build a -> Build a -> Graph a
bicliqueC  xs ys = biclique (build xs) (build ys)
bicliqueCR xs ys =
  case xs (maybeF Overlay . Vertex) Nothing of
    Nothing -> vertices (build ys)
    -- vertices is a good consumer so this is optimized
    Just xs ->
      case ys (maybeF Overlay . Vertex) Nothing of
        Nothing -> xs
        Just ys -> Connect xs ys

inspect $ 'bicliqueC === 'bicliqueCR

bicliqueP, bicliquePR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> [a] -> b
bicliqueP  e v o c xs ys = foldg e v o c (biclique xs ys)
bicliquePR e v o c xs ys =
  case foldr1Safe o (map v xs) of
    Nothing -> foldg e v o c $ vertices ys
    -- vertices is a good producer so this is optimized
    Just xs ->
      case foldr1Safe o (map v ys) of
        Nothing -> xs
        Just ys -> c xs ys

inspect $ 'bicliqueP === 'bicliquePR

-- replaceVertex
replaceVertexC, replaceVertexCR :: Eq a => a -> a -> Buildg a -> Graph a
replaceVertexC  u v g = replaceVertex u v (buildg g)
replaceVertexCR u v g =
  g Empty (\w -> Vertex (if w == u then v else w)) Overlay Connect

inspect $ 'replaceVertexC === 'replaceVertexCR

replaceVertexP, replaceVertexPR :: Eq a => a -> a ->
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
replaceVertexP  u v e v' o c g =
  foldg e v' o c (replaceVertex u v g)
replaceVertexPR u v e v' o c g =
  foldg e (\w -> v' (if w == u then v else w)) o c g

inspect $ 'replaceVertexP === 'replaceVertexPR

-- mergeVertices
mergeVerticesC, mergeVerticesCR :: (a -> Bool) -> a -> Buildg a -> Graph a
mergeVerticesC  p v g = mergeVertices p v (buildg g)
mergeVerticesCR p v g =
  g Empty (\w -> Vertex (if p w then v else w)) Overlay Connect

inspect $ 'mergeVerticesC === 'mergeVerticesCR

mergeVerticesP, mergeVerticesPR :: (a -> Bool) -> a ->
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
mergeVerticesP  p v e v' o c g =
  foldg e v' o c (mergeVertices p v g)
mergeVerticesPR p v e v' o c g =
  foldg e (\w -> v' (if p w then v else w)) o c g

inspect $ 'mergeVerticesP === 'mergeVerticesPR

-- splitVertex
-- Good consumption if lists is guaranteed by `vertices`
splitVertexC, splitVertexCR :: Eq a => a -> [a] -> Buildg a -> Graph a
splitVertexC  x us g = splitVertex x us (buildg g)
splitVertexCR x us g =
  let gus = vertices us in
  g Empty (\w -> if w == x then gus else Vertex w) Overlay Connect

inspect $ 'splitVertexC === 'splitVertexCR

splitVertexP, splitVertexPR :: Eq a => a -> [a] ->
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
splitVertexP  x us e v o c g = foldg e v o c (splitVertex x us g)
splitVertexPR x us e v o c g =
  let gus = foldg e v o c (vertices us) in
  foldg e (\w -> if w == x then gus else v w) o c g

inspect $ 'splitVertexP === 'splitVertexPR

-- transpose
transposeC, transposeCR :: Buildg a -> Graph a
transposeC  g = transpose (buildg g)
transposeCR g = g Empty Vertex Overlay (flip Connect)

inspect $ 'transposeC === 'transposeCR

transposeP, transposePR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
transposeP  e v o c g = foldg e v o c (transpose g)
transposePR e v o c g = foldg e v o (flip c) g

inspect $ 'transposeP === 'transposePR

-- simplify
simple :: Eq g => (g -> g -> g) -> g -> g -> g
simple op x y
    | x == z    = x
    | y == z    = y
    | otherwise = z
  where
    z = op x y

simplifyC, simplifyCR :: Ord a => Buildg a -> Graph a
simplifyC  g = simplify (buildg g)
simplifyCR g = g Empty Vertex (simple Overlay) (simple Connect)

inspect $ 'simplifyC === 'simplifyCR
