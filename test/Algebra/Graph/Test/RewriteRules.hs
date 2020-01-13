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

import qualified Algebra.Graph.AdjacencyIntMap as AIM
import qualified Data.Set                      as Set
import qualified Data.IntSet                   as IntSet

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
--- * the suffix "T" when testing specialisation for a type

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
overlaysC :: Build (Graph a) -> Graph a
overlaysC xs = overlays (build xs)

inspect $ 'overlaysC `hasNoType` ''[]

overlaysP, overlaysPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [Graph a] -> b
overlaysP  e v o c xs = foldg e v o c (overlays xs)
overlaysPR e v o c xs = fromMaybe e (foldr (maybeF o . foldg e v o c) Nothing xs)

inspect $ 'overlaysP === 'overlaysPR

-- vertices
verticesC :: Build a -> Graph a
verticesC xs = vertices (build xs)

inspect $ 'verticesC `hasNoType` ''[]

verticesP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
verticesP e v o c xs = foldg e v o c (vertices xs)

inspect $ 'verticesP `hasNoType` ''Graph

-- connects
connectsC :: Build (Graph a) -> Graph a
connectsC xs = connects (build xs)

inspect $ 'connectsC `hasNoType` ''[]

connectsP, connectsPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [Graph a] -> b
connectsP  e v o c xs = foldg e v o c (connects xs)
connectsPR e v o c xs = fromMaybe e (foldr (maybeF c . foldg e v o c) Nothing xs)

inspect $ 'connectsP === 'connectsPR

-- isSubgraphOf
isSubgraphOfC :: Ord a => Buildg a -> Buildg a -> Bool
isSubgraphOfC x y = isSubgraphOf (buildg x) (buildg y)

inspect $ 'isSubgraphOfC `hasNoType` ''Graph

-- clique
cliqueC :: Build a -> Graph a
cliqueC xs = clique (build xs)

inspect $ 'cliqueC `hasNoType` ''[]

cliqueP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
cliqueP e v o c xs = foldg e v o c (clique xs)

inspect $ 'cliqueP `hasNoType` ''Graph

-- edges
edgesC :: Build (a,a) -> Graph a
edgesC xs = edges (build xs)

inspect $ 'edgesC `hasNoType` ''[]

edgesP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [(a,a)] -> b
edgesP e v o c xs = foldg e v o c (edges xs)

inspect $ 'edgesP `hasNoType` ''Graph

-- star
starC :: a -> Build a -> Graph a
starC x xs = star x (build xs)

inspect $ 'starC `hasNoType` ''[]

starP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> a -> [a] -> b
starP e v o c x xs = foldg e v o c (star x xs)

inspect $ 'starP `hasNoType` ''Graph

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

inspect $ 'bindP === 'bindPR

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

-- eq
eqC :: Ord a => Buildg a -> Buildg a -> Bool
eqC x y = buildg x == buildg y

inspect $ 'eqC `hasNoType` ''Graph

eqT, eqTR :: Graph Int -> Graph Int -> Bool
eqT  x y = x == y
eqTR x y =
  foldg AIM.empty AIM.vertex AIM.overlay AIM.connect x == foldg AIM.empty AIM.vertex AIM.overlay AIM.connect y

inspect $ 'eqT === 'eqTR

-- ord
ordC :: Ord a => Buildg a -> Buildg a -> Ordering
ordC  x y = compare (buildg x) (buildg y)

inspect $ 'ordC `hasNoType` ''Graph

ordT, ordTR :: Graph Int -> Graph Int -> Ordering
ordT  x y = compare x y
ordTR x y =
  compare
    (foldg AIM.empty AIM.vertex AIM.overlay AIM.connect x)
    (foldg AIM.empty AIM.vertex AIM.overlay AIM.connect y)

inspect $ 'ordT === 'ordTR

-- isEmpty
isEmptyC :: Buildg a -> Bool
isEmptyC g = isEmpty (buildg g)

inspect $ 'isEmptyC `hasNoType` ''Graph

-- size
sizeC :: Buildg a -> Int
sizeC g = size (buildg g)

inspect $ 'sizeC `hasNoType` ''Graph

-- vertexSet
vertexSetC :: Ord a => Buildg a -> Set.Set a
vertexSetC g = vertexSet (buildg g)

inspect $ 'vertexSetC `hasNoType` ''Graph

-- vertexCount
vertexCountC :: Ord a => Buildg a -> Int
vertexCountC g = vertexCount (buildg g)

inspect $ 'vertexSetC `hasNoType` ''Graph

vertexCountT, vertexCountTR :: Graph Int -> Int
vertexCountT  g = vertexCount g
vertexCountTR g =
  IntSet.size (foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union g)

inspect $ 'vertexCountT === 'vertexCountTR

-- edgeCount
edgeCountC :: Ord a => Buildg a -> Int
edgeCountC g = edgeCount (buildg g)

inspect $ 'edgeCountC `hasNoType` ''Graph

edgeCountT, edgeCountTR :: Graph Int -> Int
edgeCountT  g = edgeCount g
edgeCountTR g =
  AIM.edgeCount (foldg AIM.empty AIM.vertex AIM.overlay AIM.connect g)

inspect $ 'edgeCountT === 'edgeCountTR

-- vertexList
vertexListC :: Ord a => Buildg a -> [a]
vertexListC g = vertexList (buildg g)

inspect $ 'vertexListC `hasNoType` ''Graph

vertexListT, vertexListTR :: Graph Int -> [Int]
vertexListT  g = vertexList g
vertexListTR g = IntSet.toAscList (foldg IntSet.empty IntSet.singleton IntSet.union IntSet.union g)

inspect $ 'vertexListT === 'vertexListTR

-- edgeSet
edgeSetC :: Ord a => Buildg a -> Set.Set (a,a)
edgeSetC g = edgeSet (buildg g)

inspect $ 'edgeSetC `hasNoType` ''Graph

edgeSetT, edgeSetTR :: Graph Int -> Set.Set (Int,Int)
edgeSetT  g = edgeSet g
edgeSetTR g = AIM.edgeSet (foldg AIM.empty AIM.vertex AIM.overlay AIM.connect g)

inspect $ 'edgeSetT === 'edgeSetTR

-- edgeList
edgeListC :: Ord a => Buildg a -> [(a,a)]
edgeListC g = edgeList (buildg g)

inspect $ 'edgeListC `hasNoType` ''Graph

edgeListT, edgeListTR :: Graph Int -> [(Int,Int)]
edgeListT  g = edgeList g
edgeListTR g =
  AIM.edgeList (foldg AIM.empty AIM.vertex AIM.overlay AIM.connect g)

inspect $ 'edgeListT === 'edgeListTR

-- hasVertex
hasVertexC :: Eq a => a -> Buildg a -> Bool
hasVertexC x g = hasVertex x (buildg g)

inspect $ 'hasVertexC `hasNoType` ''Graph

-- hasEdge
hasEdgeC :: Eq a => a -> a -> Buildg a -> Bool
hasEdgeC x y g = hasEdge x y (buildg g)

inspect $ 'hasEdgeC `hasNoType` ''Graph

-- adjacencyList
adjacencyListC :: Ord a => Buildg a -> [(a, [a])]
adjacencyListC g = adjacencyList (buildg g)

inspect $ 'adjacencyListC `hasNoType` ''Graph

-- path
pathP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
pathP e v o c xs = foldg e v o c (path xs)

inspect $ 'pathP `hasNoType` ''Graph

-- circuit
circuitP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> b
circuitP e v o c xs = foldg e v o c (circuit xs)

inspect $ 'circuitP `hasNoType` ''Graph

-- biclique
bicliqueC :: Build a -> Build a -> Graph a
bicliqueC xs ys = biclique (build xs) (build ys)

inspect $ 'bicliqueC `hasNoType` ''[]

bicliqueP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> [a] -> [a] -> b
bicliqueP e v o c xs ys = foldg e v o c (biclique xs ys)

inspect $ 'bicliqueP `hasNoType` ''Graph

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
