{-# LANGUAGE TemplateHaskell, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.RewriteRules
-- Copyright  : (c) Andrey Mokhov 2016-2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph" rewrite rules.
-----------------------------------------------------------------------------
module Algebra.Graph.Test.RewriteRules where

import Data.Maybe (fromMaybe)

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Set                   as Set

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
verticesCP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Build a -> b
verticesCP e v o c xs = foldg e v o c (vertices (build xs))

inspect $ 'verticesCP `hasNoType` ''[]
inspect $ 'verticesCP `hasNoType` ''Graph

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
cliqueCP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Build a -> b
cliqueCP e v o c xs = foldg e v o c (clique (build xs))

inspect $ 'cliqueCP `hasNoType` ''[]
inspect $ 'cliqueCP `hasNoType` ''Graph

-- edges
edgesCP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Build (a,a) -> b
edgesCP e v o c xs = foldg e v o c (edges (build xs))

inspect $ 'edgesCP `hasNoType` ''[]
inspect $ 'edgesCP `hasNoType` ''Graph

-- star
starCP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> a -> Build a -> b
starCP e v o c x xs = foldg e v o c (star x (build xs))

inspect $ 'starCP `hasNoType` ''[]
inspect $ 'starCP `hasNoType` ''Graph

-- fmap
fmapCP ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (c -> a) -> Buildg c -> b
fmapCP  e v o c f g = foldg e v o c (fmap f (buildg g))

inspect $ 'fmapCP `hasNoType` ''Graph

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
apC, apCR :: Buildg (a -> b) -> Graph a -> Graph b
apC  f x = buildg f <*> x
apCR f x = f Empty (\v -> foldg Empty (Vertex . v) Overlay Connect x) Overlay Connect

inspect $ 'apC === 'apCR

apP, apPR ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph (c -> a) -> Graph c -> b
apP  e v o c f x = foldg e v o c (f <*> x)
apPR e v o c f x =
  foldg e (\w -> foldg e (v . w) o c x) o c f

inspect $ 'apP === 'apPR

-- eq
eqC :: Ord a => Buildg a -> Buildg a -> Bool
eqC x y = buildg x == buildg y

inspect $ 'eqC `hasNoType` ''Graph

eqT :: Graph Int -> Graph Int -> Bool
eqT x y = x == y

inspect $ 'eqT `hasNoType` ''AM.AdjacencyMap

-- ord
ordC :: Ord a => Buildg a -> Buildg a -> Ordering
ordC x y = compare (buildg x) (buildg y)

inspect $ 'ordC `hasNoType` ''Graph

ordT :: Graph Int -> Graph Int -> Ordering
ordT x y = compare x y

inspect $ 'ordT  `hasNoType` ''AM.AdjacencyMap

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

vertexCountT :: Graph Int -> Int
vertexCountT g = vertexCount g

inspect $ 'vertexCountT  `hasNoType` ''Set.Set

-- edgeCount
edgeCountC :: Ord a => Buildg a -> Int
edgeCountC g = edgeCount (buildg g)

inspect $ 'edgeCountC `hasNoType` ''Graph

edgeCountT :: Graph Int -> Int
edgeCountT g = edgeCount g

inspect $ 'edgeCountT `hasNoType` ''Set.Set

-- vertexList
vertexListCP :: Ord a => (a -> b -> b) -> b -> Buildg a -> b
vertexListCP k c g = foldr k c (vertexList (buildg g))

inspect $ 'vertexListCP `hasNoType` ''Graph
inspect $ 'vertexListCP `hasNoType` ''[]

vertexListT :: Graph Int -> [Int]
vertexListT g = vertexList g

inspect $ 'vertexListT `hasNoType` ''Set.Set

-- edgeSet
edgeSetC :: Ord a => Buildg a -> Set.Set (a,a)
edgeSetC g = edgeSet (buildg g)

inspect $ 'edgeSetC `hasNoType` ''Graph

edgeSetT :: Graph Int -> Set.Set (Int,Int)
edgeSetT g = edgeSet g

inspect $ 'vertexListT `hasNoType` ''AM.AdjacencyMap

-- edgeList
edgeListCP :: Ord a => ((a,a) -> b -> b) -> b -> Buildg a -> b
edgeListCP k c g = foldr k c (edgeList (buildg g))

inspect $ 'edgeListCP `hasNoType` ''Graph
inspect $ 'edgeListCP `hasNoType` ''[]

edgeListT :: Graph Int -> [(Int,Int)]
edgeListT g = edgeList g

inspect $ 'edgeListT `hasNoType` ''AM.AdjacencyMap

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
bicliqueCP :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Build a -> Build a -> b
bicliqueCP e v o c xs ys = foldg e v o c (biclique (build xs) (build ys))

inspect $ 'bicliqueCP `hasNoType` ''[]
inspect $ 'bicliqueCP `hasNoType` ''Graph

-- replaceVertex
replaceVertexCP :: Eq a => a -> a ->
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Buildg a -> b
replaceVertexCP u v e v' o c g =
  foldg e v' o c (replaceVertex u v (buildg g))

inspect $ 'replaceVertexCP `hasNoType` ''Graph

-- mergeVertices
mergeVerticesCP :: (a -> Bool) -> a ->
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Buildg a -> b
mergeVerticesCP p v e v' o c g =
  foldg e v' o c (mergeVertices p v (buildg g))

inspect $ 'mergeVerticesCP `hasNoType` ''Graph

-- splitVertex
splitVertexCP :: Eq a => a -> Build a ->
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Buildg a -> b
splitVertexCP x us e v o c g = foldg e v o c (splitVertex x (build us) (buildg g))

inspect $ 'splitVertexCP `hasNoType` ''[]
inspect $ 'splitVertexCP `hasNoType` ''Graph

-- transpose
transposeCP ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Buildg a -> b
transposeCP e v o c g = foldg e v o c (transpose (buildg g))

inspect $ 'transposeCP `hasNoType` ''Graph

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

-- compose
composeCP :: Ord a => b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Buildg a -> Buildg a -> b
composeCP e v o c x y = foldg e v o c $ compose (buildg x) (buildg y)

inspect $ 'composeCP `hasNoType` ''Graph

-- induce
induceCP ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (a -> Bool) -> Buildg a -> b
induceCP e v o c p g = foldg e v o c (induce p (buildg g))

inspect $ 'induceCP `hasNoType` ''Graph

-- induceJust
induceJustCP ::
  b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Buildg (Maybe a) -> b
induceJustCP e v o c g = foldg e v o c (induceJust (buildg g))

inspect $ 'induceJustCP `hasNoType` ''Graph

-- context
contextC :: (a -> Bool) -> Buildg a -> Maybe (Context a)
contextC p g = context p (buildg g)

inspect $ 'contextC `hasNoType` ''Graph
