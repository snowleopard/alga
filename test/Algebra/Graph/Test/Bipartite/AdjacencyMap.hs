{-# LANGUAGE OverloadedLists, ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Bipartite.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2023
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Bipartite.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Bipartite.AdjacencyMap (
    -- * Testsuite
    testBipartiteAdjacencyMap,
    testBipartiteAdjacencyMapAlgorithm
    ) where

import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Bipartite.AdjacencyMap.Algorithm
import Algebra.Graph.Test
import Data.Either
import Data.Either.Extra
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Algebra.Graph.AdjacencyMap           as AM
import qualified Algebra.Graph.Bipartite.AdjacencyMap as B
import qualified Data.Bifunctor                       as Bifunctor
import qualified Data.Map.Strict                      as Map
import qualified Data.Set                             as Set
import qualified Data.Tuple
import qualified Algebra.Graph.Bipartite.AdjacencyMap as B

type AI   = AM.AdjacencyMap Int
type AII  = AM.AdjacencyMap (Either Int Int)
type BAII = AdjacencyMap Int Int
type MII  = Matching Int Int
type MIC  = Matching Int Char
type LII  = List Int Int

testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
    -- Help with type inference by shadowing overly polymorphic functions
    let consistent :: BAII -> Bool
        consistent = B.consistent
        show :: BAII -> String
        show = Prelude.show
        leftAdjacencyMap :: BAII -> Map Int (Set Int)
        leftAdjacencyMap = B.leftAdjacencyMap
        rightAdjacencyMap :: BAII -> Map Int (Set Int)
        rightAdjacencyMap = B.rightAdjacencyMap
        leftAdjacencyList :: BAII -> [(Int, [Int])]
        leftAdjacencyList = B.leftAdjacencyList
        rightAdjacencyList :: BAII -> [(Int, [Int])]
        rightAdjacencyList = B.rightAdjacencyList
        empty :: BAII
        empty = B.empty
        vertex :: Either Int Int -> BAII
        vertex = B.vertex
        leftVertex :: Int -> BAII
        leftVertex = B.leftVertex
        rightVertex :: Int -> BAII
        rightVertex = B.rightVertex
        edge :: Int -> Int -> BAII
        edge = B.edge
        isEmpty :: BAII -> Bool
        isEmpty = B.isEmpty
        hasLeftVertex :: Int -> BAII -> Bool
        hasLeftVertex = B.hasLeftVertex
        hasRightVertex :: Int -> BAII -> Bool
        hasRightVertex = B.hasRightVertex
        hasVertex :: Either Int Int -> BAII -> Bool
        hasVertex = B.hasVertex
        hasEdge :: Int -> Int -> BAII -> Bool
        hasEdge = B.hasEdge
        vertexCount :: BAII -> Int
        vertexCount = B.vertexCount
        edgeCount :: BAII -> Int
        edgeCount = B.edgeCount
        vertices :: [Int] -> [Int] -> BAII
        vertices = B.vertices
        edges :: [(Int, Int)] -> BAII
        edges = B.edges
        overlays :: [BAII] -> BAII
        overlays = B.overlays
        connects :: [BAII] -> BAII
        connects = B.connects
        swap :: BAII -> BAII
        swap = B.swap
        toBipartite :: AII -> BAII
        toBipartite = B.toBipartite
        toBipartiteWith :: Ord a => (a -> Either Int Int) -> AM.AdjacencyMap a -> BAII
        toBipartiteWith = B.toBipartiteWith
        fromBipartite :: BAII -> AII
        fromBipartite = B.fromBipartite
        biclique :: [Int] -> [Int] -> BAII
        biclique = B.biclique
        star :: Int -> [Int] -> BAII
        star = B.star
        stars :: [(Int, [Int])] -> BAII
        stars = B.stars
        removeLeftVertex :: Int -> BAII -> BAII
        removeLeftVertex = B.removeLeftVertex
        removeRightVertex :: Int -> BAII -> BAII
        removeRightVertex = B.removeRightVertex
        removeEdge :: Int -> Int -> BAII -> BAII
        removeEdge = B.removeEdge

    putStrLn "\n============ Bipartite.AdjacencyMap.Num ============"
    test "0                     == rightVertex 0" $
          0                     == rightVertex 0
    test "swap 1                == leftVertex 1" $
          swap 1                == leftVertex 1
    test "swap 1 + 2            == vertices [1] [2]" $
          swap 1 + 2            == vertices [1] [2]
    test "swap 1 * 2            == edge 1 2" $
          swap 1 * 2            == edge 1 2
    test "swap 1 + 2 * swap 3   == overlay (leftVertex 1) (edge 3 2)" $
          swap 1 + 2 * swap 3   == overlay (leftVertex 1) (edge 3 2)
    test "swap 1 * (2 + swap 3) == connect (leftVertex 1) (vertices [3] [2])" $
          swap 1 * (2 + swap 3) == connect (leftVertex 1) (vertices [3] [2])

    putStrLn "\n============ Bipartite.AdjacencyMap.Show ============"
    test "show empty                 == \"empty\"" $
          show empty                 == "empty"
    test "show 1                     == \"rightVertex 1\"" $
          show 1                     == "rightVertex 1"
    test "show (swap 2)              == \"leftVertex 2\"" $
          show (swap 2)              == "leftVertex 2"
    test "show 1 + 2                 == \"vertices [] [1,2]\"" $
          show (1 + 2)               == "vertices [] [1,2]"
    test "show (swap (1 + 2))        == \"vertices [1,2] []\"" $
          show (swap (1 + 2))        == "vertices [1,2] []"
    test "show (swap 1 * 2)          == \"edge 1 2\"" $
          show (swap 1 * 2)          == "edge 1 2"
    test "show (swap 1 * 2 * swap 3) == \"edges [(1,2),(3,2)]\"" $
          show (swap 1 * 2 * swap 3) == "edges [(1,2),(3,2)]"
    test "show (swap 1 * 2 + swap 3) == \"overlay (leftVertex 3) (edge 1 2)\"" $
          show (swap 1 * 2 + swap 3) == "overlay (leftVertex 3) (edge 1 2)"

    putStrLn "\n============ Bipartite.AdjacencyMap.Eq ============"
    test "(x == y) == (leftAdjacencyMap x == leftAdjacencyMap y && rightAdjacencyMap x == rightAdjacencyMap y)" $ \(x :: BAII) (y :: BAII) ->
          (x == y) == (leftAdjacencyMap x == leftAdjacencyMap y && rightAdjacencyMap x == rightAdjacencyMap y)

    putStrLn ""
    test "        x + y == y + x" $ \(x :: BAII) y ->
                  x + y == y + x
    test "  x + (y + z) == (x + y) + z" $ \(x :: BAII) y z ->
            x + (y + z) == (x + y) + z
    test "    x * empty == x" $ \(x :: BAII) ->
              x * empty == x
    test "    empty * x == x" $ \(x :: BAII) ->
              empty * x == x
    test "        x * y == y * x" $ \(x :: BAII) y ->
                  x * y == y * x
    test "  x * (y * z) == (x * y) * z" $ size10 $ \(x :: BAII) y z ->
            x * (y * z) == (x * y) * z
    test "  x * (y + z) == x * y + x * z" $ size10 $ \(x :: BAII) y z ->
            x * (y + z) == x * (y + z)
    test "  (x + y) * z == x * z + y * z" $ size10 $ \(x :: BAII) y z ->
            (x + y) * z == x * z + y * z
    test "    x * y * z == x * y + x * z + y * z" $ size10 $ \(x :: BAII) y z ->
              x * y * z == x * y + x * z + y * z
    test "    x + empty == x" $ \(x :: BAII) ->
              x + empty == x
    test "    empty + x == x" $ \(x :: BAII) ->
              empty + x == x
    test "        x + x == x" $ \(x :: BAII) ->
                  x + x == x
    test "x * y + x + y == x * y" $ \(x :: BAII) (y :: BAII) ->
          x * y + x + y == x * y
    test "    x * x * x == x * x" $ size10 $ \(x :: BAII) ->
              x * x * x == x * x

    putStrLn ""
    test " leftVertex x * leftVertex y  ==  leftVertex x + leftVertex y " $ \x y ->
           leftVertex x * leftVertex y  ==  leftVertex x + leftVertex y
    test "rightVertex x * rightVertex y == rightVertex x + rightVertex y" $ \x y ->
          rightVertex x * rightVertex y == rightVertex x + rightVertex y

    putStrLn "\n============ Bipartite.AdjacencyMap.leftAdjacencyMap ============"
    test "leftAdjacencyMap empty           == Map.empty" $
          leftAdjacencyMap empty           == Map.empty
    test "leftAdjacencyMap (leftVertex x)  == Map.singleton x Set.empty" $ \x ->
          leftAdjacencyMap (leftVertex x)  == Map.singleton x Set.empty
    test "leftAdjacencyMap (rightVertex x) == Map.empty" $ \x ->
          leftAdjacencyMap (rightVertex x) == Map.empty
    test "leftAdjacencyMap (edge x y)      == Map.singleton x (Set.singleton y)" $ \x y ->
          leftAdjacencyMap (edge x y)      == Map.singleton x (Set.singleton y)

    putStrLn "\n============ Bipartite.AdjacencyMap.rightAdjacencyMap ============"
    test "rightAdjacencyMap empty           == Map.empty" $
          rightAdjacencyMap empty           == Map.empty
    test "rightAdjacencyMap (leftVertex x)  == Map.empty" $ \x ->
          rightAdjacencyMap (leftVertex x)  == Map.empty
    test "rightAdjacencyMap (rightVertex x) == Map.singleton x Set.empty" $ \x ->
          rightAdjacencyMap (rightVertex x) == Map.singleton x Set.empty
    test "rightAdjacencyMap (edge x y)      == Map.singleton y (Set.singleton x)" $ \x y ->
          rightAdjacencyMap (edge x y)      == Map.singleton y (Set.singleton x)

    putStrLn "\n============ Bipartite.AdjacencyMap.empty ============"
    test "isEmpty empty           == True" $
          isEmpty empty           == True
    test "leftAdjacencyMap empty  == Map.empty" $
          leftAdjacencyMap empty  == Map.empty
    test "rightAdjacencyMap empty == Map.empty" $
          rightAdjacencyMap empty == Map.empty
    test "hasVertex x empty       == False" $ \x ->
          hasVertex x empty       == False

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertex ============"
    test "leftAdjacencyMap (leftVertex x)  == Map.singleton x Set.empty" $ \x ->
          leftAdjacencyMap (leftVertex x)  == Map.singleton x Set.empty
    test "rightAdjacencyMap (leftVertex x) == Map.empty" $ \x ->
          rightAdjacencyMap (leftVertex x) == Map.empty
    test "hasLeftVertex x (leftVertex y)   == (x == y)" $ \x y ->
          hasLeftVertex x (leftVertex y)   == (x == y)
    test "hasRightVertex x (leftVertex y)  == False" $ \x y ->
          hasRightVertex x (leftVertex y)  == False
    test "hasEdge x y (leftVertex z)       == False" $ \x y z ->
          hasEdge x y (leftVertex z)       == False

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertex ============"
    test "leftAdjacencyMap (rightVertex x)  == Map.empty" $ \x ->
          leftAdjacencyMap (rightVertex x)  == Map.empty
    test "rightAdjacencyMap (rightVertex x) == Map.singleton x Set.empty" $  \x ->
          rightAdjacencyMap (rightVertex x) == Map.singleton x Set.empty
    test "hasLeftVertex x (rightVertex y)   == False" $ \x y ->
          hasLeftVertex x (rightVertex y)   == False
    test "hasRightVertex x (rightVertex y)  == (x == y)" $ \x y ->
          hasRightVertex x (rightVertex y)  == (x == y)
    test "hasEdge x y (rightVertex z)       == False" $ \x y z ->
          hasEdge x y (rightVertex z)       == False

    putStrLn "\n============ Bipartite.AdjacencyMap.vertex ============"
    test "vertex . Left  == leftVertex" $ \x ->
         (vertex . Left) x == leftVertex x
    test "vertex . Right == rightVertex" $ \x ->
         (vertex . Right) x == rightVertex x

    putStrLn "\n============ Bipartite.AdjacencyMap.edge ============"
    test "edge x y                     == connect (leftVertex x) (rightVertex y)" $ \x y ->
          edge x y                     == connect (leftVertex x) (rightVertex y)
    test "leftAdjacencyMap (edge x y)  == Map.singleton x (Set.singleton y)" $ \x y ->
          leftAdjacencyMap (edge x y)  == Map.singleton x (Set.singleton y)
    test "rightAdjacencyMap (edge x y) == Map.singleton y (Set.singleton x)" $ \x y ->
          rightAdjacencyMap (edge x y) == Map.singleton y (Set.singleton x)
    test "hasEdge x y (edge x y)       == True" $ \x y ->
          hasEdge x y (edge x y)       == True
    test "hasEdge 1 2 (edge 2 1)       == False" $
          hasEdge 1 2 (edge 2 1)       == False

    putStrLn "\n============ Bipartite.AdjacencyMap.overlay ============"
    test "isEmpty     (overlay x y) == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty     (overlay x y) ==(isEmpty   x   && isEmpty   y)
    test "hasVertex z (overlay x y) == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z (overlay x y) ==(hasVertex z x || hasVertex z y)
    test "vertexCount (overlay x y) >= vertexCount x" $ \x y ->
          vertexCount (overlay x y) >= vertexCount x
    test "vertexCount (overlay x y) <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount (overlay x y) <= vertexCount x + vertexCount y
    test "edgeCount   (overlay x y) >= edgeCount x" $ \x y ->
          edgeCount   (overlay x y) >= edgeCount x
    test "edgeCount   (overlay x y) <= edgeCount x   + edgeCount y" $ \x y ->
          edgeCount   (overlay x y) <= edgeCount x   + edgeCount y

    putStrLn "\n============ Bipartite.AdjacencyMap.connect ============"
    test "connect (leftVertex x)     (leftVertex y)     == vertices [x,y] []" $ \x y ->
          connect (leftVertex x)     (leftVertex y)     == vertices [x,y] []
    test "connect (leftVertex x)     (rightVertex y)    == edge x y" $ \x y ->
          connect (leftVertex x)     (rightVertex y)    == edge x y
    test "connect (rightVertex x)    (leftVertex y)     == edge y x" $ \x y ->
          connect (rightVertex x)    (leftVertex y)     == edge y x
    test "connect (rightVertex x)    (rightVertex y)    == vertices [] [x,y]" $ \x y ->
          connect (rightVertex x)    (rightVertex y)    == vertices [] [x,y]
    test "connect (vertices xs1 ys1) (vertices xs2 ys2) == overlay (biclique xs1 ys2) (biclique xs2 ys1)" $ \xs1 ys1 xs2 ys2 ->
          connect (vertices xs1 ys1) (vertices xs2 ys2) == overlay (biclique xs1 ys2) (biclique xs2 ys1)
    test "isEmpty     (connect x y)                     == isEmpty   x   && isEmpty   y" $ \x y ->
          isEmpty     (connect x y)                     ==(isEmpty   x   && isEmpty   y)
    test "hasVertex z (connect x y)                     == hasVertex z x || hasVertex z y" $ \x y z ->
          hasVertex z (connect x y)                     ==(hasVertex z x || hasVertex z y)
    test "vertexCount (connect x y)                     >= vertexCount x" $ \x y ->
          vertexCount (connect x y)                     >= vertexCount x
    test "vertexCount (connect x y)                     <= vertexCount x + vertexCount y" $ \x y ->
          vertexCount (connect x y)                     <= vertexCount x + vertexCount y
    test "edgeCount   (connect x y)                     >= edgeCount x" $ \x y ->
          edgeCount   (connect x y)                     >= edgeCount x
    test "edgeCount   (connect x y)                     >= leftVertexCount x * rightVertexCount y" $ \x y ->
          edgeCount   (connect x y)                     >= leftVertexCount x * rightVertexCount y
    test "edgeCount   (connect x y)                     <= leftVertexCount x * rightVertexCount y + rightVertexCount x * leftVertexCount y + edgeCount x + edgeCount y" $ \x y ->
          edgeCount   (connect x y)                     <= leftVertexCount x * rightVertexCount y + rightVertexCount x * leftVertexCount y + edgeCount x + edgeCount y

    putStrLn "\n============ Bipartite.AdjacencyMap.vertices ============"
    test "vertices [] []                    == empty" $
          vertices [] []                    == empty
    test "vertices [x] []                   == leftVertex x" $ \x ->
          vertices [x] []                   == leftVertex x
    test "vertices [] [x]                   == rightVertex x" $ \x ->
          vertices [] [x]                   == rightVertex x
    test "vertices xs ys                    == overlays (map leftVertex xs ++ map rightVertex ys)" $ \xs ys ->
          vertices xs ys                    == overlays (map leftVertex xs ++ map rightVertex ys)
    test "hasLeftVertex  x (vertices xs ys) == elem x xs" $ \x xs ys ->
          hasLeftVertex  x (vertices xs ys) == elem x xs
    test "hasRightVertex y (vertices xs ys) == elem y ys" $ \y xs ys ->
          hasRightVertex y (vertices xs ys) == elem y ys

    putStrLn "\n============ Bipartite.AdjacencyMap.edges ============"
    test "edges []            == empty" $
          edges []            == empty
    test "edges [(x,y)]       == edge x y" $ \x y ->
          edges [(x,y)]       == edge x y
    test "edges               == overlays . map (uncurry edge)" $ \xs ->
          edges xs            == (overlays . map (uncurry edge)) xs
    test "hasEdge x y . edges == elem (x,y)" $ \x y es ->
         (hasEdge x y . edges) es == elem (x,y) es
    test "edgeCount   . edges == length . nub" $ \es ->
         (edgeCount   . edges) es == (length . nubOrd) es

    putStrLn "\n============ Bipartite.AdjacencyMap.overlays ============"
    test "overlays []        == empty" $
          overlays []        == empty
    test "overlays [x]       == x" $ \x ->
          overlays [x]       == x
    test "overlays [x,y]     == overlay x y" $ \x y ->
          overlays [x,y]     == overlay x y
    test "overlays           == foldr overlay empty" $ size10 $ \xs ->
          overlays xs        == foldr overlay empty xs
    test "isEmpty . overlays == all isEmpty" $ size10 $ \xs ->
         (isEmpty . overlays) xs == all isEmpty xs

    putStrLn "\n============ Bipartite.AdjacencyMap.connects ============"
    test "connects []        == empty" $
          connects []        == empty
    test "connects [x]       == x" $ \x ->
          connects [x]       == x
    test "connects [x,y]     == connect x y" $ \x y ->
          connects [x,y]     == connect x y
    test "connects           == foldr connect empty" $ size10 $ \xs ->
          connects xs        == foldr connect empty xs
    test "isEmpty . connects == all isEmpty" $ size10 $ \ xs ->
         (isEmpty . connects) xs == all isEmpty xs

    putStrLn "\n============ Bipartite.AdjacencyMap.swap ============"
    test "swap empty            == empty" $
          swap empty            == empty
    test "swap . leftVertex     == rightVertex" $ \x ->
         (swap . leftVertex) x  == rightVertex x
    test "swap (vertices xs ys) == vertices ys xs" $ \xs ys ->
          swap (vertices xs ys) == vertices ys xs
    test "swap (edge x y)       == edge y x" $ \x y ->
          swap (edge x y)       == edge y x
    test "swap . edges          == edges . map Data.Tuple.swap" $ \es ->
         (swap . edges) es      == (edges . map Data.Tuple.swap) es
    test "swap . swap           == id" $ \x ->
         (swap . swap) x        == x

    putStrLn "\n============ Bipartite.AdjacencyMap.toBipartite ============"
    test "toBipartite empty                      == empty" $
          toBipartite AM.empty                   == empty
    test "toBipartite (vertex (Left x))          == leftVertex x" $ \x ->
          toBipartite (AM.vertex (Left x))       == leftVertex x
    test "toBipartite (vertex (Right x))         == rightVertex x" $ \x ->
          toBipartite (AM.vertex (Right x))      == rightVertex x
    test "toBipartite (edge (Left x) (Left y))   == vertices [x,y] []" $ \x y ->
          toBipartite (AM.edge (Left x) (Left y)) == vertices [x,y] []
    test "toBipartite (edge (Left x) (Right y))  == edge x y" $ \x y ->
          toBipartite (AM.edge (Left x) (Right y)) == edge x y
    test "toBipartite (edge (Right x) (Left y))  == edge y x" $ \x y ->
          toBipartite (AM.edge (Right x) (Left y)) == edge y x
    test "toBipartite (edge (Right x) (Right y)) == vertices [] [x,y]" $ \x y ->
          toBipartite (AM.edge (Right x) (Right y)) == vertices [] [x,y]
    test "toBipartite . clique                   == uncurry biclique . partitionEithers" $ \xs ->
         (toBipartite . AM.clique) xs            == (uncurry biclique . partitionEithers) xs
    test "toBipartite . fromBipartite            == id" $ \x ->
         (toBipartite . fromBipartite) x         == x

    putStrLn "\n============ Bipartite.AdjacencyMap.toBipartiteWith ============"
    test "toBipartiteWith f empty == empty" $ \(apply -> f) ->
          toBipartiteWith f (AM.empty :: AII) == empty
    test "toBipartiteWith Left x  == vertices (vertexList x) []" $ \x ->
          toBipartiteWith Left x  == vertices (AM.vertexList x) []
    test "toBipartiteWith Right x == vertices [] (vertexList x)" $ \x ->
          toBipartiteWith Right x == vertices [] (AM.vertexList x)
    test "toBipartiteWith f       == toBipartite . gmap f" $ \(apply -> f) x ->
          toBipartiteWith f x     == (toBipartite . AM.gmap f) (x :: AII)
    test "toBipartiteWith id      == toBipartite" $ \x ->
          toBipartiteWith id x    == toBipartite x

    putStrLn "\n============ Bipartite.AdjacencyMap.fromBipartite ============"
    test "fromBipartite empty          == empty" $
          fromBipartite empty          == AM.empty
    test "fromBipartite (leftVertex x) == vertex (Left x)" $ \x ->
          fromBipartite (leftVertex x) == AM.vertex (Left x)
    test "fromBipartite (edge x y)     == edges [(Left x, Right y), (Right y, Left x)]" $ \x y ->
          fromBipartite (edge x y)     == AM.edges [(Left x, Right y), (Right y, Left x)]

    putStrLn "\n============ Bipartite.AdjacencyMap.fromBipartiteWith ============"
    test "fromBipartiteWith Left Right             == fromBipartite" $ \x ->
          fromBipartiteWith Left Right x           == fromBipartite x
    test "fromBipartiteWith id id (vertices xs ys) == vertices (xs ++ ys)" $ \xs ys ->
          fromBipartiteWith id id (vertices xs ys) == AM.vertices (xs ++ ys)
    test "fromBipartiteWith id id . edges          == symmetricClosure . edges" $ \xs ->
         (fromBipartiteWith id id . edges) xs      == (AM.symmetricClosure . AM.edges) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.isEmpty ============"
    test "isEmpty empty                 == True" $
          isEmpty empty                 == True
    test "isEmpty (overlay empty empty) == True" $
          isEmpty (overlay empty empty) == True
    test "isEmpty (vertex x)            == False" $ \x ->
          isEmpty (vertex x)            == False
    test "isEmpty                       == (==) empty" $ \x ->
          isEmpty x                     == (==) empty x

    putStrLn "\n============ Bipartite.AdjacencyMap.hasLeftVertex ============"
    test "hasLeftVertex x empty           == False" $ \x ->
          hasLeftVertex x empty           == False
    test "hasLeftVertex x (leftVertex y)  == (x == y)" $ \x y ->
          hasLeftVertex x (leftVertex y)  == (x == y)
    test "hasLeftVertex x (rightVertex y) == False" $ \x y ->
          hasLeftVertex x (rightVertex y) == False

    putStrLn "\n============ Bipartite.AdjacencyMap.hasRightVertex ============"
    test "hasRightVertex x empty           == False" $ \x ->
          hasRightVertex x empty           == False
    test "hasRightVertex x (leftVertex y)  == False" $ \x y ->
          hasRightVertex x (leftVertex y)  == False
    test "hasRightVertex x (rightVertex y) == (x == y)" $ \x y ->
          hasRightVertex x (rightVertex y) == (x == y)

    putStrLn "\n============ Bipartite.AdjacencyMap.hasVertex ============"
    test "hasVertex . Left  == hasLeftVertex" $ \x y ->
         (hasVertex . Left) x y == hasLeftVertex x y
    test "hasVertex . Right == hasRightVertex" $ \x y ->
         (hasVertex . Right) x y == hasRightVertex x y

    putStrLn "\n============ Bipartite.AdjacencyMap.hasEdge ============"
    test "hasEdge x y empty      == False" $ \x y ->
          hasEdge x y empty      == False
    test "hasEdge x y (vertex z) == False" $ \x y z ->
          hasEdge x y (vertex z) == False
    test "hasEdge x y (edge x y) == True" $ \x y ->
          hasEdge x y (edge x y) == True
    test "hasEdge x y            == elem (x,y) . edgeList" $ \x y z -> do
        let es = edgeList z
        (x, y) <- elements ((x, y) : es)
        return $ hasEdge x y z == elem (x, y) es

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexCount ============"
    test "leftVertexCount empty           == 0" $
          leftVertexCount empty           == 0
    test "leftVertexCount (leftVertex x)  == 1" $ \x ->
          leftVertexCount (leftVertex x)  == 1
    test "leftVertexCount (rightVertex x) == 0" $ \x ->
          leftVertexCount (rightVertex x) == 0
    test "leftVertexCount (edge x y)      == 1" $ \x y ->
          leftVertexCount (edge x y)      == 1
    test "leftVertexCount . edges         == length . nub . map fst" $ \xs ->
         (leftVertexCount . edges) xs     == (length . nub . map fst) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexCount ============"
    test "rightVertexCount empty           == 0" $
          rightVertexCount empty           == 0
    test "rightVertexCount (leftVertex x)  == 0" $ \x ->
          rightVertexCount (leftVertex x)  == 0
    test "rightVertexCount (rightVertex x) == 1" $ \x ->
          rightVertexCount (rightVertex x) == 1
    test "rightVertexCount (edge x y)      == 1" $ \x y ->
          rightVertexCount (edge x y)      == 1
    test "rightVertexCount . edges         == length . nub . map snd" $ \xs ->
         (rightVertexCount . edges) xs     == (length . nub . map snd) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexCount ============"
    test "vertexCount empty      == 0" $
          vertexCount empty      == 0
    test "vertexCount (vertex x) == 1" $ \x ->
          vertexCount (vertex x) == 1
    test "vertexCount (edge x y) == 2" $ \x y ->
          vertexCount (edge x y) == 2
    test "vertexCount x          == leftVertexCount x + rightVertexCount x" $ \x ->
          vertexCount x          == leftVertexCount x + rightVertexCount x

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeCount ============"
    test "edgeCount empty      == 0" $
          edgeCount empty      == 0
    test "edgeCount (vertex x) == 0" $ \x ->
          edgeCount (vertex x) == 0
    test "edgeCount (edge x y) == 1" $ \x y ->
          edgeCount (edge x y) == 1
    test "edgeCount . edges    == length . nub" $ \xs ->
         (edgeCount . edges) xs == (length . nubOrd) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexList ============"
    test "leftVertexList empty              == []" $
          leftVertexList empty              == []
    test "leftVertexList (leftVertex x)     == [x]" $ \x ->
          leftVertexList (leftVertex x)     == [x]
    test "leftVertexList (rightVertex x)    == []" $ \x ->
          leftVertexList (rightVertex x)    == []
    test "leftVertexList . flip vertices [] == nub . sort" $ \xs ->
         (leftVertexList . flip vertices []) xs == (nubOrd . sort) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexList ============"
    test "rightVertexList empty           == []" $
          rightVertexList empty           == []
    test "rightVertexList (leftVertex x)  == []" $ \x ->
          rightVertexList (leftVertex x)  == []
    test "rightVertexList (rightVertex x) == [x]" $ \x ->
          rightVertexList (rightVertex x) == [x]
    test "rightVertexList . vertices []   == nub . sort" $ \xs ->
         (rightVertexList . vertices []) xs == (nubOrd . sort) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexList ============"
    test "vertexList empty                             == []" $
          vertexList empty                             == []
    test "vertexList (vertex x)                        == [x]" $ \x ->
          vertexList (vertex x)                        == [x]
    test "vertexList (edge x y)                        == [Left x, Right y]" $ \x y ->
          vertexList (edge x y)                        == [Left x, Right y]
    test "vertexList (vertices (lefts xs) (rights xs)) == nub (sort xs)" $ \xs ->
          vertexList (vertices (lefts xs) (rights xs)) == nubOrd (sort xs)

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeList ============"
    test "edgeList empty      == []" $
          edgeList empty      == []
    test "edgeList (vertex x) == []" $ \x ->
          edgeList (vertex x) == []
    test "edgeList (edge x y) == [(x,y)]" $ \x y ->
          edgeList (edge x y) == [(x,y)]
    test "edgeList . edges    == nub . sort" $ \xs ->
         (edgeList . edges) xs == (nubOrd . sort) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexSet ============"
    test "leftVertexSet empty              == Set.empty" $
          leftVertexSet empty              == Set.empty
    test "leftVertexSet . leftVertex       == Set.singleton" $ \x ->
         (leftVertexSet . leftVertex) x    == Set.singleton x
    test "leftVertexSet . rightVertex      == const Set.empty" $ \x ->
         (leftVertexSet . rightVertex) x   == const Set.empty x
    test "leftVertexSet . flip vertices [] == Set.fromList" $ \xs ->
         (leftVertexSet . flip vertices []) xs == Set.fromList xs

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexSet ============"
    test "rightVertexSet empty         == Set.empty" $
          rightVertexSet empty         == Set.empty
    test "rightVertexSet . leftVertex  == const Set.empty" $ \x ->
         (rightVertexSet . leftVertex) x == const Set.empty x
    test "rightVertexSet . rightVertex == Set.singleton" $ \x ->
         (rightVertexSet . rightVertex) x == Set.singleton x
    test "rightVertexSet . vertices [] == Set.fromList" $ \xs ->
         (rightVertexSet . vertices []) xs == Set.fromList xs

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexSet ============"
    test "vertexSet empty                             == Set.empty" $
          vertexSet empty                             == Set.empty
    test "vertexSet . vertex                          == Set.singleton" $ \x ->
         (vertexSet . vertex) x                       == Set.singleton x
    test "vertexSet (edge x y)                        == Set.fromList [Left x, Right y]" $ \x y ->
          vertexSet (edge x y)                        == Set.fromList [Left x, Right y]
    test "vertexSet (vertices (lefts xs) (rights xs)) == Set.fromList xs" $ \xs ->
          vertexSet (vertices (lefts xs) (rights xs)) == Set.fromList xs

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeSet ============"
    test "edgeSet empty      == Set.empty" $
          edgeSet empty      == Set.empty
    test "edgeSet (vertex x) == Set.empty" $ \x ->
          edgeSet (vertex x) == Set.empty
    test "edgeSet (edge x y) == Set.singleton (x,y)" $ \x y ->
          edgeSet (edge x y) == Set.singleton (x,y)
    test "edgeSet . edges    == Set.fromList" $ \xs ->
         (edgeSet . edges) xs == Set.fromList xs

    putStrLn "\n============ Bipartite.AdjacencyMap.leftAdjacencyList ============"
    test "leftAdjacencyList empty            == []" $
          leftAdjacencyList empty            == []
    test "leftAdjacencyList (vertices [] xs) == []" $ \xs ->
          leftAdjacencyList (vertices [] xs) == []
    test "leftAdjacencyList (vertices xs []) == []" $ \xs ->
          leftAdjacencyList (vertices xs []) == [(x, []) | x <- nubOrd (sort xs)]
    test "leftAdjacencyList (edge x y)       == [(x, [y])]" $ \x y ->
          leftAdjacencyList (edge x y)       == [(x, [y])]
    test "leftAdjacencyList (star x ys)      == [(x, nub (sort ys))]" $ \x ys ->
          leftAdjacencyList (star x ys)      == [(x, nubOrd (sort ys))]

    putStrLn "\n============ Bipartite.AdjacencyMap.rightAdjacencyList ============"
    test "rightAdjacencyList empty            == []" $
          rightAdjacencyList empty            == []
    test "rightAdjacencyList (vertices [] xs) == [(x, []) | x <- nub (sort xs)]" $ \xs ->
          rightAdjacencyList (vertices [] xs) == [(x, []) | x <- nubOrd (sort xs)]
    test "rightAdjacencyList (vertices xs []) == []" $ \xs ->
          rightAdjacencyList (vertices xs []) == []
    test "rightAdjacencyList (edge x y)       == [(y, [x])]" $ \x y ->
          rightAdjacencyList (edge x y)       == [(y, [x])]
    test "rightAdjacencyList (star x ys)      == [(y, [x])  | y <- nub (sort ys)]" $ \x ys ->
          rightAdjacencyList (star x ys)      == [(y, [x])  | y <- nubOrd (sort ys)]

    putStrLn "\n============ Bipartite.AdjacencyMap.evenList ============"
    test "evenList []                 == Nil" $
          evenList []                 == Nil @Int @Int
    test "evenList [(1,2), (3,4)]     == [1, 2, 3, 4] :: List Int Int" $
          evenList [(1,2), (3,4)]     == ([1, 2, 3, 4] :: List Int Int)
    test "evenList [(1,'a'), (2,'b')] == Cons 1 (Cons 'a' (Cons 2 (Cons 'b' Nil)))" $
          evenList [(1,'a'), (2 :: Int,'b')] == Cons 1 (Cons 'a' (Cons 2 (Cons 'b' Nil)))

    putStrLn "\n============ Bipartite.AdjacencyMap.oddList ============"
    test "oddList 1 []                 == Cons 1 Nil" $
          oddList 1 []                 == Cons 1 (Nil @Int @Int)
    test "oddList 1 [(2,3), (4,5)]     == [1, 2, 3, 4, 5] :: List Int Int" $
          oddList 1 [(2,3), (4,5)]     ==([1, 2, 3, 4, 5] :: List Int Int)
    test "oddList 1 [('a',2), ('b',3)] == Cons 1 (Cons 'a' (Cons 2 (Cons 'b' (Cons 3 Nil))))" $
          oddList 1 [('a',2), ('b',3)] == Cons 1 (Cons 'a' (Cons 2 (Cons 'b' (Cons @Int 3 Nil))))

    putStrLn "\n============ Bipartite.AdjacencyMap.path ============"
    test "path Nil                   == empty" $
          path Nil                   == empty
    test "path (Cons x Nil)          == leftVertex x" $ \x ->
          path (Cons x Nil)          == leftVertex x
    test "path (Cons x (Cons y Nil)) == edge x y" $ \x y ->
          path (Cons x (Cons y Nil)) == edge x y
    test "path [1, 2, 3, 4, 5]       == edges [(1,2), (3,2), (3,4), (5,4)]" $
          path [1, 2, 3, 4, 5]       == edges [(1,2), (3,2), (3,4), (5,4)]

    putStrLn "\n============ Bipartite.AdjacencyMap.circuit ============"
    test "circuit []                    == empty" $
          circuit []                    == empty
    test "circuit [(x,y)]               == edge x y" $ \x y ->
          circuit [(x,y)]               == edge x y
    test "circuit [(1,2), (3,4), (5,6)] == edges [(1,2), (3,2), (3,4), (5,4), (5,6), (1,6)]" $
          circuit [(1,2), (3,4), (5,6)] == edges [(1,2), (3,2), (3,4), (5,4), (5,6), (1,6)]
    test "circuit . reverse             == swap . circuit . map Data.Tuple.swap" $ \xs ->
         (circuit . reverse) xs         == (swap . circuit . map Data.Tuple.swap) xs

    putStrLn "\n============ Bipartite.AdjacencyMap.biclique ============"
    test "biclique [] [] == empty" $
          biclique [] [] == empty
    test "biclique xs [] == vertices xs []" $ \xs ->
          biclique xs [] == vertices xs []
    test "biclique [] ys == vertices [] ys" $ \ys ->
          biclique [] ys == vertices [] ys
    test "biclique xs ys == connect (vertices xs []) (vertices [] ys)" $ \xs ys ->
          biclique xs ys == connect (vertices xs []) (vertices [] ys)

    putStrLn "\n============ Bipartite.AdjacencyMap.star ============"
    test "star x []    == leftVertex x" $ \x ->
          star x []    == leftVertex x
    test "star x [y]   == edge x y" $ \x y ->
          star x [y]   == edge x y
    test "star x [y,z] == edges [(x,y), (x,z)]" $ \x y z ->
          star x [y,z] == edges [(x,y), (x,z)]
    test "star x ys    == connect (leftVertex x) (vertices [] ys)" $ \x ys ->
          star x ys    == connect (leftVertex x) (vertices [] ys)

    putStrLn "\n============ Bipartite.AdjacencyMap.stars ============"
    test "stars []                      == empty" $
          stars []                      == empty
    test "stars [(x, [])]               == leftVertex x" $ \x ->
          stars [(x, [])]               == leftVertex x
    test "stars [(x, [y])]              == edge x y" $ \x y ->
          stars [(x, [y])]              == edge x y
    test "stars [(x, ys)]               == star x ys" $ \x ys ->
          stars [(x, ys)]               == star x ys
    test "star x [y,z]                  == edges [(x,y), (x,z)]" $ \x y z ->
          star x [y,z]                  == edges [(x,y), (x,z)]
    test "stars                         == overlays . map (uncurry star)" $ \xs ->
          stars xs                      == (overlays . map (uncurry star)) xs
    test "overlay (stars xs) (stars ys) == stars (xs ++ ys)" $ \xs ys ->
          overlay (stars xs) (stars ys) == stars (xs ++ ys)

    putStrLn "\n============ Bipartite.AdjacencyMap.mesh ============"
    test "mesh xs []           == empty" $ \xs ->
          mesh xs []           == B.empty @(Int,Int)
    test "mesh [] ys           == empty" $ \ys ->
          mesh [] ys           == B.empty @(Int,Int)
    test "mesh [x] [y]         == leftVertex (x,y)" $ \x y ->
          mesh [x] [y]         == B.leftVertex @(Int,Int) (x,y)
    test "mesh [1,1] ['a','b'] == biclique [(1,'a'), (1,'b')] [(1,'a'), (1,'b')]" $
          mesh [1,1] ['a','b'] == B.biclique @(Int,Char) [(1,'a'), (1,'b')] [(1,'a'), (1,'b')]
    test "mesh [1,2] ['a','b'] == biclique [(1,'a'), (2,'b')] [(1,'b'), (2,'a')]" $
          mesh [1,2] ['a','b'] == B.biclique @(Int,Char) [(1,'a'), (2,'b')] [(1,'b'), (2,'a')]

    putStrLn "\n============ Bipartite.AdjacencyMap.removeLeftVertex ============"
    test "removeLeftVertex x (leftVertex x)       == empty" $ \x ->
          removeLeftVertex x (leftVertex x)       == empty
    test "removeLeftVertex 1 (leftVertex 2)       == leftVertex 2" $
          removeLeftVertex 1 (leftVertex 2)       ==(leftVertex 2 :: BAII)
    test "removeLeftVertex x (rightVertex y)      == rightVertex y" $ \x y ->
          removeLeftVertex x (rightVertex y)      == rightVertex y
    test "removeLeftVertex x (edge x y)           == rightVertex y" $ \x y ->
          removeLeftVertex x (edge x y)           == rightVertex y
    test "removeLeftVertex x . removeLeftVertex x == removeLeftVertex x" $ \x (g :: BAII)->
         (removeLeftVertex x . removeLeftVertex x) g == removeLeftVertex x g

    putStrLn "\n============ Bipartite.AdjacencyMap.removeRightVertex ============"
    test "removeRightVertex x (rightVertex x)       == empty" $ \x ->
          removeRightVertex x (rightVertex x)       == empty
    test "removeRightVertex 1 (rightVertex 2)       == rightVertex 2" $
          removeRightVertex 1 (rightVertex 2)       ==(rightVertex 2 :: BAII)
    test "removeRightVertex x (leftVertex y)        == leftVertex y" $ \x y ->
          removeRightVertex x (leftVertex y)        == leftVertex y
    test "removeRightVertex y (edge x y)            == leftVertex x" $ \x y ->
          removeRightVertex y (edge x y)            == leftVertex x
    test "removeRightVertex x . removeRightVertex x == removeRightVertex x" $ \x (y :: BAII)->
         (removeRightVertex x . removeRightVertex x) y == removeRightVertex x y

    putStrLn "\n============ Bipartite.AdjacencyMap.removeEdge ============"
    test "removeEdge x y (edge x y)            == vertices [x] [y]" $ \x y ->
          removeEdge x y (edge x y)            == vertices [x] [y]
    test "removeEdge x y . removeEdge x y      == removeEdge x y" $ \x y z ->
         (removeEdge x y . removeEdge x y) z   == removeEdge x y z
    test "removeEdge x y . removeLeftVertex x  == removeLeftVertex x" $ \x y z ->
         (removeEdge x y . removeLeftVertex x) z == removeLeftVertex x z
    test "removeEdge x y . removeRightVertex y == removeRightVertex y" $ \x y z ->
         (removeEdge x y . removeRightVertex y) z == removeRightVertex y z

    putStrLn "\n============ Bipartite.AdjacencyMap.bimap ============"
    test "bimap f g empty           == empty" $ \(apply -> f) (apply -> g) ->
          bimap f g empty           == empty
    test "bimap f g . vertex        == vertex . Data.Bifunctor.bimap f g" $ \(apply -> f) (apply -> g) x ->
         (bimap f g . vertex) x     ==(vertex .      Bifunctor.bimap f g) x
    test "bimap f g (edge x y)      == edge (f x) (g y)" $ \(apply -> f) (apply -> g) x y ->
          bimap f g (edge x y)      == edge (f x) (g y)
    test "bimap id id               == id" $ \(x :: BAII) ->
          bimap id id x             == id x
    test "bimap f1 g1 . bimap f2 g2 == bimap (f1 . f2) (g1 . g2)" $ \(apply -> f1 :: Int -> Int) (apply -> g1 :: Int -> Int) (apply -> f2 :: Int -> Int) (apply -> g2 :: Int -> Int) x ->
         (bimap f1 g1 . bimap f2 g2) x == bimap (f1 . f2) (g1 . g2) x

    putStrLn "\n============ Bipartite.AdjacencyMap.box ============"
    test "box (path [0,1]) (path ['a','b']) == <correct result>" $
          box (path [0,1]) (path ['a','b']) == B.edges @(Int,Char) [ ((0,'a'), (0,'b'))
                                                                   , ((0,'a'), (1,'a'))
                                                                   , ((1,'b'), (0,'b'))
                                                                   , ((1,'b'), (1,'a')) ]
    let unit x = (x, ())
        biunit = B.bimap unit unit
        comm (x, y) = (y, x)
        bicomm = B.bimap comm comm
        assoc ((x, y), z) = (x, (y, z))
        biassoc = B.bimap assoc assoc

    putStrLn ""
    test "box x y                ~~ box y x" $ size10 $ \(x :: BAII) (y :: BAII) ->
          box x y                == bicomm (box y x)
    test "box x (box y z)        ~~ box (box x y) z" $ size10 $ \(x :: BAII) (y :: BAII) (z :: BAII) ->
          box x (box y z)        == biassoc (box (box x y) z)
    test "box x (box y z)        ~~ box (box x y) z" $ mapSize (min 3) $ \(x :: BAII) (y :: BAII) (z :: BAII) ->
          box x (box y z)        == biassoc (box (box x y) z)
    test "box x (leftVertex ())  ~~ x" $ size10 $ \(x :: BAII) ->
          box x (B.leftVertex ()) == biunit x
    test "box x (rightVertex ()) ~~ swap x" $ size10 $ \(x :: BAII) ->
          box x (B.rightVertex ()) == biunit (B.swap x)
    test "box x empty            ~~ empty" $ size10 $ \(x :: BAII) ->
          box x B.empty          == biunit empty
    test "vertexCount (box x y)  <= vertexCount x * vertexCount y" $ size10 $ \(x :: BAII) (y :: BAII) ->
        B.vertexCount (box x y)  <= vertexCount x * vertexCount y
    test "edgeCount (box x y)    <= vertexCount x * edgeCount y + edgeCount x * vertexCount y" $ size10 $ \(x :: BAII) (y :: BAII) ->
        B.edgeCount (box x y)    <= vertexCount x * edgeCount y + edgeCount x * vertexCount y

    putStrLn ""
    test "box == boxWith (,) (,) (,) (,)" $ size10 $ \(x :: BAII) (y :: BAII) ->
          box x y == boxWith (,) (,) (,) (,) x y

    putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
    test "consistent empty            == True" $
          consistent empty            == True
    test "consistent (vertex x)       == True" $ \x ->
          consistent (vertex x)       == True
    test "consistent (edge x y)       == True" $ \x y ->
          consistent (edge x y)       == True
    test "consistent (edges x)        == True" $ \x ->
          consistent (edges x)        == True
    test "consistent (toBipartite x)  == True" $ \x ->
          consistent (toBipartite x)  == True
    test "consistent (swap x)         == True" $ \x ->
          consistent (swap x)         == True
    test "consistent (circuit xs)     == True" $ \xs ->
          consistent (circuit xs)     == True
    test "consistent (biclique xs ys) == True" $ \xs ys ->
          consistent (biclique xs ys) == True

testBipartiteAdjacencyMapAlgorithm :: IO ()
testBipartiteAdjacencyMapAlgorithm = do
    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.detectParts ============"
    test "detectParts empty                                       == Right empty" $
          detectParts (AM.empty :: AI)                            == Right empty
    test "detectParts (vertex 1)                                  == Right (leftVertex 1)" $
          detectParts (AM.vertex 1 :: AI)                         == Right (leftVertex 1)
    test "detectParts (edge 1 1)                                  == Left [1]" $
          detectParts (AM.edge 1 1 :: AI)                         == Left [1]
    test "detectParts (edge 1 2)                                  == Right (edge 1 2)" $
          detectParts (AM.edge 1 2 :: AI)                         == Right (edge 1 2)
    test "detectParts (edge 0 (-1))                               == Right (edge (-1) 0)" $
          detectParts (AM.edge 0 (-1) :: AI)                      == Right (edge (-1) 0)
    test "detectParts (1 * (2 + 3))                               == Right (edges [(1, 2), (1, 3)])" $
          detectParts (1 * (2 + 3) :: AI)                         == Right (edges [(1, 2), (1, 3)])
    test "detectParts ((1 + 3) * (2 + 4) + 6 * 5)                 == Right (swap (1 + 3) * (2 + 4) + swap 5 * 6" $
          detectParts ((1 + 3) * (2 + 4) + 6 * 5 :: AI)           == Right (swap (1 + 3) * (2 * 4) + swap 5 * 6)
    test "detectParts ((1 + 2) * (3 + 4) * (5 + 6))               == Left [1, 3, 2, 4, 5]" $
          detectParts ((1 + 2) * (3 + 4) * (5 + 6) :: AI)         == Left [1, 3, 2, 4, 5]
    test "detectParts ((1 + 2) * (3 + 4) + (3 + 4) * 5)           == Right (swap (1 + 2) * (3 + 4) + swap 5 * (3 + 4))" $
          detectParts ((1 + 2) * (3 + 4) + (3 + 4) * 5 :: AI)     == Right (swap (1 + 2) * (3 + 4) + swap 5 * (3 + 4))
    test "detectParts (1 * 2 * 3)                                 == Left [2, 3, 1]" $
          detectParts (1 * 2 * 3 :: AI)                           == Left [1, 2, 3]
    test "detectParts ((1 * 3 * 4) + 2 * (1 + 2))                 == Left [2]" $
          detectParts ((1 * 3 * 4) + 2 * (1 + 2) :: AI)           == Left [2]
    test "detectParts (clique [1..10])                            == Left [1, 2, 3]" $
          detectParts (AM.clique [1..10] :: AI)                   == Left [1, 2, 3]
    test "detectParts (circuit [1..11])                           == Left [1..11]" $
          detectParts (AM.circuit [1..11] :: AI)                  == Left [1..11]
    test "detectParts (circuit [1..10])                           == Right (circuit [(2 * x - 1, 2 * x) | x <- [1..5]])" $
          detectParts (AM.circuit [1..10] :: AI)                  == Right (circuit [(2 * x - 1, 2 * x) | x <- [1..5]])
    test "detectParts (biclique [] xs)                            == Right (vertices xs [])" $ \(xs :: [Int]) ->
          detectParts (AM.biclique [] xs :: AI)                   == Right (vertices xs [])
    test "detectParts (biclique (map Left (x:xs)) (map Right ys)) == Right (biclique (map Left (x:xs)) (map Right ys))" $ \(x :: Int) (xs :: [Int]) (ys :: [Int]) ->
          detectParts (AM.biclique (map Left (x:xs)) (map Right ys)) == Right (biclique (map Left (x:xs)) (map Right ys))
    test "isRight (detectParts (star x ys))                       == not (elem x ys)" $ \(x :: Int) (ys :: [Int]) ->
          isRight (detectParts (AM.star x ys))                    == (not $ elem x ys)
    test "isRight (detectParts (fromBipartite (toBipartite x)))   == True" $ \(x :: AII) ->
          isRight (detectParts (fromBipartite (toBipartite x)))   == True

    -- TODO: Clean up these tests
    putStrLn ""
    test "((all ((flip Set.member) $ edgeSet $ symmetricClosure x) . edgeSet) <$> detectParts x) /= Right False" $ \(x :: AI) ->
          ((all ((flip Set.member) $ AM.edgeSet $ AM.symmetricClosure x) . edgeSet) <$> detectParts x) /= Right False
    test "(Set.map $ fromEither) <$> (vertexSet <$> (detectParts (fromBipartite (toBipartite x)))) == Right (vertexSet x)" $ \(x :: AII) ->
         ((Set.map $ fromEither) <$> (vertexSet <$> (detectParts (fromBipartite (toBipartite x))))) == Right (AM.vertexSet x)
    test "fromEither (Bifunctor.bimap ((flip Set.isSubsetOf) (vertexSet x) . Set.fromList) (const True) (detectParts x)) == True" $ \(x :: AI) ->
          fromEither (Bifunctor.bimap ((flip Set.isSubsetOf) (AM.vertexSet x) . Set.fromList) (const True) (detectParts x))
    test "fromEither (Bifunctor.bimap ((flip Set.isSubsetOf) (edgeSet (symmetricClosure x)) . AM.edgeSet . circuit) (const True) (detectParts x)) == True" $ \(x :: AI) ->
          fromEither (Bifunctor.bimap ((flip Set.isSubsetOf) (AM.edgeSet (AM.symmetricClosure x)) . AM.edgeSet . AM.circuit) (const True) (detectParts x))
    test "fromEither (Bifunctor.bimap (((==) 1) . ((flip mod) 2) . length) (const True) (detectParts x)) == True" $ \(x :: AI) ->
          fromEither (Bifunctor.bimap (((==) 1) . ((flip mod) 2) . length) (const True) (detectParts x))

    putStrLn "\n============ Show (Bipartite.AdjacencyMap.Algorithm.Matching a b) ============"
    test "show (matching [])                == \"matching []\"" $
          show (matching [] :: MII)         ==  "matching []"
    test "show (matching [(2,'a'),(1,'b')]) == \"matching [(1,'b'),(2,'a')]\"" $
          show (matching [(2,'a'),(1,'b')] :: MIC) == "matching [(1,'b'),(2,'a')]"

    putStrLn "\n============ Eq (Bipartite.AdjacencyMap.Algorithm.Matching a b) ============"
    test "(x == y) == ((pairOfLeft x == pairOfLeft y) && (pairOfRight x == pairOfRight y))" $ \(x :: MII) (y :: MII) ->
        (x == y) == ((pairOfLeft x == pairOfLeft y) && (pairOfRight x == pairOfRight y))

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.pairOfLeft ============"
    test "pairOfLeft (matching [])                 == Map.empty" $
          pairOfLeft (matching [] :: MII)          == Map.empty
    test "pairOfLeft (matching [(2,'a'), (1,'b')]) == Map.fromList [(2,'a'), (1,'b')]" $
          pairOfLeft (matching [(2,'a'), (1,'b')] :: MIC) == Map.fromList [(2,'a'), (1,'b')]
    test "Map.size . pairOfLeft                    == Map.size . pairOfRight" $ \(x :: MII) ->
         (Map.size . pairOfLeft) x                 ==(Map.size . pairOfRight) x

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.pairOfRight ============"
    test "pairOfRight (matching [])                 == Map.empty" $
          pairOfRight (matching [] :: MII)          == Map.empty
    test "pairOfRight (matching [(2,'a'), (1,'b')]) == Map.fromList [('a',2), ('b',1)]" $
          pairOfRight (matching [(2,'a'), (1,'b')] :: MIC) == Map.fromList [('a',2), ('b',1)]
    test "Map.size . pairOfRight                    == Map.size . pairOfLeft" $ \(x :: MII) ->
         (Map.size . pairOfRight) x                 ==(Map.size . pairOfLeft) x

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.matching ============"
    test "matching [(1,'a'), (1,'b')]                   == matching [(1,'b')]" $
          matching [(1,'a'), (1,'b')]                   == (matching [(1,'b')] :: MIC)
    test "matching [(1,'a'), (1,'b'), (2,'b'), (2,'a')] == matching [(2,'a')]" $
          matching [(1,'a'), (1,'b'), (2,'b'), (2,'a')] == (matching [(2,'a')] :: MIC)

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.isMatchingOf ============"
    test "isMatchingOf (matching []) x               == True" $ \(x :: BAII) ->
          isMatchingOf (matching []) x               == True
    test "isMatchingOf (matching xs) empty           == null xs" $ \(xs :: [(Int, Int)]) ->
          isMatchingOf (matching xs) empty           == null xs
    test "isMatchingOf (matching [(x,y)]) (edge x y) == True" $ \(x :: Int) (y :: Int) ->
          isMatchingOf (matching [(x,y)]) (edge x y) == True
    test "isMatchingOf (matching [(1,2)]) (edge 2 1) == False" $
          isMatchingOf (matching [(1,2)]) (edge 2 1 :: BAII) == False

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.matchingSize ============"
    test "matchingSize (matching [])                 == 0" $
          matchingSize (matching [] :: MII)          == 0
    test "matchingSize (matching [(2,'a'), (1,'b')]) == 2" $
          matchingSize (matching [(2,'a'), (1,'b')] :: MIC) == 2
    test "matchingSize (matching [(1,'a'), (1,'b')]) == 1" $
          matchingSize (matching [(1,'a'), (1,'b')] :: MIC) == 1
    test "matchingSize (matching xs)                 <= length xs" $ \(xs :: [(Int, Int)]) ->
          matchingSize (matching xs)                 <= length xs
    test "matchingSize x                             == Map.size . pairOfLeft" $ \(x :: MII) ->
          matchingSize x                             ==(Map.size . pairOfLeft) x

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.maxMatching ============"
    test "maxMatching empty                                          == matching []" $
          maxMatching (empty :: BAII)                                == matching []
    test "maxMatching (vertices xs ys)                               == matching []" $ \(xs :: [Int]) (ys :: [Int]) ->
          maxMatching (vertices xs ys)                               == matching []
    test "maxMatching (path [1,2,3,4])                               == matching [(1,2), (3,4)]" $
          maxMatching (path ([1,2,3,4] :: LII))                      == matching [(1,2), (3,4)]
    test "matchingSize (maxMatching (circuit [(1,2), (3,4), (5,6)])) == 3" $
          matchingSize (maxMatching (circuit [(1,2), (3,4), (5,6)] :: BAII)) == 3
    test "matchingSize (maxMatching (star x (y:ys)))                 == 1" $ \(x :: Int) (y :: Int) (ys :: [Int]) ->
          matchingSize (maxMatching (star x (y:ys)))                 == 1
    test "matchingSize (maxMatching (biclique xs ys))                == min (length (nub xs)) (length (nub ys))" $ \(xs :: [Int]) (ys :: [Int]) ->
          matchingSize (maxMatching (biclique xs ys))                == min (length (nub xs)) (length (nub ys))
    test "isMatchingOf (maxMatching x) x                             == True" $ \(x :: BAII) ->
          isMatchingOf (maxMatching x) x                             == True

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.isVertexCoverOf ============"
    test "isVertexCoverOf (xs             , ys             ) empty          == Set.null xs && Set.null ys" $ \(xs :: Set Int) (ys :: Set Int) ->
          isVertexCoverOf (xs             , ys             ) empty          ==(Set.null xs && Set.null ys)
    test "isVertexCoverOf (xs             , ys             ) (leftVertex x) == Set.isSubsetOf xs (Set.singleton x) && Set.null ys" $ \(x :: Int) (xs :: Set Int) (ys :: Set Int) ->
          isVertexCoverOf (xs             , ys             ) (leftVertex x) ==(Set.isSubsetOf xs (Set.singleton x) && Set.null ys)
    test "isVertexCoverOf (Set.empty      , Set.empty      ) (edge x y)     == False" $ \(x :: Int) (y :: Int) ->
          isVertexCoverOf (Set.empty      , Set.empty      ) (edge x y)     == False
    test "isVertexCoverOf (Set.singleton x, ys             ) (edge x y)     == Set.isSubsetOf ys (Set.singleton y)" $ \(x :: Int) (y :: Int) (ys :: Set Int) ->
          isVertexCoverOf (Set.singleton x, ys             ) (edge x y)     == Set.isSubsetOf ys (Set.singleton y)
    test "isVertexCoverOf (xs             , Set.singleton y) (edge x y)     == Set.isSubsetOf xs (Set.singleton x)" $ \(x :: Int) (y :: Int) (xs :: Set Int) ->
          isVertexCoverOf (xs             , Set.singleton y) (edge x y)     == Set.isSubsetOf xs (Set.singleton x)

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.minVertexCover ============"
    test "minVertexCover empty                              == (Set.empty, Set.empty)" $
          minVertexCover (empty :: BAII)                    == (Set.empty, Set.empty)
    test "minVertexCover (vertices xs ys)                   == (Set.empty, Set.empty)" $ \(xs :: [Int]) (ys :: [Int]) ->
          minVertexCover (vertices xs ys)                   == (Set.empty, Set.empty)
    test "minVertexCover (path [1,2,3])                     == (Set.empty, Set.singleton 2)" $
          minVertexCover (path [1,2,3] :: BAII)             == (Set.empty, Set.singleton 2)
    test "minVertexCover (star x (1:2:ys))                  == (Set.singleton x, Set.empty)" $ \(x :: Int) (ys :: [Int]) ->
          minVertexCover (star x (1:2:ys) :: BAII)          == (Set.singleton x, Set.empty)
    test "vertexCoverSize (minVertexCover (biclique xs ys)) == min (length (nub xs)) (length (nub ys))" $ size10 $ \(xs :: [Int]) (ys :: [Int]) ->
          vertexCoverSize (minVertexCover (biclique xs ys)) == min (length (nub xs)) (length (nub ys))
    test "vertexCoverSize . minVertexCover                  == matchingSize . maxMatching" $ \(x :: BAII) ->
         (vertexCoverSize . minVertexCover) x               ==(matchingSize . maxMatching) x
    test "isVertexCoverOf (minVertexCover x) x              == True" $ \(x :: BAII) ->
          isVertexCoverOf (minVertexCover x) x              == True

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.isIndependentSetOf ============"
    test "isIndependentSetOf (xs             , ys             ) empty          == Set.null xs && Set.null ys" $ \(xs :: Set Int) (ys :: Set Int) ->
          isIndependentSetOf (xs             , ys             ) empty          ==(Set.null xs && Set.null ys)
    test "isIndependentSetOf (xs             , ys             ) (leftVertex x) == Set.isSubsetOf xs (Set.singleton x) && Set.null ys" $ \(x :: Int) (xs :: Set Int) (ys :: Set Int) ->
          isIndependentSetOf (xs             , ys             ) (leftVertex x) ==(Set.isSubsetOf xs (Set.singleton x) && Set.null ys)
    test "isIndependentSetOf (Set.empty      , Set.empty      ) (edge x y)     == True" $ \(x :: Int) (y :: Int) ->
          isIndependentSetOf (Set.empty      , Set.empty      ) (edge x y)     == True
    test "isIndependentSetOf (Set.singleton x, ys             ) (edge x y)     == Set.null ys" $ \(x :: Int) (y :: Int) (ys :: Set Int) ->
          isIndependentSetOf (Set.singleton x, ys             ) (edge x y)     == Set.null ys
    test "isIndependentSetOf (xs             , Set.singleton y) (edge x y)     == Set.null xs" $ \(x :: Int) (y :: Int) (xs :: Set Int) ->
          isIndependentSetOf (xs             , Set.singleton y) (edge x y)     == Set.null xs

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.maxIndependentSet ============"
    test "maxIndependentSet empty                                 == (Set.empty, Set.empty)" $
          maxIndependentSet (empty :: BAII)                       == (Set.empty, Set.empty)
    test "maxIndependentSet (vertices xs ys)                      == (Set.fromList xs, Set.fromList ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          maxIndependentSet (vertices xs ys)                      == (Set.fromList xs, Set.fromList ys)
    test "maxIndependentSet (path [1,2,3])                        == (Set.fromList [1,3], Set.empty)" $
          maxIndependentSet (path [1,2,3] :: BAII)                == (Set.fromList [1,3], Set.empty)
    test "maxIndependentSet (star x (1:2:ys))                     == (Set.empty, Set.fromList (1:2:ys))" $ \(x :: Int) (ys :: [Int]) ->
          maxIndependentSet (star x (1:2:ys))                     == (Set.empty, Set.fromList (1:2:ys))
    test "independentSetSize (maxIndependentSet (biclique xs ys)) == max (length (nub xs)) (length (nub ys))" $ \(xs :: [Int]) (ys :: [Int]) ->
          independentSetSize (maxIndependentSet (biclique xs ys)) == max (length (nub xs)) (length (nub ys))
    test "independentSetSize (maxIndependentSet x)                == vertexCount x - vertexCoverSize (minVertexCover x)" $ \(x :: BAII) ->
          independentSetSize (maxIndependentSet x)                == vertexCount x - vertexCoverSize (minVertexCover x)
    test "isIndependentSetOf (maxIndependentSet x) x              == True" $ \(x :: BAII) ->
          isIndependentSetOf (maxIndependentSet x) x              == True

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.augmentingPath ============"
    test "augmentingPath (matching [])      empty            == Left (Set.empty, Set.empty)" $
          augmentingPath (matching [])     (empty :: BAII)   == Left (Set.empty, Set.empty)
    test "augmentingPath (matching [])      (edge 1 2)       == Right [1,2]" $
          augmentingPath (matching [])      (edge 1 2)       == Right ([1,2] :: LII)
    test "augmentingPath (matching [(1,2)]) (path [1,2,3])   == Left (Set.empty, Set.singleton 2)" $
          augmentingPath (matching [(1,2)]) (path [1,2,3] :: BAII) == Left (Set.empty, Set.singleton 2)
    test "augmentingPath (matching [(3,2)]) (path [1,2,3,4]) == Right [1,2,3,4]" $
          augmentingPath (matching [(3,2)]) (path [1,2,3,4]) == Right ([1,2,3,4] :: LII)
    test "isLeft (augmentingPath (maxMatching x) x)          == True" $ \(x :: BAII) ->
          isLeft (augmentingPath (maxMatching x) x)          == True

    putStrLn "\n============ Bipartite.AdjacencyMap.Algorithm.consistentMatching ============"
    test "consistentMatching (matching xs)   == True" $ \(xs :: [(Int,Int)]) ->
          consistentMatching (matching xs)   == True
    test "consistentMatching (maxMatching x) == True" $ \(x :: BAII) ->
          consistentMatching (maxMatching x) == True
